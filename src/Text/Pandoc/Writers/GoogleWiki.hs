{-
Copyright (C) 2012 Gavin Beatty <gavinbeatty@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.GoogleWiki
   Copyright   : Copyright (C) 2012 Gavin Beatty
   License     : GNU GPL, version 2 or above

   Maintainer  : Gavin Beatty <gavinbeatty@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to GoogleWiki markup.

GoogleWiki:  <http://code.google.com/p/support/wiki/WikiFAQ>
-}
module Text.Pandoc.Writers.GoogleWiki ( writeGoogleWiki ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
--import Text.Pandoc.XML ( escapeStringForXML )
--import Data.List ( intersect, intercalate, mapAccumL )
import Data.List ( intersect, intercalate )
--import Data.Char ( isUpper, isLower, isDigit )
import Control.Monad.State

data WriterState = WriterState {
  stListLevel :: Int            -- How many nested levels. Top level => 0
  , stUseTags :: Bool           -- True if we should use HTML tags because we're in a complex list
  , stEndnotes    :: [String]       -- List of endnotes
  }

-- | Convert Pandoc to GoogleWiki.
writeGoogleWiki :: WriterOptions -> Pandoc -> String
writeGoogleWiki opts document =
  evalState (pandocToGoogleWiki opts document)
            (WriterState { stListLevel = 0, stUseTags = False, stEndnotes = [] })

-- | Somewhat hacky, but acceptable formatting for end notes.
fmtEndNotes :: [String] -> [String]
fmtEndNotes (x:xs) =
  let acc = [("==== End notes ====\n^1^: " ++ x)]
  in go (2 :: Int) acc xs
  where go n acc (a:as) = go (n+1) acc' as
            where acc' = acc ++ ['^':(show n) ++ "^ " ++ a]
        go _ acc []     = acc
fmtEndNotes [] = []

-- | Return GoogleWiki representation of document.
pandocToGoogleWiki :: WriterOptions -> Pandoc -> State WriterState String
pandocToGoogleWiki opts (Pandoc _ blocks) = do
  body <- blockListToGoogleWiki opts blocks
  endNotes <- get >>= return . stEndnotes
  let fullbody = intercalate "\n" (body : fmtEndNotes endNotes)
      context = writerVariables opts ++
                [ ("body", fullbody) ] ++
                [ ("toc", "yes") | writerTableOfContents opts ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return fullbody

-- | Escape special characters for GoogleWiki.
escapeString :: String -> String
escapeString s =
  let (pre,as) = break (=='<') s
  in if not $ null as
        then let (inside,bs) = break (=='>') as
             in if not $ null bs
                   then pre ++ "{{{" ++ inside ++ ">}}}" ++ tail bs
                   else s
        else s

-- | Convert Pandoc block element to GoogleWiki.
blockToGoogleWiki :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String

blockToGoogleWiki _ Null = return ""

blockToGoogleWiki opts (Plain inlines) =
  inlineListToGoogleWiki opts inlines

blockToGoogleWiki opts (Para [Image txt (src,tit)]) = do
  capt <- inlineListToGoogleWiki opts txt
  let attrTitle = if not $ null tit
                     then " title='" ++ tit ++ "'"
                     else if not $ null txt
                             then " title='" ++ capt ++ "'"
                             else ""
      attrAlt = if not $ null txt
                   then " alt='" ++ capt ++ "'"
                   else if not $ null tit
                           then " alt='" ++ tit ++ "'"
                           else ""
      attrs = attrTitle ++ attrAlt
  return $ if null attrs
              then "\n" ++ src ++ "\n"
              else "\n<img src='" ++ src ++ "'" ++ attrs ++ " />\n"

blockToGoogleWiki opts (Para inlines) = do
  useTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToGoogleWiki opts inlines
  return $ if useTags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if 0 == listLevel then "\n" else ""

blockToGoogleWiki _ (RawBlock "mediawiki" str) = return str
blockToGoogleWiki _ (RawBlock "html" str) = return str
blockToGoogleWiki _ (RawBlock _ _) = return ""

blockToGoogleWiki _ HorizontalRule = return "\n-----\n"

blockToGoogleWiki opts (Header level inlines) = do
  contents <- inlineListToGoogleWiki opts inlines
  let eqs = replicate (2 * level) '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

blockToGoogleWiki _ (CodeBlock (_,classes,_) str) = do
  -- XXX what does google code support
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl", "", "thinbasic", "tsql", "vb", "vbnet", "vhdl",
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  let (beg, end) = if null at
                      then ("{{{\n", "\n}}}\n")
                      else ("<code language=\"" ++ head at ++ "\">", "</code>")
  return $ beg ++ str ++ end

blockToGoogleWiki opts (BlockQuote blocks) = do
  contents <- blockListToGoogleWiki opts blocks
  return $ "<blockquote>" ++ contents ++ "</blockquote>"

-- XXX todo tables
blockToGoogleWiki opts (Table capt aligns widths headers rows') = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToGoogleWiki opts capt
                      return $ "<caption>" ++ c ++ "</caption>\n"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then ""
                   else unlines $ map
                         (\w -> "<col width=\"" ++ percent w ++ "\" />") widths
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToGoogleWiki opts alignStrings 0 headers
                 return $ "<thead>\n" ++ hs ++ "\n</thead>\n"
  body' <- zipWithM (tableRowToGoogleWiki opts alignStrings) [1..] rows'
  return $ "<table>\n" ++ captionDoc ++ coltags ++ head' ++
            "<tbody>\n" ++ unlines body' ++ "</tbody>\n</table>\n"

blockToGoogleWiki opts x@(BulletList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToGoogleWiki opts "*") items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
     else do
        modify $ \s -> s { stListLevel = (stListLevel s) + 1 }
        contents <- mapM (listItemToGoogleWiki opts "*") items
        modify $ \s -> s { stListLevel = (stListLevel s) - 1 }
        return $ vcat contents ++ if 0 == listLevel then "\n" else ""

blockToGoogleWiki opts x@(OrderedList attribs items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToGoogleWiki opts "#") items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
     else do
        modify $ \s -> s { stListLevel = (stListLevel s) + 1 }
        contents <- mapM (listItemToGoogleWiki opts "#") items
        modify $ \s -> s { stListLevel = (stListLevel s) - 1 }
        return $ vcat contents ++ if 0 == listLevel then "\n" else ""

blockToGoogleWiki opts (DefinitionList items) = do
  oldUseTags <- get >>= return . stUseTags
  modify $ \s -> s { stUseTags = True }
  contents <- mapM (definitionListItemToGoogleWiki opts) items
  modify $ \s -> s { stUseTags = oldUseTags }
  return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
listAttribsToString :: ListAttributes -> String
listAttribsToString (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ show numstyle
  in  (if startnum /= 1
          then " start=\"" ++ show startnum ++ "\""
          else "") ++
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " ++ numstyle' ++ ";\""
          else "")

-- | Convert bullet or ordered list item (list of blocks) to GoogleWiki.
listItemToGoogleWiki :: WriterOptions -> String -> [Block] -> State WriterState String
listItemToGoogleWiki opts marker items = do
  contents <- blockListToGoogleWiki opts items
  useTags <- get >>= return . stUseTags
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else do
       level <- get >>= return . stListLevel
       return $ (replicate (2 * level + 1) ' ') ++ marker ++ contents

-- | Convert definition list item (label, list of blocks) to GoogleWiki.
definitionListItemToGoogleWiki :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToGoogleWiki opts (label, items) = do
  labelText <- inlineListToGoogleWiki opts label
  contents <- mapM (blockListToGoogleWiki opts) items
  return $ "<dt>" ++ labelText ++ "</dt>\n" ++
      (intercalate "\n" $ map (\d -> "<dd>" ++ d ++ "</dd>") contents)

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (num, sty, _) items  -> all isSimpleListItem items &&
                                            num == 1 && sty `elem` [DefaultStyle, Decimal]
       DefinitionList items             -> all isSimpleListItem $ concatMap snd items
       _                                -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x] =
  case x of
       Plain _           -> True
       Para  _           -> True
       BulletList _      -> isSimpleList x
       OrderedList _ _   -> isSimpleList x
       DefinitionList _  -> isSimpleList x
       _                 -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _      -> isSimpleList y
       OrderedList _ _   -> isSimpleList y
       DefinitionList _  -> isSimpleList y
       _                 -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- Auxiliary functions for tables:

tableRowToGoogleWiki :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToGoogleWiki opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToGoogleWiki opts celltype alignment item)
            alignStrings cols'
  return $ "<tr class=\"" ++ rowclass ++ "\">\n" ++ unlines cols'' ++ "</tr>"

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToGoogleWiki :: WriterOptions
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
tableItemToGoogleWiki opts celltype align' item = do
  let mkcell x = "<" ++ celltype ++ " align=\"" ++ align' ++ "\">" ++
                    x ++ "</" ++ celltype ++ ">"
  contents <- blockListToGoogleWiki opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to GoogleWiki.
blockListToGoogleWiki :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToGoogleWiki opts blocks =
  mapM (blockToGoogleWiki opts) blocks >>= return . vcat

--linkEscapeString :: String -> String
{-
linkEscapeString = id
-}
{-
linkEscapeString = fst . mapAccumL f ""
  where f acc x = if x == '[' || x == ']'
                     then ((acc++) $ '\\' : [x], [])
                     else (acc ++ [x], [])
-}
{-
linkEscapeString = fst . mapAccumL f ""
  where f acc x = case x of
                    '[' -> (acc ++ "&#91;", [])
                    ']' -> (acc ++ "&#93;", [])
                    _   -> (acc ++ [x], [])
-}

bangEscapeString :: String -> String
bangEscapeString s = s
{-
bangEscapeString s@(x:y:z:ts) =
  if isUpper x && (isLower y || isDigit y) && (or $ map isUpper (z:ts))
     then '!' : s
     else s
bangEscapeString s = s
-}

-- | Convert list of Pandoc inline elements to GoogleWiki.
inlineListToGoogleWiki :: WriterOptions -> [Inline] -> State WriterState String
inlineListToGoogleWiki opts lst =
  mapM (inlineToGoogleWiki opts) lst >>= return . concat

-- | Convert Pandoc inline element to GoogleWiki.
inlineToGoogleWiki :: WriterOptions -> Inline -> State WriterState String

inlineToGoogleWiki opts (Emph lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ "_" ++ contents ++ "_"

inlineToGoogleWiki opts (Strong lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ "*" ++ contents ++ "*"

inlineToGoogleWiki opts (Strikeout lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ "~~" ++ contents ++ "~~"

inlineToGoogleWiki opts (Superscript lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ "^" ++ contents ++ "^"

inlineToGoogleWiki opts (Subscript lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ ",," ++ contents ++ ",,"

inlineToGoogleWiki opts (SmallCaps lst) = inlineListToGoogleWiki opts lst

inlineToGoogleWiki opts (Quoted SingleQuote lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToGoogleWiki opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToGoogleWiki opts lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToGoogleWiki opts (Cite _  lst) = inlineListToGoogleWiki opts lst

inlineToGoogleWiki _ (Code _ str) =
  return $ "`" ++ str ++ "`"

inlineToGoogleWiki _ (Str str) =
  -- XXX need to bangEscapeString only in pars, etc.
  -- don't want to do it in links, etc.
  return $ bangEscapeString $ escapeString str

-- XXX not supported
inlineToGoogleWiki _ (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                                 -- note:  str should NOT be escaped

inlineToGoogleWiki _ (RawInline "googlewiki" str) = return str
inlineToGoogleWiki _ (RawInline "html" str) = return str
inlineToGoogleWiki _ (RawInline _ _) = return ""

inlineToGoogleWiki _ (LineBreak) = return "<br />\n"

inlineToGoogleWiki _ Space = return " "

inlineToGoogleWiki opts (Link txt (src, _)) = do
  label <- inlineListToGoogleWiki opts txt
  case txt of
     [Code _ s] | s == src -> return src
     _  -> return $ "[" ++ src ++ " " ++ label ++ "]"

inlineToGoogleWiki opts (Image alt (src, tit)) = do
  alt' <- inlineListToGoogleWiki opts alt
  let attrTitle = if not $ null tit
                     then " title='" ++ tit ++ "'"
                     else ""
      attrAlt = if not $ null alt
                   then " alt='" ++ alt' ++ "'"
                   else ""
      attrs = attrTitle ++ attrAlt
  return $ if null attrs
              then src
              else "<img src='" ++ src ++ "'" ++ attrs ++ " />"

inlineToGoogleWiki opts (Note contents) = do
  contents' <- blockListToGoogleWiki opts contents
  modify $ \s -> s { stEndnotes = (stEndnotes s) ++ [contents'] }
  return ""
  -- note - may not work for notes with multiple blocks
