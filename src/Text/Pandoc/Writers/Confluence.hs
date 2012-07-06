{-# LANGUAGE DoAndIfThenElse #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>
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
   Module      : Text.Pandoc.Writers.Confluence
   Copyright   : Copyright (C) 2006-2010 John MacFarlane, Copyright (C) 2012 Gavin Beatty
   License     : GNU GPL, version 2 or above

   Maintainer  : Gavin Beatty <gavinbeatty@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Confluence markup.

Confluence:  <https://confluence.atlassian.com/display/DOC/Confluence+Wiki+Markup>
-}
module Text.Pandoc.Writers.Confluence ( writeConfluence ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate)
import Data.List ( intersect, intercalate )
import Data.List.Split ( splitOn )
--import Network.URI ( escapeURIString, isUnescapedInURI )
import Control.Monad.State

data WriterState = WriterState {
  stListLevel       :: Int            -- How many nested levels. Top level => 0
  , stHaveFootnotes :: Bool           -- Whether there are footnotes to display
  , stMacros        :: [String]       -- List of macros we're inside
  }

-- | Convert Pandoc to Confluence.
writeConfluence :: WriterOptions -> Pandoc -> String
writeConfluence opts document =
  evalState (pandocToConfluence opts document)
            (WriterState {
                stListLevel = 0
                , stHaveFootnotes = False
                , stMacros = []
                })

-- | Return Confluence representation of document.
pandocToConfluence :: WriterOptions -> Pandoc -> State WriterState String
pandocToConfluence opts (Pandoc _ blocks) = do
  body <- blockListToConfluence opts blocks
  footnotes <- displayFootnotesToConfluence opts -- top level footnotes
  modify $ \s -> s { stHaveFootnotes = False }
  let fullbody = body ++ "\n" ++ footnotes
      context = writerVariables opts ++
                [ ("body", fullbody) ] ++
                [ ("toc", "yes") | writerTableOfContents opts ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return fullbody

-- Returns 'True' if 'xs' has >= 'n' elements.
lengthGeq :: Int -> [a] -> Bool
lengthGeq n []
    | n <= 0 = True
    | otherwise = False
lengthGeq 0 (x:xs) = True
lengthGeq n (x:xs) = lengthGeq (n-1) xs

-- XXX hmm, use a regex here since i want something like:
-- /c[^c]+c/ for most cases
-- | Escape 'infx' if >= 'n' occurrences are found in 's' (escape using '\\').
escapeN :: Int -> String -> String -> String
escapeN 0 _ s = s
escapeN n infx s
    | lengthGeq n split = intercalate (map ('\\':) infx) split
    | otherwise = s
  where split = splitOn infx s

-- | Repeated application of 'escapeN'.
escapeNs :: [(Int,String)] -> String -> String
escapeNs [] s = s
escapeNs ((n,x):nxs) s = escapeNs nxs $ escapeN n x s

-- | Escape special characters for Confluence.
escapeString :: String -> String
escapeString = escapeNs $ map (2:) infxs $ escapeStringUsing [
                     ('{', "\\{")
                   , ('}', "\\}")
                   ]
                 where infxs = ["*","_","??","-","+","^","~","bq.","!""|",]

-- | Escape only '<' and '>' to "&lt;" and "&gt;".
escapeHtml :: String -> String
escapeHtml = escapeStringUsing [
                  ('<', "&lt;")
                , ('>', "&gt;")
                ]

-- | Footnotes require the following free (after filling in survey...) plugin:
-- http://www.adaptavist.com/display/Plugins/Footnotes
displayFootnotesToConfluence :: WriterOptions -> State WriterState String
displayFootnotesToConfluence _ = do
  footnotes <- get >>= return . stHaveFootnotes
  return $ if footnotes then "{display-footnotes}" else ""

addHtmlMacro :: WriterOptions -> State

-- | Convert Pandoc block element to Confluence.
blockToConfluence :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState String
blockToConfluence _ Null = return ""

blockToConfluence opts (Plain inlines) =
  inlineListToConfluence opts inlines

blockToConfluence opts (Para inlines) = do
  listLevel <- get >>= return . stListLevel
  contents <- inlineListToConfluence opts inlines
  return $ contents ++ if 0 == listLevel then "\n" else ""

blockToConfluence _ (RawBlock "confluence" str) = return str
-- Try it using the `{html}` macro, even though it's non-standard.
blockToConfluence _ (RawBlock "html" str) = return "{html}" ++ str ++ "{html}"
blockToConfluence _ (RawBlock _ _) = return ""

blockToConfluence _ HorizontalRule = return "----\n"

blockToConfluence opts (Header level inlines) = do
  -- Before a new header, flush the footnotes.
  displayFootnotes <- displayFootnotesToConfluence opts
  modify $ \s -> s { stHaveFootnotes = False }
  contents <- inlineListToConfluence opts inlines
  let prefix = if null displayFootnotes
                  then ""
                  else displayFootnotes ++ "\n"
  return $ prefix ++ "h" ++ (show level) ++ ". " ++ contents ++ "\n"

blockToConfluence _ (CodeBlock (_,classes,_) str) = do
  -- XXX what does confluence code support
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl", "", "thinbasic", "tsql", "vb", "vbnet", "vhdl",
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  let fmt = if null at then "" else ':':(head at)
  return $ "{code" ++ fmt ++ "}\n" ++ str ++ "\n{code}\n"

blockToConfluence opts (BlockQuote blocks) = do
  macros <- get >>= return . stMacros
  modify $ \s -> s { stMacros = macros ++ ["quote"] }
  contents <- blockListToConfluence opts blocks
  modify $ \s -> s { stInQuote = init $ stMacros s }
  return $ "{quote}\n" ++ init contents ++ "{quote}\n"

blockToConfluence opts t@(Table _ _ _ _ _) = do
  macros <- get >>= return . stMacros
  if not $ isSimpleTable t then do
      addHtmlMacro
      if "html" `elem` macros
         then do
           table <- tableToConfluenceUsingTags opts t
           return table
         else do
           modify $ \s -> s { stMacros = macros ++ ["html"] }
           table <- tableToConfluenceUsingTags opts t
           modify $ \s -> s { stUseTags = useTags }
           return table
      else 
      return table
  else do return $ tableToConfluenceUsingBars opts $ mkSimpleTable t

blockToConfluence opts x@(BulletList items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToConfluence opts '*') items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ul>\n" ++ unlines contents ++ "</ul>\n"
     else do
        modify $ \s -> s { stListLevel = listLevel + 1 }
        contents <- mapM (listItemToConfluence opts '*') items
        modify $ \s -> s { stListLevel = listLevel - 1 }
        return $ unlines contents ++ if 0 == listLevel then "\n" else ""

blockToConfluence opts x@(OrderedList attribs items) = do
  oldUseTags <- get >>= return . stUseTags
  listLevel <- get >>= return . stListLevel
  let useTags = oldUseTags || not (isSimpleList x)
  if useTags
     then do
        modify $ \s -> s { stUseTags = True }
        contents <- mapM (listItemToConfluence opts '#') items
        modify $ \s -> s { stUseTags = oldUseTags }
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ unlines contents ++ "</ol>\n"
     else do
        modify $ \s -> s { stListLevel = listLevel + 1 }
        contents <- mapM (listItemToConfluence opts '#') items
        modify $ \s -> s { stListLevel = listLevel - 1 }
        return $ unlines contents ++ if 0 == listLevel then "\n" else ""

blockToConfluence opts (DefinitionList items) = do
  oldUseTags <- get >>= return . stUseTags
  modify $ \s -> s { stUseTags = True }
  contents <- mapM (definitionListItemToConfluence opts) items
  modify $ \s -> s { stUseTags = oldUseTags }
  return $ "<dl>\n" ++ unlines contents ++ "</dl>\n"

isSimpleTable :: Block -> Bool
isSimpleTable (Table capt aligns widths _ _) =
  noCaption && allZeroWith && allSimpleAlign
  where allZeroWith    = all (== 0.0) widths
        allSimpleAlign = all (\a -> a == AlignLeft || a == AlignDefault) aligns
        noCaption      = null capt
isSimpleTable _ = False

mkSimpleTable :: Block -> Block
mkSimpleTable (Table capt _ _ headers rows) = Table capt [] [] headers rows
mkSimpleTable x = x

tableToConfluenceUsingTags :: WriterOptions -> Block -> State WriterState String
tableToConfluenceUsingTags opts (Table capt aligns widths headers rows) = do
  let alignStrings = map alignmentToString aligns
  captionDoc <- if null capt
                   then return ""
                   else do
                      c <- inlineListToConfluence opts capt
                      return $ "<caption>" ++ c ++ "</caption>\n"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let coltags = if all (== 0.0) widths
                   then ""
                   else unlines $ map
                         (\w -> "<col width=\"" ++ percent w ++ "\" />") widths
  head' <- if all null headers
              then return ""
              else do
                 hs <- tableRowToConfluenceUsingTags opts alignStrings 0 headers
                 return $ "<thead>\n" ++ hs ++ "\n</thead>\n"
  body' <- zipWithM (tableRowToConfluenceUsingTags opts alignStrings) [1..] rows
  return $ "<table>\n" ++ captionDoc ++ coltags ++ head' ++
            "<tbody>\n" ++ unlines body' ++ "</tbody>\n</table>\n"

tableToConfluenceUsingTags _ _ = undefined

tableToConfluenceUsingBars :: WriterOptions -> Block -> State WriterState String
tableToConfluenceUsingBars opts (Table [] [] [] headers rows) = do
  hdrs <- tableRowToConfluenceUsingBars opts "||" headers
  body <- mapM (tableRowToConfluenceUsingBars opts "|") rows
  return $ unlines $ (hdrs : body)

tableToConfluenceUsingBars _ _ = undefined

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

-- | Convert bullet or ordered list item (list of blocks) to Confluence.
listItemToConfluence :: WriterOptions -> Char -> [Block] -> State WriterState String
listItemToConfluence opts marker items = do
  contents <- blockListToConfluence opts items
  useTags <- get >>= return . stUseTags
  level <- get >>= return . stListLevel
  if useTags
     then return $ "<li>" ++ contents ++ "</li>"
     else return $ (replicate level marker) ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to Confluence.
definitionListItemToConfluence :: WriterOptions
                             -> ([Inline],[[Block]])
                             -> State WriterState String
definitionListItemToConfluence opts (label, items) = do
  labelText <- inlineListToConfluence opts label
  contents <- mapM (blockListToConfluence opts) items
  return $ "<dt>" ++ labelText ++ "</dt>\n" ++
      (unlines $ map (\d -> "<dd>" ++ d ++ "</dd>") contents)

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

-- Auxiliary functions for tables:

tableRowToConfluenceUsingTags :: WriterOptions
                    -> [String]
                    -> Int
                    -> [[Block]]
                    -> State WriterState String
tableRowToConfluenceUsingTags opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
                      0                  -> "header"
                      x | x `rem` 2 == 1 -> "odd"
                      _                  -> "even"
  cols'' <- sequence $ zipWith
            (\alignment item -> tableItemToConfluence opts celltype alignment item)
            alignStrings cols'
  return $ "<tr class=\"" ++ rowclass ++ "\">\n" ++ unlines cols'' ++ "</tr>"

tableRowToConfluenceUsingBars :: WriterOptions
                    -> String
                    -> [[Block]]
                    -> State WriterState String
tableRowToConfluenceUsingBars opts bars cols = do
  contents <- mapM (blockListToConfluence opts) cols
  return $ bars ++ (intercalate bars contents) ++ bars

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableItemToConfluence :: WriterOptions
                     -> String
                     -> String
                     -> [Block]
                     -> State WriterState String
tableItemToConfluence opts celltype align' item = do
  let mkcell x = "<" ++ celltype ++ " align=\"" ++ align' ++ "\">" ++
                    x ++ "</" ++ celltype ++ ">"
  contents <- blockListToConfluence opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to Confluence.
blockListToConfluence :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState String
blockListToConfluence opts blocks =
  mapM (blockToConfluence opts) blocks >>= return . unlines

-- | Convert list of Pandoc inline elements to Confluence.
inlineListToConfluence :: WriterOptions -> [Inline] -> State WriterState String
inlineListToConfluence opts lst =
  mapM (inlineToConfluence opts) lst >>= return . concat

-- | Convert Pandoc inline element to Confluence.
inlineToConfluence :: WriterOptions -> Inline -> State WriterState String

inlineToConfluence opts (Emph lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "_" ++ contents ++ "_"

inlineToConfluence opts (Strong lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "*" ++ contents ++ "*"

inlineToConfluence opts (Strikeout lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "-" ++ contents ++ "-"

inlineToConfluence opts (Superscript lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "^" ++ contents ++ "^"

inlineToConfluence opts (Subscript lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "~" ++ contents ++ "~"

inlineToConfluence opts (SmallCaps lst) = inlineListToConfluence opts lst

inlineToConfluence opts (Quoted SingleQuote lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "\8216" ++ contents ++ "\8217"

inlineToConfluence opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToConfluence opts lst
  return $ "\8220" ++ contents ++ "\8221"

inlineToConfluence opts (Cite _  lst) = inlineListToConfluence opts lst

inlineToConfluence _ (Code _ str) =
  return $ "{code}" ++ str ++ "{code}"

inlineToConfluence _ (Str str) = do
  macros <- get >>= return . stMacros
  return $ if "html" `elem` macros
              then escapeString str
              else $ escapeHtml $ escapeString str

-- | Requires the LaTeX plugin:
-- https://studio.plugins.atlassian.com/wiki/display/CLATEX/LaTeX+Plugin
inlineToConfluence _ (Math _ str) =
  return $ "{latex}\\( " ++ str ++ " \\){latex}"

inlineToConfluence _ (RawInline "confluence" str) = return str
inlineToConfluence _ (RawInline "html" str) = return str
inlineToConfluence _ (RawInline _ _) = return ""

-- | Trailing space so consecutive line breaks can work.
inlineToConfluence _ (LineBreak) = return "\\\\ "

inlineToConfluence _ Space = return " "

inlineToConfluence opts (Link txt (src, _)) = do
  label <- inlineListToConfluence opts txt
  case txt of
     [Code _ s] | s == src -> return src
     _  -> return $ "[" ++ label ++ "|" ++ src ++ "]"

inlineToConfluence opts (Image alt (src, tit)) = do
  alt' <- inlineListToConfluence opts alt
  let attrTitle = if not $ null tit
                     then "title=\"" ++ dqAmpEscapeString tit ++ "\""
                     else ""
      attrAlt = if not $ null alt
                   then "alt=\"" ++ dqAmpEscapeString alt' ++ "\""
                   else ""
      attrs = filter (not . null) [attrTitle, attrAlt]
  return $ "!" ++ src ++ linkAttrJoin attrs ++ "!"

-- | Footnotes require the following free (after filling in survey...) plugin:
-- http://www.adaptavist.com/display/Plugins/Footnotes
inlineToConfluence opts (Note contents) = do
  contents' <- blockListToConfluence opts contents
  modify $ \s -> s { stHaveFootnotes = True }
  return $ "{footnote}" ++ contents' ++ "{footnote}"

twoSepJoin :: String -> String -> [String] -> String
twoSepJoin c1 c2 (x:xs) = c1 ++ x ++ concat (map (c2 ++) xs)
twoSepJoin _ _ _ = ""

-- | Join a list of link attributes. First use '|' then ','
linkAttrJoin :: [String] -> String
linkAttrJoin = twoSepJoin "|" ", "

-- | Join a list of macro attributes. First use ':' then '|'
--macroAttrJoin :: [String] -> String
--macroAttrJoin = twoSepJoin ":" "|"

-- | Escape a double quote string using &quot;
dqAmpEscapeString :: String -> String
dqAmpEscapeString = escapeStringUsing [('"', "&quot;")]

