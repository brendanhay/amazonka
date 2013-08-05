{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Aeson.XML
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               Berkeley Software Distribution License, v. 3.0.
--               You can obtain it at
--               http://http://opensource.org/licenses/BSD-3-Clause.
-- Author      : Vladimir Kirillov <proger@hackndev.com>
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Aeson.XML where

-- import           Control.Applicative        ((<*), (*>))
-- import           Control.Arrow              (first, (>>>), (&&&), (***))
-- import           Control.Category           (id)
-- import           Control.Monad              (when, forM_)
-- import qualified Data.Aeson                 as Aeson
-- import qualified Data.ByteString.Lazy.Char8 as BS
-- import qualified Data.HashMap.Strict        as HashMap
-- import qualified Data.Map                   as M
-- import           Data.Maybe                 (catMaybes)
-- import qualified Data.Text                  as T
-- import           Data.Tree.NTree.TypeDefs
-- import qualified Data.Vector                as Vector
-- import           Prelude                    hiding (id)
-- import           System.Console.GetOpt      (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), usageInfo, getOpt)
-- import           System.Environment         (getArgs)
-- import           System.Exit                (ExitCode(ExitFailure), exitWith)
-- import           System.IO                  (hPutStrLn, stderr)
-- import           Text.Regex.Posix           ((=~))
-- import           Text.XML.HXT.Core          (readDocument, getChildren, getText, isElem, XmlTree, XNode(..), deep, getName, localPart, hasName, ArrowXml, runLA, getAttrl, runX, withValidate, no)
-- import           Text.XML.HXT.Curl                                                                                                                                                                 -- use libcurl for HTTP access, only necessary when reading http://...
-- import           Text.XML.HXT.Expat         (withExpat)


-- getStartNodes :: ArrowXml cat => [Flag] -> cat (NTree XNode) XmlTree
-- getStartNodes flags =
--   case [x | StartFrom x <- flags] of
--   []  -> getChildren >>> isElem
--   [x] -> deep (isElem >>> hasName x)
--   _   -> error "Expecting at most one --tag-name (-t) option"

-- main :: IO ()
-- main = do
--   args <- getArgs
--   (flags, inputFiles) <- parseOptions args

--   when (elem ShowHelp flags || (null flags && null inputFiles)) .
--     die $ usageInfo usageHeader options

--   let
--     skipRoots     = SkipRoots      `elem` flags
--     wrapArray     = WrapArray      `elem` flags
--     collapseTextRegex = singleOrNothing "Expecting at most one --no-collapse-text option" [x | NoCollapseText x <- flags]
--     wrapAction act
--       | wrapArray = putStr "[" *> act <* putStr "]"
--       | otherwise = act
--     multiline = case (wrapArray, Multiline `elem` flags) of
--       (False, _)     -> BS.intercalate (BS.pack "\n")
--       (True,  False) -> BS.intercalate (BS.pack ",")
--       (True,  True)  -> BS.intercalate (BS.pack ",\n")

--     ignoreNulls
--       | NoIgnoreNulls `notElem` flags =
--         filter (/= Aeson.Null)
--       | otherwise =  id

--     nodesFilter
--       | skipRoots = getChildren
--       | otherwise = id

--   forM_ inputFiles $ \src -> do
--     rootElems <-
--       runX $
--       readDocument
--       [ withValidate no
--       , withExpat True
--       , withCurl []
--       ]
--       src
--       >>> getStartNodes flags
--       >>> nodesFilter
--     -- TODO: de-uglify and optimize the following
--     wrapAction
--       . BS.putStr . multiline
--       . map Aeson.encode
--       . ignoreNulls
--       . map (wrapRoot . xmlTreeToJSON collapseTextRegex)
--       $ rootElems

-- data JSValueName = Text | Tag String | Attr String
--   deriving (Eq, Ord, Show)

-- concatMapValues :: (Ord k) => [M.Map k v] -> M.Map k [v]
-- concatMapValues = M.unionsWith (++) . (fmap . fmap) (: [])

-- getAttrVals :: XmlTree -> [(String, String)]
-- getAttrVals = runLA (getAttrl >>> getName &&& (getChildren >>> getText))

-- arrayValuesToJSONArrays :: (Ord k) => M.Map k [Aeson.Value] -> M.Map k Aeson.Value
-- arrayValuesToJSONArrays = M.mapMaybe f
--   where
--     f [] = Nothing -- will be discarded
--     f [x] = Just x  -- don't store as array, just a single value
--     f xss = Just $ Aeson.Array . Vector.fromList $ xss -- arrays with more than one element are kept

-- packJSValueName :: JSValueName -> T.Text
-- packJSValueName Text = T.pack "value"
-- packJSValueName (Attr x) = T.pack x
-- packJSValueName (Tag x)  = T.pack x

-- wrapRoot :: Maybe (JSValueName, Aeson.Value) -> Aeson.Value
-- wrapRoot Nothing       = Aeson.Null
-- wrapRoot (Just (a, b)) = Aeson.object [(packJSValueName a, b)]

-- -- converts a map to a json value, usually resulting in a json object unless the map contains ONLY a single Text entry,
-- -- in which case the value produced is a json string
-- tagMapToJSValue :: Bool -> M.Map JSValueName Aeson.Value -> Aeson.Value
-- tagMapToJSValue collapseTextRegex m = case (collapseTextRegex, M.toList m) of
--   (True, [(Text, val)]) -> val
--   _                     ->
--     Aeson.Object . HashMap.fromList . (map . first) packJSValueName $ M.toList m

-- xmlTreeToJSON :: Maybe String -> XmlTree -> Maybe (JSValueName, Aeson.Value)
-- xmlTreeToJSON collapseTextRegex node@(NTree (XTag qName _) children)
--   = Just (Tag (localPart qName),
--           tagMapToJSValue shouldCollapseText objMap)
--   where
--     objMap =
--         arrayValuesToJSONArrays    -- unify into a single map,
--       . concatMapValues            -- grouping into arrays by pair name
--       . map (uncurry M.singleton)  -- convert pairs to maps
--       . (++) attrVals
--       . catMaybes                  -- filter out the empty values (unconvertable nodes)
--       $ map (xmlTreeToJSON collapseTextRegex) children -- convert xml nodes to Maybe (QName, Aeson.Value) pairs

--     attrVals =
--       map (Attr *** Aeson.String . T.pack) $ getAttrVals node

--     shouldCollapseText = case collapseTextRegex of
--                          Nothing -> True
--                          Just "" -> False
--                          Just pattern -> not $ (localPart qName) =~ pattern

-- xmlTreeToJSON _ (NTree (XText str) _)
--   | T.null text = Nothing
--   | otherwise = Just (Text, Aeson.String text)
--   where
--     text = T.strip $ T.pack str

-- xmlTreeToJSON _ _ = Nothing

-- die :: String -> IO a
-- die msg = do
--   hPutStrLn stderr msg
--   exitWith (ExitFailure 1)

-- singleOrNothing :: String -> [a] -> Maybe a
-- singleOrNothing _   []  = Nothing
-- singleOrNothing _   [x] = Just x
-- singleOrNothing msg _   = error msg
