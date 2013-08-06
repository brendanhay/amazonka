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

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Tree.NTree.TypeDefs
import qualified Data.Vector              as V
import           Text.XML.HXT.Core
import           Text.XML.HXT.Expat       (withExpat)

data JSValue = Text | Tag String | Attr String
    deriving (Eq, Ord, Show)

convertXML :: String -> String -> IO (Maybe Value)
convertXML name src = do
    elems <- runX $ flip readString src
        [ withValidate no
        , withCheckNamespaces no
        , withParseByMimeType no
        , withExpat yes
        ] >>> startNodes

    return . decodeFirst $ encode <$> map (wrapRoot . treeToJSON) elems
  where
    startNodes = deep (isElem >>> hasName name)

    wrapRoot (Just (a, b)) = object [(packJSValue a, b)]
    wrapRoot Nothing       = Null

    decodeFirst xs = case listToMaybe xs of
        Just x  -> decode x
        Nothing -> Nothing

--
-- Internal
--

treeToJSON :: XmlTree -> Maybe (JSValue, Value)
treeToJSON node
    | (NTree (XText str) _)     <- node = text str
    | (NTree (XTag qName _) cs) <- node = tag qName cs
    | otherwise                        = Nothing
  where
    text "" = Nothing
    text s  = Just (Text, String . T.strip $ T.pack s)

    tag qName cs = Just (Tag (localPart qName), mapToJSValue $ objMap cs)

    objMap cs = arrayValuesToJSON     -- unify into a single map,
        . concatValues                -- grouping into arrays by pair name
        . map (uncurry Map.singleton) -- convert pairs to maps
        . (++) attrValues             -- attributes turned into normal pairs
        . catMaybes                   -- filter out the empty values (unconvertable nodes)
        $ map treeToJSON cs           -- convert xml nodes to Maybe (QName, Value) pairs

    arrayValuesToJSON = Map.mapMaybe f
      where
        f []  = Nothing                        -- will be discarded
        f [x] = Just x                         -- don't store as array, just a single value
        f xs  = Just $ Array . V.fromList $ xs -- arrays with more than one element are kept

    attrValues = map (Attr *** String . T.pack) $
        runLA (getAttrl >>> getName &&& (getChildren >>> getText)) node

    concatValues = Map.unionsWith (++) . (fmap . fmap) (: [])

    mapToJSValue m = case Map.toList m of
        [(Text, val)] -> val
        _ -> Object
            . HashMap.fromList
            . (map . first) packJSValue
            $ Map.toList m

packJSValue :: JSValue -> T.Text
packJSValue Text     = T.pack "value"
packJSValue (Attr x) = T.pack x
packJSValue (Tag x)  = T.pack x
