{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.XML
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Data.Aeson.XML
    ( FromXML(..)
    , stringToXML
    ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.Text                as Text
import           Data.Tree.NTree.TypeDefs
import qualified Data.Vector              as V
import           Text.XML.HXT.Core
import           Text.XML.HXT.Expat       (withExpat)

-- FIXME: Whole model is inefficient; revisit

data JSValue = Text | Tag String | Attr String
    deriving (Eq, Ord, Show)

class FromJSON a => FromXML a where
    fromXML :: String -> IO (Maybe a)
    fromXML = stringToXML 1

stringToXML :: FromJSON a => Int -> String -> IO (Maybe a)
stringToXML level src = do
    elems <- runX . foldl1 (>>>) $ readString options src : replicate level getChildren
    return . decodeFirst $ encode <$> map (unWrap . treeToJSON) elems
  where
    options =
        [ withValidate no
        , withCheckNamespaces no
        , withParseByMimeType no
        , withExpat yes
        ]

    unWrap (Just (_, x)) = x
    unWrap Nothing       = Null

    decodeFirst (x:_) = decode x
    decodeFirst _     = Nothing

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
    text s  = Just (Text, String . Text.strip $ Text.pack s)

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

    attrValues = map (Attr *** String . Text.pack) $
        runLA (getAttrl >>> getName &&& (getChildren >>> getText)) node

    concatValues = Map.unionsWith (++) . (fmap . fmap) (: [])

    mapToJSValue m
        | Map.null m = Array V.empty -- convert empty elements to empty arrays
        | otherwise  =
            case Map.toList m of
                [(Text, val)] -> val
                _             -> Object . HashMap.fromList
                                       . (map . first) packJSValue
                                       $ Map.toList m

    packJSValue Text     = Text.pack "value"
    packJSValue (Attr x) = Text.pack x
    packJSValue (Tag x)  = Text.pack x
