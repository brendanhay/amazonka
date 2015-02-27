{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Data.Internal.XML
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.Internal.XML
    (
    -- * FromXML
      FromXML   (..)
    , decodeXML
    , parseXMLText
    , childNodes
    , findElement
    , withContent
    , withElement
    , localName
    , (.@)
    , (.@?)
    , (.!@)

    -- * ToXML
    , ToXML     (..)
    , ToXMLRoot (..)
    , encodeXML
    , toXMLText
    , namespaced
    , element
    , nodes
    , (=@)
    , extractRoot
    , unsafeToXML
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid
import           Data.Text                            (Text)
import           Network.AWS.Data.Internal.ByteString
import           Network.AWS.Data.Internal.Text
import           Numeric.Natural
import           Text.XML

decodeXML :: LazyByteString -> Either String [Node]
decodeXML = either failure success . parseLBS def
  where
    failure = Left  . show
    success = Right . elementNodes . documentRoot

encodeXML :: ToXMLRoot a => a -> LazyByteString
encodeXML = maybe mempty (renderLBS def . d) . toXMLRoot
  where
    d x = Document
        { documentPrologue = p
        , documentRoot     = x
        , documentEpilogue = []
        }

    p = Prologue
        { prologueBefore  = []
        , prologueDoctype = Nothing
        , prologueAfter   = []
        }

parseXMLText :: FromText a => String -> [Node] -> Either String a
parseXMLText n = withContent n >=> maybe err fromText
  where
    err = Left $ "empty node list, when expecting single node " ++ n

toXMLText :: ToText a => a -> [Node]
toXMLText x = [NodeContent (toText x)]

(.@) :: FromXML a => [Node] -> Text -> Either String a
ns .@ n = findElement n ns >>= parseXML

(.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
ns .@? n =
    case findElement n ns of
        Left _   -> Right Nothing
        Right xs -> parseXML xs

(.!@) :: Either String (Maybe a) -> a -> Either String a
f .!@ x = fromMaybe x <$> f

namespaced :: Text -> Text -> [Node] -> Maybe Element
namespaced g l = Just . element (Name l (Just g) Nothing)

element :: Name -> [Node] -> Element
element n = Element n mempty

nodes :: Name -> [Node] -> [Node]
nodes n ns = [NodeElement (element n ns)]

(=@) :: ToXML a => Name -> a -> Node
n =@ x = NodeElement (element n (toXML x))

extractRoot :: Text -> [Node] -> Maybe Element
extractRoot g ns =
    case ns of
        [NodeElement x] -> Just x { elementName = rename x }
        _               -> Nothing
  where
    rename x = (elementName x) { nameNamespace = Just g }

-- | /Caution:/ This is for use with types which are 'flattened' in
-- AWS service model terminology.
--
-- It is applied by the generator/templating in safe contexts only.
unsafeToXML :: (Show a, ToXML a) => a -> Node
unsafeToXML x =
    fromMaybe (error $ "Failed to unflatten node-list for: " ++ show x)
              (listToMaybe (toXML x))

withContent :: String -> [Node] -> Either String (Maybe Text)
withContent n = exactly >=> \case
    Just  x -> Just <$> content x
    Nothing -> return Nothing
  where
    exactly :: [Node] -> Either String (Maybe Node)
    exactly = \case
        [x] -> Right (Just x)
        []  -> Right Nothing
        _   -> Left $
            "encountered node list, when expecting exactly one node: " ++ n

    content :: Node -> Either String Text
    content (NodeContent x)
        = Right x
    content (NodeElement e)
        = let k = show (elementName e)
           in Left $ "unexpected element " ++ k ++ ", when expecting node content: " ++ n
    content _ = Left $ "unrecognised element, when expecting node content: " ++ n

withElement :: Text -> ([Node] -> Either String a) -> [Node] -> Either String a
withElement n f = findElement n >=> f

findElement :: Text -> [Node] -> Either String [Node]
findElement n ns = maybe err Right . listToMaybe $ mapMaybe (childNodes n) ns
  where
    err = Left $ "unable to find element "
        ++ show n
        ++ " in nodes "
        ++ show (mapMaybe localName ns)

childNodes :: Text -> Node -> Maybe [Node]
childNodes n (NodeElement e)
    | nameLocalName (elementName e) == n = Just (elementNodes e)
childNodes _ _ = Nothing

localName :: Node -> Maybe Text
localName (NodeElement e) = Just (nameLocalName (elementName e))
localName _               = Nothing

class FromXML a where
    parseXML :: [Node] -> Either String a

instance FromXML a => FromXML (Maybe a) where
    parseXML [] = pure Nothing
    parseXML ns = Just <$> parseXML ns

instance FromXML Text where
    parseXML = withContent "Text" >=> fromText . fromMaybe mempty

instance FromXML Int     where parseXML = parseXMLText "Int"
instance FromXML Integer where parseXML = parseXMLText "Integer"
instance FromXML Natural where parseXML = parseXMLText "Natural"
instance FromXML Double  where parseXML = parseXMLText "Double"
instance FromXML Bool    where parseXML = parseXMLText "Bool"

class ToXMLRoot a where
    toXMLRoot :: a -> Maybe Element

class ToXML a where
    toXML :: a -> [Node]

    default toXML :: ToXMLRoot a => a -> [Node]
    toXML = maybeToList . fmap NodeElement . toXMLRoot

instance ToXML a => ToXML (Maybe a) where
    toXML (Just x) = toXML x
    toXML Nothing  = []

instance ToXML Text    where toXML = toXMLText
instance ToXML Int     where toXML = toXMLText
instance ToXML Integer where toXML = toXMLText
instance ToXML Natural where toXML = toXMLText
instance ToXML Double  where toXML = toXMLText
instance ToXML Bool    where toXML = toXMLText
