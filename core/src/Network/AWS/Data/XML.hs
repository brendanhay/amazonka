{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Data.XML
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Data.XML where
    -- (
    -- -- * FromXML
    --   FromXML      (..)
    -- , decodeXML
    -- , parseXMLText
    -- -- ** Combinators
    -- , withContent
    -- , withElement
    -- , findElement
    -- -- ** Operators
    -- , (.@)
    -- , (.@?)
    -- , (.!@)
    -- , (.@@)

    -- -- * ToXML
    -- , ToElement (..)
    -- , ToXML        (..)
    -- , encodeXML
    -- , toXMLText
    -- -- ** Constructors
    -- , namespaced
    -- , element
    -- , nodes
    -- -- ** Operators
    -- , (@=)
    -- ) where

import           Control.Applicative
import           Control.Monad
import           Data.Default.Class
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Text                   (Text)
import           GHC.Exts
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Numeric.Natural
import           Text.XML

infixl 7 .@, .@?, .!@

(.@) :: FromXML a => [Node] -> Text -> Either String a
ns .@ n = findElement n ns >>= parseXML

(.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
ns .@? n =
    case findElement n ns of
        Left _   -> Right Nothing
        Right xs -> parseXML xs

-- FIXME: .!@ and may can be moved somewhere else.

(.!@) :: Functor f => f (Maybe a) -> a -> f a
f .!@ x = fromMaybe x <$> f

may :: Applicative f => ([a] -> f b) -> [a] -> f (Maybe b)
may f [] = pure Nothing
may f xs = Just <$> f xs

infixr 7 @=

-- Both this @= and toXMLList take 'Text' rather than 'Name' since the
-- IsString instance for Name creates a qualified name with an empty ns.

(@=) :: ToXML a => Text -> a -> XML
n @= x = XOne . NodeElement $ mkElement (Name n Nothing Nothing) x

decodeXML :: FromXML a => LazyByteString -> Either String a
decodeXML = either failure success . parseLBS def
  where
    failure = Left  . show
    success = parseXML . elementNodes . documentRoot

encodeXML :: ToElement a => a -> LazyByteString
encodeXML = renderLBS def . toDocument

toDocument :: ToElement a => a -> Document
toDocument x = Document
    { documentRoot     = toElement x
    , documentEpilogue = []
    , documentPrologue =
        Prologue
            { prologueBefore  = []
            , prologueDoctype = Nothing
            , prologueAfter   = []
            }
    }

class FromXML a where
    parseXML :: [Node] -> Either String a

instance FromXML [Node] where
    parseXML = pure

instance FromXML a => FromXML (Maybe a) where
    parseXML [] = pure Nothing
    parseXML ns = Just <$> parseXML ns

instance FromXML Text where
    parseXML = fmap (fromMaybe mempty) . withContent "Text"

instance FromXML Char       where parseXML = parseXMLText "Char"
instance FromXML ByteString where parseXML = parseXMLText "ByteString"
instance FromXML Int        where parseXML = parseXMLText "Int"
instance FromXML Integer    where parseXML = parseXMLText "Integer"
instance FromXML Natural    where parseXML = parseXMLText "Natural"
instance FromXML Double     where parseXML = parseXMLText "Double"
instance FromXML Bool       where parseXML = parseXMLText "Bool"

class ToElement a where
    toElement :: a -> Element

instance ToElement Element where
    toElement = id

-- | Provides a way to make the operators for ToXML instance
-- declaration be consistent WRT to single nodes or lists of nodes.
data XML
    = XNull
    | XOne  Node
    | XMany [Node]
      deriving (Show)

instance Monoid XML where
    mempty              = XNull
    mappend XNull XNull = XNull
    mappend a     b     = XMany (listXMLNodes a <> listXMLNodes b)

listXMLNodes :: XML -> [Node]
listXMLNodes = \case
    XNull    -> []
    XOne  n  -> [n]
    XMany ns -> ns

class ToXML a where
    toXML :: a -> XML

toXMLNodes :: ToXML a => a -> [Node]
toXMLNodes = listXMLNodes . toXML

instance ToXML XML where
    toXML = id

instance ToXML a => ToXML (Maybe a) where
    toXML (Just x) = toXML x
    toXML Nothing  = XNull

instance ToXML Text       where toXML = toXMLText
instance ToXML ByteString where toXML = toXMLText
instance ToXML Int        where toXML = toXMLText
instance ToXML Integer    where toXML = toXMLText
instance ToXML Natural    where toXML = toXMLText
instance ToXML Double     where toXML = toXMLText
instance ToXML Bool       where toXML = toXMLText

parseXMLList :: FromXML a
             => Text
             -> [Node]
             -> Either String [a]
parseXMLList n = traverse parseXML . mapMaybe (childNodesOf n)

parseXMLText :: FromText a => String -> [Node] -> Either String a
parseXMLText n = withContent n >=>
    maybe (Left $ "empty node list, when expecting single node " ++ n)
        fromText

toXMLList :: (IsList a, ToXML (Item a)) => Text -> a -> XML
toXMLList n = XMany . map (NodeElement . mkElement e) . toList
  where
    e = Name n Nothing Nothing

toXMLText :: ToText a => a -> XML
toXMLText = XOne . NodeContent . toText

mkElement :: ToXML a => Name -> a -> Element
mkElement n = Element n mempty . listXMLNodes . toXML

withContent :: String -> [Node] -> Either String (Maybe Text)
withContent k = \case
    []              -> Right Nothing
    [NodeContent x] -> Right (Just x)
    _               -> Left $ "encountered many nodes, when expecting text: " ++ k

withElement :: Text -> ([Node] -> Either String a) -> [Node] -> Either String a
withElement n f = findElement n >=> f

findElement :: Text -> [Node] -> Either String [Node]
findElement n ns =
    maybe (Left missing) Right . listToMaybe $ mapMaybe (childNodesOf n) ns
  where
    missing = "unable to find element "
        ++ show n
        ++ " in nodes "
        ++ show (mapMaybe localName ns)

childNodesOf :: Text -> Node -> Maybe [Node]
childNodesOf n x = case x of
    NodeElement e
        | Just n' <- localName x
        , n == n' -> Just (elementNodes e)
    _             -> Nothing

localName :: Node -> Maybe Text
localName = \case
    NodeElement e -> Just (nameLocalName (elementName e))
    _             -> Nothing
