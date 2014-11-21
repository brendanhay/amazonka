{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Data.Internal.XML
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    , withNode
    , (.@)
    , (.@?)

    -- * ToXML
    , ToXML     (..)
    , ToXMLRoot (..)
    , encodeXML
    , toXMLText
    , namespaced
    , element
    , nodes
    , (=@)
    , unsafeToXML
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Default.Class
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as Map
import           Data.Hashable
import           Data.List                            (find)
import           Data.List.NonEmpty                   (NonEmpty(..))
import qualified Data.List.NonEmpty                   as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Tagged
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import           Data.Traversable                     (traverse)
import           GHC.Generics
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
encodeXML x = renderLBS def d
  where
    d = Document
        { documentPrologue = p
        , documentRoot     = toXMLRoot x
        , documentEpilogue = []
        }

    p = Prologue
        { prologueBefore  = []
        , prologueDoctype = Nothing
        , prologueAfter   = []
        }

parseXMLText :: FromText a => String -> [Node] -> Either String a
parseXMLText n = withContent n fromText
{-# INLINE parseXMLText #-}

toXMLText :: ToText a => a -> [Node]
toXMLText x = [NodeContent (toText x)]
{-# INLINE toXMLText #-}

(.@) :: FromXML a => [Node] -> Text -> Either String a
ns .@ n = findElement n ns >>= parseXML
{-# INLINE (.@) #-}

(.@?) :: FromXML a => [Node] -> Text -> Either String (Maybe a)
ns .@? n =
    case findElement n ns of
        Left _   -> Right Nothing
        Right xs -> parseXML xs
{-# INLINE (.@?) #-}

namespaced :: Text -> Text -> [Node] -> Element
namespaced g l = element (Name l (Just g) Nothing)
{-# INLINE namespaced #-}

element :: Name -> [Node] -> Element
element n = Element n mempty
{-# INLINE element #-}

nodes :: Name -> [Node] -> [Node]
nodes n ns = [NodeElement (element n ns)]
{-# INLINE nodes #-}

(=@) :: ToXML a => Name -> a -> Node
n =@ x = NodeElement (element n (toXML x))
{-# INLINE (=@) #-}

-- | /Caution:/ This is for use with types which are 'flattened' in
-- AWS service model terminology. It is applied by the generator/templating
-- in safe contexts only.
unsafeToXML :: (Show a, ToXML a) => a -> Node
unsafeToXML x =
    fromMaybe (error $ "Failed to unflatten node-list for: " ++ show x)
              (listToMaybe (toXML x))
{-# INLINE unsafeToXML #-}

withContent :: String -> (Text -> Either String a) -> [Node] -> Either String a
withContent n f = withNode n (join . fmap f . g)
  where
    g (NodeContent x)
        = Right x
    g (NodeElement e)
        = Left $ "unexpected element " ++ show (elementName e) ++ " when expecting node content: " ++ n
    g _ = Left $ "unexpected element, when expecting node content: " ++ n
{-# INLINE withContent #-}

withNode :: String -> (Node -> Either String a) -> [Node] -> Either String a
withNode n f = \case
    [x] -> f x
    []  -> Left $ "empty node list, when expecting a single node: " ++ n
    _   -> Left $ "encountered node list, when expecting a single node: " ++ n
{-# INLINE withNode #-}

withElement :: Text -> ([Node] -> Either String a) -> [Node] -> Either String a
withElement n f = join . fmap f . findElement n
{-# INLINE withElement #-}

findElement :: Text -> [Node] -> Either String [Node]
findElement n = maybe err Right . listToMaybe . mapMaybe (childNodes n)
  where
    err = Left $ "unable to find element " ++ show n
{-# INLINE findElement #-}

childNodes :: Text -> Node -> Maybe [Node]
childNodes n (NodeElement e)
    | nameLocalName (elementName e) == n = Just (elementNodes e)
childNodes _ _ = Nothing
{-# INLINE childNodes #-}

class FromXML a where
    parseXML :: [Node] -> Either String a

instance FromXML a => FromXML (Maybe a) where
    parseXML [] = pure Nothing
    parseXML ns = Just <$> parseXML ns
    {-# INLINE parseXML #-}

instance FromXML Text    where parseXML = parseXMLText "Text"
instance FromXML Int     where parseXML = parseXMLText "Int"
instance FromXML Integer where parseXML = parseXMLText "Integer"
instance FromXML Natural where parseXML = parseXMLText "Natural"
instance FromXML Double  where parseXML = parseXMLText "Double"
instance FromXML Bool    where parseXML = parseXMLText "Bool"

class ToXMLRoot a where
    toXMLRoot :: a -> Element

class ToXML a where
    toXML :: a -> [Node]

    default toXML :: ToXMLRoot a => a -> [Node]
    toXML = (:[]) . NodeElement . toXMLRoot
    {-# INLINE toXML #-}

instance ToXML a => ToXML (Maybe a) where
    toXML (Just x) = toXML x
    toXML Nothing  = []
    {-# INLINE toXML #-}

instance ToXML Text    where toXML = toXMLText
instance ToXML Int     where toXML = toXMLText
instance ToXML Integer where toXML = toXMLText
instance ToXML Natural where toXML = toXMLText
instance ToXML Double  where toXML = toXMLText
instance ToXML Bool    where toXML = toXMLText
