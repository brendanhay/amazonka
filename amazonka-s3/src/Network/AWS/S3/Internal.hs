{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}

-- |
-- Module      : Network.AWS.S3.Internal
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Internal
    ( Region
    , BucketName      (..)
    , ETag            (..)
    , ObjectVersionId (..)
    , Delimiter
    , ObjectKey       (..)
    , _ObjectKey
    , keyPrefix
    , keyName
    , keyComponents
    ) where

import           Control.Lens
import           Data.String
import qualified Data.Text            as Text
import           Network.AWS.Data.XML
import           Network.AWS.Prelude

newtype BucketName = BucketName Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

-- FIXME: Add the difference between weak + strong ETags and their respective
-- equalities if necessary, see: https://github.com/brendanhay/amazonka/issues/76
newtype ETag = ETag ByteString
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ObjectVersionId = ObjectVersionId Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        )

newtype ObjectKey = ObjectKey Text
    deriving
        ( Eq
        , Ord
        , Read
        , Show
        , Data
        , Typeable
        , Generic
        , IsString
        , FromText
        , ToText
        , ToByteString
        , FromXML
        , ToXML
        , ToQuery
        , ToPath
        )

type Delimiter = Char

_ObjectKey :: Iso' ObjectKey Text
_ObjectKey = iso (\(ObjectKey k) -> k) ObjectKey
{-# INLINE _ObjectKey #-}

-- FIXME: Note about laws for combining keyPrefix/keyName.

-- | Traverse the prefix of an object key.
--
-- The prefix is classified as the entirety of the object key minus the name.
-- A leading prefix in the presence of a name, and no other delimiters is
-- interpreted as a blank prefix.
--
-- >>> "/home/jsmith/base.wiki" ^? keyPrefix '/'
-- Just "/home/jsmith"
--
-- >>> "/home/jsmith/" ^? keyPrefix '/'
-- Just "/home/jsmith"
--
-- >>> "/home" ^? keyPrefix '/'
-- Nothing
--
-- >>> "/" ^? keyPrefix '/'
-- Nothing
--
keyPrefix :: Delimiter -> Traversal' ObjectKey Text
keyPrefix c = _ObjectKeySnoc True c . _1
{-# INLINE keyPrefix #-}

-- | Traverse the name of an object key.
---
-- The name is classified as last path component based on the given delimiter.
-- A trailing delimiter is interpreted as a blank name.
--
-- >>> "/home/jsmith/base.wiki" ^? keyName '/'
-- Just "base.wiki"
--
-- >>> "/home/jsmith/" ^? keyName '/'
-- Just ""
--
-- >>> "/home" ^? keyName '/'
-- Just "home"
--
-- >>> "/" ^? keyName '/'
-- Just ""
--
keyName :: Delimiter -> Traversal' ObjectKey Text
keyName c = _ObjectKeySnoc False c . _2
{-# INLINE keyName #-}

-- | Traverse the path components of an object key using the specified delimiter.
keyComponents :: Delimiter -> IndexedTraversal' Int ObjectKey Text
keyComponents !c f (ObjectKey k) = cat <$> traversed f split
  where
    split = Text.split (== c) k
    cat   = ObjectKey . Text.intercalate (Text.singleton c)
{-# INLINE keyComponents #-}

-- | Modelled on the '_Snoc' type class from "Control.Lens.Cons".
_ObjectKeySnoc :: Bool -> Delimiter -> Prism' ObjectKey (Text, Text)
_ObjectKeySnoc dir !c = prism (ObjectKey . uncurry cat) split
  where
    split x@(ObjectKey k) =
        let (h, t) = Text.breakOnEnd suf k
         in if | Text.length h <= 1, dir -> Left x
               | otherwise               -> Right (Text.dropEnd 1 h, t)

    cat h t
        | Text.null h             = t
        | Text.null t             = h
        | suf `Text.isSuffixOf` h = h <> t
        | suf `Text.isPrefixOf` t = h <> t
        | otherwise               = h <> suf <> t

    suf = Text.singleton c
