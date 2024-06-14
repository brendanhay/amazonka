{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Amazonka.S3.Internal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : This Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Internal
  ( -- * BucketName
    BucketName (..),
    _BucketName,

    -- * ETag
    ETag (..),
    _ETag,

    -- * Object Version ID
    ObjectVersionId (..),
    _ObjectVersionId,

    -- * Bucket Location
    LocationConstraint (..),
    _LocationConstraint,
    Region (..),

    -- * Object Key
    Delimiter,
    ObjectKey (..),
    _ObjectKey,
    objectKey_keyPrefix,
    objectKey_keyName,
    objectKey_keyComponents,

    -- * Website Endpoints
    getWebsiteEndpoint,
  )
where

import Amazonka.Core
import Amazonka.Core.Lens.Internal
  ( Traversal',
    coerced,
    prism,
    traversed,
    _1,
    _2,
  )
import Amazonka.Data
import Amazonka.Prelude
import qualified Data.Text as Text

newtype BucketName = BucketName {fromBucketName :: Text}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Generic,
      IsString,
      FromText,
      ToText,
      ToByteString,
      FromXML,
      ToXML,
      ToQuery,
      ToLog,
      FromJSON
    )

{-# INLINE _BucketName #-}
_BucketName :: Iso' BucketName Text
_BucketName = coerced

instance Hashable BucketName

instance NFData BucketName

-- FIXME: Add the difference between weak + strong ETags and their respective
-- equalities if necessary, see: https://github.com/brendanhay/amazonka/issues/76
newtype ETag = ETag {fromETag :: ByteString}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Generic,
      IsString,
      FromText,
      ToText,
      ToByteString,
      FromXML,
      ToXML,
      ToQuery,
      ToLog
    )

{-# INLINE _ETag #-}
_ETag :: Iso' ETag ByteString
_ETag = coerced

instance Hashable ETag

instance NFData ETag

newtype ObjectVersionId = ObjectVersionId {fromObjectVersionId :: Text}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Generic,
      IsString,
      FromText,
      ToText,
      ToByteString,
      FromXML,
      ToXML,
      ToQuery,
      ToLog
    )

_ObjectVersionId :: Iso' ObjectVersionId Text
_ObjectVersionId = coerced

instance Hashable ObjectVersionId

instance NFData ObjectVersionId

newtype LocationConstraint = LocationConstraint {constraintRegion :: Region}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Generic,
      ToText,
      ToByteString,
      ToLog
    )

{-# INLINE _LocationConstraint #-}
_LocationConstraint :: Iso' LocationConstraint Region
_LocationConstraint = coerced

instance Hashable LocationConstraint

instance NFData LocationConstraint

instance FromText LocationConstraint where
  fromText text =
    LocationConstraint
      <$> case Text.toLower text of
        "" -> pure NorthVirginia
        "eu" -> pure Ireland
        other -> pure $ Region' other

instance FromXML LocationConstraint where
  parseXML = \case
    [] -> pure (LocationConstraint NorthVirginia)
    ns -> parseXMLText "LocationConstraint" ns

instance ToXML LocationConstraint where
  toXML = \case
    LocationConstraint NorthVirginia -> XNull
    LocationConstraint r -> toXMLText r

newtype ObjectKey = ObjectKey Text
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Generic,
      IsString,
      FromText,
      ToText,
      ToByteString,
      FromXML,
      ToXML,
      ToQuery,
      ToPath,
      ToLog
    )

instance Hashable ObjectKey

instance NFData ObjectKey

type Delimiter = Char

_ObjectKey :: Iso' ObjectKey Text
_ObjectKey = coerced
{-# INLINE _ObjectKey #-}

-- FIXME: Note about laws for combining objectKey_keyPrefix/objectKey_keyName.

-- | Traverse the prefix of an object key.
--
-- The prefix is classified as the entirety of the object key minus the name.
-- A leading prefix in the presence of a name, and no other delimiters is
-- interpreted as a blank prefix.
--
-- >>> "/home/jsmith/base.wiki" ^? objectKey_keyPrefix '/'
-- Just "/home/jsmith"
--
-- >>> "/home/jsmith/" ^? objectKey_keyPrefix '/'
-- Just "/home/jsmith"
--
-- >>> "/home" ^? objectKey_keyPrefix '/'
-- Nothing
--
-- >>> "/" ^? objectKey_keyPrefix '/'
-- Nothing
objectKey_keyPrefix :: Delimiter -> Traversal' ObjectKey Text
objectKey_keyPrefix c = _ObjectKeySnoc True c . _1
{-# INLINE objectKey_keyPrefix #-}

-- | Traverse the name of an object key.
--
-- The name is classified as last path component based on the given delimiter.
-- A trailing delimiter is interpreted as a blank name.
--
-- >>> "/home/jsmith/base.wiki" ^? objectKey_keyName '/'
-- Just "base.wiki"
--
-- >>> "/home/jsmith/" ^? objectKey_keyName '/'
-- Just ""
--
-- >>> "/home" ^? objectKey_keyName '/'
-- Just "home"
--
-- >>> "/" ^? objectKey_keyName '/'
-- Just ""
objectKey_keyName :: Delimiter -> Traversal' ObjectKey Text
objectKey_keyName c = _ObjectKeySnoc False c . _2
{-# INLINE objectKey_keyName #-}

-- | Traverse the path components of an object key using the specified delimiter.
objectKey_keyComponents :: Delimiter -> Traversal' ObjectKey Text
objectKey_keyComponents !c f (ObjectKey k) = cat <$> traversed f split
  where
    split = Text.split (== c) k
    cat = ObjectKey . Text.intercalate (Text.singleton c)
{-# INLINE objectKey_keyComponents #-}

-- | Modelled on the '_Snoc' type class from "Control.Lens.Cons".
_ObjectKeySnoc :: Bool -> Delimiter -> Prism' ObjectKey (Text, Text)
_ObjectKeySnoc dir !c = prism (ObjectKey . uncurry cat) split
  where
    split x@(ObjectKey k) =
      let (h, t) = Text.breakOnEnd suf k
       in if
            | Text.length h <= 1, dir -> Left x
            | otherwise -> Right (Text.dropEnd 1 h, t)

    cat h t
      | Text.null h = t
      | Text.null t = h
      | suf `Text.isSuffixOf` h = h <> t
      | suf `Text.isPrefixOf` t = h <> t
      | otherwise = h <> suf <> t

    suf = Text.singleton c

-- | Get the S3 website endpoint for a specific region.
--
-- When you configure your bucket as a website, the website is available using
-- this region-specific website endpoint.
--
-- /See:/ <https://docs.aws.amazon.com/general/latest/gr/s3.html#s3_website_region_endpoints Amazon Simple Storage Service Website Endpoints>.
getWebsiteEndpoint :: Region -> Maybe Text
getWebsiteEndpoint = \case
  Ohio -> Just "s3-website.us-east-2.amazonaws.com"
  NorthVirginia -> Just "s3-website-us-east-1.amazonaws.com"
  NorthCalifornia -> Just "s3-website-us-west-1.amazonaws.com"
  Oregon -> Just "s3-website-us-west-2.amazonaws.com"
  CapeTown -> Just "s3-website.af-south-1.amazonaws.com"
  HongKong -> Just "s3-website.ap-east-1.amazonaws.com"
  Hyderabad -> Just "s3-website.ap-south-2.amazonaws.com"
  Jakarta -> Just "s3-website.ap-southeast-3.amazonaws.com"
  Melbourne -> Just "s3-website.ap-southeast-4.amazonaws.com"
  Mumbai -> Just "s3-website.ap-south-1.amazonaws.com"
  Osaka -> Just "s3-website.ap-northeast-3.amazonaws.com"
  Seoul -> Just "s3-website.ap-northeast-2.amazonaws.com"
  Singapore -> Just "s3-website-ap-southeast-1.amazonaws.com"
  Sydney -> Just "s3-website-ap-southeast-2.amazonaws.com"
  Tokyo -> Just "s3-website-ap-northeast-1.amazonaws.com"
  Montreal -> Just "s3-website.ca-central-1.amazonaws.com"
  Ningxia -> Just "s3-website.cn-northwest-1.amazonaws.com.cn"
  Frankfurt -> Just "s3-website.eu-central-1.amazonaws.com"
  Ireland -> Just "s3-website-eu-west-1.amazonaws.com"
  London -> Just "s3-website.eu-west-2.amazonaws.com"
  Milan -> Just "s3-website.eu-south-1.amazonaws.com"
  Paris -> Just "s3-website.eu-west-3.amazonaws.com"
  Stockholm -> Just "s3-website.eu-north-1.amazonaws.com"
  Spain -> Just "s3-website.eu-south-2.amazonaws.com"
  Zurich -> Just "s3-website.eu-central-2.amazonaws.com"
  Bahrain -> Just "s3-website.me-south-1.amazonaws.com"
  UAE -> Just "s3-website.me-central-1.amazonaws.com"
  SaoPaulo -> Just "s3-website-sa-east-1.amazonaws.com"
  GovCloudEast -> Just "s3-website.us-gov-east-1.amazonaws.com"
  GovCloudWest -> Just "s3-website-us-gov-west-1.amazonaws.com"
  Region' _ -> Nothing
