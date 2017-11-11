{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECR.Types.Sum where

import Network.AWS.Prelude

data ImageFailureCode
  = ImageNotFound
  | ImageTagDoesNotMatchDigest
  | InvalidImageDigest
  | InvalidImageTag
  | MissingDigestAndTag
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ImageFailureCode where
    parser = takeLowerText >>= \case
        "imagenotfound" -> pure ImageNotFound
        "imagetagdoesnotmatchdigest" -> pure ImageTagDoesNotMatchDigest
        "invalidimagedigest" -> pure InvalidImageDigest
        "invalidimagetag" -> pure InvalidImageTag
        "missingdigestandtag" -> pure MissingDigestAndTag
        e -> fromTextError $ "Failure parsing ImageFailureCode from value: '" <> e
           <> "'. Accepted values: imagenotfound, imagetagdoesnotmatchdigest, invalidimagedigest, invalidimagetag, missingdigestandtag"

instance ToText ImageFailureCode where
    toText = \case
        ImageNotFound -> "ImageNotFound"
        ImageTagDoesNotMatchDigest -> "ImageTagDoesNotMatchDigest"
        InvalidImageDigest -> "InvalidImageDigest"
        InvalidImageTag -> "InvalidImageTag"
        MissingDigestAndTag -> "MissingDigestAndTag"

instance Hashable     ImageFailureCode
instance NFData       ImageFailureCode
instance ToByteString ImageFailureCode
instance ToQuery      ImageFailureCode
instance ToHeader     ImageFailureCode

instance FromJSON ImageFailureCode where
    parseJSON = parseJSONText "ImageFailureCode"

data LayerAvailability
  = Available
  | Unavailable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LayerAvailability where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "unavailable" -> pure Unavailable
        e -> fromTextError $ "Failure parsing LayerAvailability from value: '" <> e
           <> "'. Accepted values: available, unavailable"

instance ToText LayerAvailability where
    toText = \case
        Available -> "AVAILABLE"
        Unavailable -> "UNAVAILABLE"

instance Hashable     LayerAvailability
instance NFData       LayerAvailability
instance ToByteString LayerAvailability
instance ToQuery      LayerAvailability
instance ToHeader     LayerAvailability

instance FromJSON LayerAvailability where
    parseJSON = parseJSONText "LayerAvailability"

data LayerFailureCode
  = InvalidLayerDigest
  | MissingLayerDigest
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LayerFailureCode where
    parser = takeLowerText >>= \case
        "invalidlayerdigest" -> pure InvalidLayerDigest
        "missinglayerdigest" -> pure MissingLayerDigest
        e -> fromTextError $ "Failure parsing LayerFailureCode from value: '" <> e
           <> "'. Accepted values: invalidlayerdigest, missinglayerdigest"

instance ToText LayerFailureCode where
    toText = \case
        InvalidLayerDigest -> "InvalidLayerDigest"
        MissingLayerDigest -> "MissingLayerDigest"

instance Hashable     LayerFailureCode
instance NFData       LayerFailureCode
instance ToByteString LayerFailureCode
instance ToQuery      LayerFailureCode
instance ToHeader     LayerFailureCode

instance FromJSON LayerFailureCode where
    parseJSON = parseJSONText "LayerFailureCode"

data TagStatus
  = Tagged
  | Untagged
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TagStatus where
    parser = takeLowerText >>= \case
        "tagged" -> pure Tagged
        "untagged" -> pure Untagged
        e -> fromTextError $ "Failure parsing TagStatus from value: '" <> e
           <> "'. Accepted values: tagged, untagged"

instance ToText TagStatus where
    toText = \case
        Tagged -> "TAGGED"
        Untagged -> "UNTAGGED"

instance Hashable     TagStatus
instance NFData       TagStatus
instance ToByteString TagStatus
instance ToQuery      TagStatus
instance ToHeader     TagStatus

instance ToJSON TagStatus where
    toJSON = toJSONText
