{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.Sum where

import           Network.AWS.Prelude

data CompressionFormat
    = Gzip
    | Snappy
    | Uncompressed
    | Zip
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CompressionFormat where
    parser = takeLowerText >>= \case
        "gzip" -> pure Gzip
        "snappy" -> pure Snappy
        "uncompressed" -> pure Uncompressed
        "zip" -> pure Zip
        e -> fromTextError $ "Failure parsing CompressionFormat from value: '" <> e
           <> "'. Accepted values: GZIP, Snappy, UNCOMPRESSED, ZIP"

instance ToText CompressionFormat where
    toText = \case
        Gzip -> "GZIP"
        Snappy -> "Snappy"
        Uncompressed -> "UNCOMPRESSED"
        Zip -> "ZIP"

instance Hashable     CompressionFormat
instance NFData       CompressionFormat
instance ToByteString CompressionFormat
instance ToQuery      CompressionFormat
instance ToHeader     CompressionFormat

instance ToJSON CompressionFormat where
    toJSON = toJSONText

instance FromJSON CompressionFormat where
    parseJSON = parseJSONText "CompressionFormat"

data DeliveryStreamStatus
    = Active
    | Creating
    | Deleting
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText DeliveryStreamStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "deleting" -> pure Deleting
        e -> fromTextError $ "Failure parsing DeliveryStreamStatus from value: '" <> e
           <> "'. Accepted values: ACTIVE, CREATING, DELETING"

instance ToText DeliveryStreamStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Deleting -> "DELETING"

instance Hashable     DeliveryStreamStatus
instance NFData       DeliveryStreamStatus
instance ToByteString DeliveryStreamStatus
instance ToQuery      DeliveryStreamStatus
instance ToHeader     DeliveryStreamStatus

instance FromJSON DeliveryStreamStatus where
    parseJSON = parseJSONText "DeliveryStreamStatus"

data NoEncryptionConfig =
    NoEncryption
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText NoEncryptionConfig where
    parser = takeLowerText >>= \case
        "noencryption" -> pure NoEncryption
        e -> fromTextError $ "Failure parsing NoEncryptionConfig from value: '" <> e
           <> "'. Accepted values: NoEncryption"

instance ToText NoEncryptionConfig where
    toText = \case
        NoEncryption -> "NoEncryption"

instance Hashable     NoEncryptionConfig
instance NFData       NoEncryptionConfig
instance ToByteString NoEncryptionConfig
instance ToQuery      NoEncryptionConfig
instance ToHeader     NoEncryptionConfig

instance ToJSON NoEncryptionConfig where
    toJSON = toJSONText

instance FromJSON NoEncryptionConfig where
    parseJSON = parseJSONText "NoEncryptionConfig"
