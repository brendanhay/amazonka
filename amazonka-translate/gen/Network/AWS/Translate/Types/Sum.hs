{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.Sum where

import Network.AWS.Prelude

data EncryptionKeyType =
  KMS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncryptionKeyType where
    parser = takeLowerText >>= \case
        "kms" -> pure KMS
        e -> fromTextError $ "Failure parsing EncryptionKeyType from value: '" <> e
           <> "'. Accepted values: kms"

instance ToText EncryptionKeyType where
    toText = \case
        KMS -> "KMS"

instance Hashable     EncryptionKeyType
instance NFData       EncryptionKeyType
instance ToByteString EncryptionKeyType
instance ToQuery      EncryptionKeyType
instance ToHeader     EncryptionKeyType

instance ToJSON EncryptionKeyType where
    toJSON = toJSONText

instance FromJSON EncryptionKeyType where
    parseJSON = parseJSONText "EncryptionKeyType"

data MergeStrategy =
  Overwrite
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MergeStrategy where
    parser = takeLowerText >>= \case
        "overwrite" -> pure Overwrite
        e -> fromTextError $ "Failure parsing MergeStrategy from value: '" <> e
           <> "'. Accepted values: overwrite"

instance ToText MergeStrategy where
    toText = \case
        Overwrite -> "OVERWRITE"

instance Hashable     MergeStrategy
instance NFData       MergeStrategy
instance ToByteString MergeStrategy
instance ToQuery      MergeStrategy
instance ToHeader     MergeStrategy

instance ToJSON MergeStrategy where
    toJSON = toJSONText

data TerminologyDataFormat
  = CSV
  | Tmx
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TerminologyDataFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        "tmx" -> pure Tmx
        e -> fromTextError $ "Failure parsing TerminologyDataFormat from value: '" <> e
           <> "'. Accepted values: csv, tmx"

instance ToText TerminologyDataFormat where
    toText = \case
        CSV -> "CSV"
        Tmx -> "TMX"

instance Hashable     TerminologyDataFormat
instance NFData       TerminologyDataFormat
instance ToByteString TerminologyDataFormat
instance ToQuery      TerminologyDataFormat
instance ToHeader     TerminologyDataFormat

instance ToJSON TerminologyDataFormat where
    toJSON = toJSONText
