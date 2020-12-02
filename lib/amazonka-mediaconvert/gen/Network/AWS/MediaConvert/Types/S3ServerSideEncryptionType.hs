{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType where

import Network.AWS.Prelude

-- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
data S3ServerSideEncryptionType
  = ServerSideEncryptionKMS
  | ServerSideEncryptionS3
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText S3ServerSideEncryptionType where
  parser =
    takeLowerText >>= \case
      "server_side_encryption_kms" -> pure ServerSideEncryptionKMS
      "server_side_encryption_s3" -> pure ServerSideEncryptionS3
      e ->
        fromTextError $
          "Failure parsing S3ServerSideEncryptionType from value: '" <> e
            <> "'. Accepted values: server_side_encryption_kms, server_side_encryption_s3"

instance ToText S3ServerSideEncryptionType where
  toText = \case
    ServerSideEncryptionKMS -> "SERVER_SIDE_ENCRYPTION_KMS"
    ServerSideEncryptionS3 -> "SERVER_SIDE_ENCRYPTION_S3"

instance Hashable S3ServerSideEncryptionType

instance NFData S3ServerSideEncryptionType

instance ToByteString S3ServerSideEncryptionType

instance ToQuery S3ServerSideEncryptionType

instance ToHeader S3ServerSideEncryptionType

instance ToJSON S3ServerSideEncryptionType where
  toJSON = toJSONText

instance FromJSON S3ServerSideEncryptionType where
  parseJSON = parseJSONText "S3ServerSideEncryptionType"
