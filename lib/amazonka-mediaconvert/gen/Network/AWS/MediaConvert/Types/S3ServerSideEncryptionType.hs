{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
  ( S3ServerSideEncryptionType
      ( S3ServerSideEncryptionType',
        ServerSideEncryptionS3,
        ServerSideEncryptionKMS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
newtype S3ServerSideEncryptionType = S3ServerSideEncryptionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ServerSideEncryptionS3 :: S3ServerSideEncryptionType
pattern ServerSideEncryptionS3 = S3ServerSideEncryptionType' "SERVER_SIDE_ENCRYPTION_S3"

pattern ServerSideEncryptionKMS :: S3ServerSideEncryptionType
pattern ServerSideEncryptionKMS = S3ServerSideEncryptionType' "SERVER_SIDE_ENCRYPTION_KMS"

{-# COMPLETE
  ServerSideEncryptionS3,
  ServerSideEncryptionKMS,
  S3ServerSideEncryptionType'
  #-}
