{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.EncryptionConfiguration where

import Network.AWS.ECR.Types.EncryptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
--
-- By default, when no encryption configuration is set or the @AES256@ encryption type is used, Amazon ECR uses server-side encryption with Amazon S3-managed encryption keys which encrypts your data at rest using an AES-256 encryption algorithm. This does not require any action on your part.
--
-- For more control over the encryption of the contents of your repository, you can use server-side encryption with customer master keys (CMKs) stored in AWS Key Management Service (AWS KMS) to encrypt your images. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/encryption-at-rest.html Amazon ECR encryption at rest> in the /Amazon Elastic Container Registry User Guide/ .
--
--
-- /See:/ 'encryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { _ecKmsKey ::
      !(Maybe Text),
    _ecEncryptionType :: !EncryptionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecKmsKey' - If you use the @KMS@ encryption type, specify the CMK to use for encryption. The alias, key ID, or full ARN of the CMK can be specified. The key must exist in the same Region as the repository. If no key is specified, the default AWS managed CMK for Amazon ECR will be used.
--
-- * 'ecEncryptionType' - The encryption type to use. If you use the @KMS@ encryption type, the contents of the repository will be encrypted using server-side encryption with customer master keys (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you can either use the default AWS managed CMK for Amazon ECR, or specify your own CMK, which you already created. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)> in the /Amazon Simple Storage Service Console Developer Guide./ . If you use the @AES256@ encryption type, Amazon ECR uses server-side encryption with Amazon S3-managed encryption keys which encrypts the images in the repository using an AES-256 encryption algorithm. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)> in the /Amazon Simple Storage Service Console Developer Guide./ .
encryptionConfiguration ::
  -- | 'ecEncryptionType'
  EncryptionType ->
  EncryptionConfiguration
encryptionConfiguration pEncryptionType_ =
  EncryptionConfiguration'
    { _ecKmsKey = Nothing,
      _ecEncryptionType = pEncryptionType_
    }

-- | If you use the @KMS@ encryption type, specify the CMK to use for encryption. The alias, key ID, or full ARN of the CMK can be specified. The key must exist in the same Region as the repository. If no key is specified, the default AWS managed CMK for Amazon ECR will be used.
ecKmsKey :: Lens' EncryptionConfiguration (Maybe Text)
ecKmsKey = lens _ecKmsKey (\s a -> s {_ecKmsKey = a})

-- | The encryption type to use. If you use the @KMS@ encryption type, the contents of the repository will be encrypted using server-side encryption with customer master keys (CMKs) stored in AWS KMS. When you use AWS KMS to encrypt your data, you can either use the default AWS managed CMK for Amazon ECR, or specify your own CMK, which you already created. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html Protecting Data Using Server-Side Encryption with CMKs Stored in AWS Key Management Service (SSE-KMS)> in the /Amazon Simple Storage Service Console Developer Guide./ . If you use the @AES256@ encryption type, Amazon ECR uses server-side encryption with Amazon S3-managed encryption keys which encrypts the images in the repository using an AES-256 encryption algorithm. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingServerSideEncryption.html Protecting Data Using Server-Side Encryption with Amazon S3-Managed Encryption Keys (SSE-S3)> in the /Amazon Simple Storage Service Console Developer Guide./ .
ecEncryptionType :: Lens' EncryptionConfiguration EncryptionType
ecEncryptionType = lens _ecEncryptionType (\s a -> s {_ecEncryptionType = a})

instance FromJSON EncryptionConfiguration where
  parseJSON =
    withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            <$> (x .:? "kmsKey") <*> (x .: "encryptionType")
      )

instance Hashable EncryptionConfiguration

instance NFData EncryptionConfiguration

instance ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    object
      ( catMaybes
          [ ("kmsKey" .=) <$> _ecKmsKey,
            Just ("encryptionType" .= _ecEncryptionType)
          ]
      )
