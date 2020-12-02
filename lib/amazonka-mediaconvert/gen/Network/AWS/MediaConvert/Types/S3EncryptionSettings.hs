{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3EncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3EncryptionSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.S3ServerSideEncryptionType
import Network.AWS.Prelude

-- | Settings for how your job outputs are encrypted as they are uploaded to Amazon S3.
--
-- /See:/ 's3EncryptionSettings' smart constructor.
data S3EncryptionSettings = S3EncryptionSettings'
  { _sesEncryptionType ::
      !(Maybe S3ServerSideEncryptionType),
    _sesKMSKeyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3EncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sesEncryptionType' - Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
--
-- * 'sesKMSKeyARN' - Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
s3EncryptionSettings ::
  S3EncryptionSettings
s3EncryptionSettings =
  S3EncryptionSettings'
    { _sesEncryptionType = Nothing,
      _sesKMSKeyARN = Nothing
    }

-- | Specify how you want your data keys managed. AWS uses data keys to encrypt your content. AWS also encrypts the data keys themselves, using a customer master key (CMK), and then stores the encrypted data keys alongside your encrypted content. Use this setting to specify which AWS service manages the CMK. For simplest set up, choose Amazon S3 (SERVER_SIDE_ENCRYPTION_S3). If you want your master key to be managed by AWS Key Management Service (KMS), choose AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). By default, when you choose AWS KMS, KMS uses the AWS managed customer master key (CMK) associated with Amazon S3 to encrypt your data keys. You can optionally choose to specify a different, customer managed CMK. Do so by specifying the Amazon Resource Name (ARN) of the key for the setting  KMS ARN (kmsKeyArn).
sesEncryptionType :: Lens' S3EncryptionSettings (Maybe S3ServerSideEncryptionType)
sesEncryptionType = lens _sesEncryptionType (\s a -> s {_sesEncryptionType = a})

-- | Optionally, specify the customer master key (CMK) that you want to use to encrypt the data key that AWS uses to encrypt your output content. Enter the Amazon Resource Name (ARN) of the CMK. To use this setting, you must also set Server-side encryption (S3ServerSideEncryptionType) to AWS KMS (SERVER_SIDE_ENCRYPTION_KMS). If you set Server-side encryption to AWS KMS but don't specify a CMK here, AWS uses the AWS managed CMK associated with Amazon S3.
sesKMSKeyARN :: Lens' S3EncryptionSettings (Maybe Text)
sesKMSKeyARN = lens _sesKMSKeyARN (\s a -> s {_sesKMSKeyARN = a})

instance FromJSON S3EncryptionSettings where
  parseJSON =
    withObject
      "S3EncryptionSettings"
      ( \x ->
          S3EncryptionSettings'
            <$> (x .:? "encryptionType") <*> (x .:? "kmsKeyArn")
      )

instance Hashable S3EncryptionSettings

instance NFData S3EncryptionSettings

instance ToJSON S3EncryptionSettings where
  toJSON S3EncryptionSettings' {..} =
    object
      ( catMaybes
          [ ("encryptionType" .=) <$> _sesEncryptionType,
            ("kmsKeyArn" .=) <$> _sesKMSKeyARN
          ]
      )
