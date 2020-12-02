{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KMSEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KMSEncryptionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an encryption key for a destination in Amazon S3.
--
--
--
-- /See:/ 'kmsEncryptionConfig' smart constructor.
newtype KMSEncryptionConfig = KMSEncryptionConfig'
  { _kecAWSKMSKeyARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KMSEncryptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kecAWSKMSKeyARN' - The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
kmsEncryptionConfig ::
  -- | 'kecAWSKMSKeyARN'
  Text ->
  KMSEncryptionConfig
kmsEncryptionConfig pAWSKMSKeyARN_ =
  KMSEncryptionConfig' {_kecAWSKMSKeyARN = pAWSKMSKeyARN_}

-- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
kecAWSKMSKeyARN :: Lens' KMSEncryptionConfig Text
kecAWSKMSKeyARN = lens _kecAWSKMSKeyARN (\s a -> s {_kecAWSKMSKeyARN = a})

instance FromJSON KMSEncryptionConfig where
  parseJSON =
    withObject
      "KMSEncryptionConfig"
      (\x -> KMSEncryptionConfig' <$> (x .: "AWSKMSKeyARN"))

instance Hashable KMSEncryptionConfig

instance NFData KMSEncryptionConfig

instance ToJSON KMSEncryptionConfig where
  toJSON KMSEncryptionConfig' {..} =
    object (catMaybes [Just ("AWSKMSKeyARN" .= _kecAWSKMSKeyARN)])
