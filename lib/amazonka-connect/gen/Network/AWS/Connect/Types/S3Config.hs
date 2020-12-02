{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.S3Config
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.S3Config where

import Network.AWS.Connect.Types.EncryptionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the S3 storage type.
--
--
--
-- /See:/ 's3Config' smart constructor.
data S3Config = S3Config'
  { _scEncryptionConfig ::
      !(Maybe EncryptionConfig),
    _scBucketName :: !Text,
    _scBucketPrefix :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Config' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scEncryptionConfig' - The S3 encryption configuration.
--
-- * 'scBucketName' - The S3 bucket name.
--
-- * 'scBucketPrefix' - The S3 bucket prefix.
s3Config ::
  -- | 'scBucketName'
  Text ->
  -- | 'scBucketPrefix'
  Text ->
  S3Config
s3Config pBucketName_ pBucketPrefix_ =
  S3Config'
    { _scEncryptionConfig = Nothing,
      _scBucketName = pBucketName_,
      _scBucketPrefix = pBucketPrefix_
    }

-- | The S3 encryption configuration.
scEncryptionConfig :: Lens' S3Config (Maybe EncryptionConfig)
scEncryptionConfig = lens _scEncryptionConfig (\s a -> s {_scEncryptionConfig = a})

-- | The S3 bucket name.
scBucketName :: Lens' S3Config Text
scBucketName = lens _scBucketName (\s a -> s {_scBucketName = a})

-- | The S3 bucket prefix.
scBucketPrefix :: Lens' S3Config Text
scBucketPrefix = lens _scBucketPrefix (\s a -> s {_scBucketPrefix = a})

instance FromJSON S3Config where
  parseJSON =
    withObject
      "S3Config"
      ( \x ->
          S3Config'
            <$> (x .:? "EncryptionConfig")
            <*> (x .: "BucketName")
            <*> (x .: "BucketPrefix")
      )

instance Hashable S3Config

instance NFData S3Config

instance ToJSON S3Config where
  toJSON S3Config' {..} =
    object
      ( catMaybes
          [ ("EncryptionConfig" .=) <$> _scEncryptionConfig,
            Just ("BucketName" .= _scBucketName),
            Just ("BucketPrefix" .= _scBucketPrefix)
          ]
      )
