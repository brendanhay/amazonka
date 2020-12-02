{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.LoggingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LoggingInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an S3 bucket to write instance-level logs to.
--
--
--
-- /See:/ 'loggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
  { _liS3KeyPrefix :: !(Maybe Text),
    _liS3BucketName :: !Text,
    _liS3Region :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liS3KeyPrefix' - (Optional) The S3 bucket subfolder.
--
-- * 'liS3BucketName' - The name of an S3 bucket where execution logs are stored .
--
-- * 'liS3Region' - The Region where the S3 bucket is located.
loggingInfo ::
  -- | 'liS3BucketName'
  Text ->
  -- | 'liS3Region'
  Text ->
  LoggingInfo
loggingInfo pS3BucketName_ pS3Region_ =
  LoggingInfo'
    { _liS3KeyPrefix = Nothing,
      _liS3BucketName = pS3BucketName_,
      _liS3Region = pS3Region_
    }

-- | (Optional) The S3 bucket subfolder.
liS3KeyPrefix :: Lens' LoggingInfo (Maybe Text)
liS3KeyPrefix = lens _liS3KeyPrefix (\s a -> s {_liS3KeyPrefix = a})

-- | The name of an S3 bucket where execution logs are stored .
liS3BucketName :: Lens' LoggingInfo Text
liS3BucketName = lens _liS3BucketName (\s a -> s {_liS3BucketName = a})

-- | The Region where the S3 bucket is located.
liS3Region :: Lens' LoggingInfo Text
liS3Region = lens _liS3Region (\s a -> s {_liS3Region = a})

instance FromJSON LoggingInfo where
  parseJSON =
    withObject
      "LoggingInfo"
      ( \x ->
          LoggingInfo'
            <$> (x .:? "S3KeyPrefix")
            <*> (x .: "S3BucketName")
            <*> (x .: "S3Region")
      )

instance Hashable LoggingInfo

instance NFData LoggingInfo

instance ToJSON LoggingInfo where
  toJSON LoggingInfo' {..} =
    object
      ( catMaybes
          [ ("S3KeyPrefix" .=) <$> _liS3KeyPrefix,
            Just ("S3BucketName" .= _liS3BucketName),
            Just ("S3Region" .= _liS3Region)
          ]
      )
