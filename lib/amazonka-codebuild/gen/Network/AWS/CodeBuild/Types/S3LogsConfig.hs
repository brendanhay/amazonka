{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.S3LogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.S3LogsConfig where

import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about S3 logs for a build project.
--
--
--
-- /See:/ 's3LogsConfig' smart constructor.
data S3LogsConfig = S3LogsConfig'
  { _slcLocation :: !(Maybe Text),
    _slcEncryptionDisabled :: !(Maybe Bool),
    _slcStatus :: !LogsConfigStatusType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3LogsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slcLocation' - The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ .
--
-- * 'slcEncryptionDisabled' - Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted.
--
-- * 'slcStatus' - The current status of the S3 build logs. Valid values are:     * @ENABLED@ : S3 build logs are enabled for this build project.     * @DISABLED@ : S3 build logs are not enabled for this build project.
s3LogsConfig ::
  -- | 'slcStatus'
  LogsConfigStatusType ->
  S3LogsConfig
s3LogsConfig pStatus_ =
  S3LogsConfig'
    { _slcLocation = Nothing,
      _slcEncryptionDisabled = Nothing,
      _slcStatus = pStatus_
    }

-- | The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ .
slcLocation :: Lens' S3LogsConfig (Maybe Text)
slcLocation = lens _slcLocation (\s a -> s {_slcLocation = a})

-- | Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted.
slcEncryptionDisabled :: Lens' S3LogsConfig (Maybe Bool)
slcEncryptionDisabled = lens _slcEncryptionDisabled (\s a -> s {_slcEncryptionDisabled = a})

-- | The current status of the S3 build logs. Valid values are:     * @ENABLED@ : S3 build logs are enabled for this build project.     * @DISABLED@ : S3 build logs are not enabled for this build project.
slcStatus :: Lens' S3LogsConfig LogsConfigStatusType
slcStatus = lens _slcStatus (\s a -> s {_slcStatus = a})

instance FromJSON S3LogsConfig where
  parseJSON =
    withObject
      "S3LogsConfig"
      ( \x ->
          S3LogsConfig'
            <$> (x .:? "location")
            <*> (x .:? "encryptionDisabled")
            <*> (x .: "status")
      )

instance Hashable S3LogsConfig

instance NFData S3LogsConfig

instance ToJSON S3LogsConfig where
  toJSON S3LogsConfig' {..} =
    object
      ( catMaybes
          [ ("location" .=) <$> _slcLocation,
            ("encryptionDisabled" .=) <$> _slcEncryptionDisabled,
            Just ("status" .= _slcStatus)
          ]
      )
