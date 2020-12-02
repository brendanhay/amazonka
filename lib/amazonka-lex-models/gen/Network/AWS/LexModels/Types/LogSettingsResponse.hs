{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LogSettingsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.LogSettingsResponse where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Destination
import Network.AWS.LexModels.Types.LogType
import Network.AWS.Prelude

-- | The settings for conversation logs.
--
--
--
-- /See:/ 'logSettingsResponse' smart constructor.
data LogSettingsResponse = LogSettingsResponse'
  { _lsDestination ::
      !(Maybe Destination),
    _lsKmsKeyARN :: !(Maybe Text),
    _lsLogType :: !(Maybe LogType),
    _lsResourceARN :: !(Maybe Text),
    _lsResourcePrefix :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsDestination' - The destination where logs are delivered.
--
-- * 'lsKmsKeyARN' - The Amazon Resource Name (ARN) of the key used to encrypt audio logs in an S3 bucket.
--
-- * 'lsLogType' - The type of logging that is enabled.
--
-- * 'lsResourceARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs are delivered.
--
-- * 'lsResourcePrefix' - The resource prefix is the first part of the S3 object key within the S3 bucket that you specified to contain audio logs. For CloudWatch Logs it is the prefix of the log stream name within the log group that you specified.
logSettingsResponse ::
  LogSettingsResponse
logSettingsResponse =
  LogSettingsResponse'
    { _lsDestination = Nothing,
      _lsKmsKeyARN = Nothing,
      _lsLogType = Nothing,
      _lsResourceARN = Nothing,
      _lsResourcePrefix = Nothing
    }

-- | The destination where logs are delivered.
lsDestination :: Lens' LogSettingsResponse (Maybe Destination)
lsDestination = lens _lsDestination (\s a -> s {_lsDestination = a})

-- | The Amazon Resource Name (ARN) of the key used to encrypt audio logs in an S3 bucket.
lsKmsKeyARN :: Lens' LogSettingsResponse (Maybe Text)
lsKmsKeyARN = lens _lsKmsKeyARN (\s a -> s {_lsKmsKeyARN = a})

-- | The type of logging that is enabled.
lsLogType :: Lens' LogSettingsResponse (Maybe LogType)
lsLogType = lens _lsLogType (\s a -> s {_lsLogType = a})

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs are delivered.
lsResourceARN :: Lens' LogSettingsResponse (Maybe Text)
lsResourceARN = lens _lsResourceARN (\s a -> s {_lsResourceARN = a})

-- | The resource prefix is the first part of the S3 object key within the S3 bucket that you specified to contain audio logs. For CloudWatch Logs it is the prefix of the log stream name within the log group that you specified.
lsResourcePrefix :: Lens' LogSettingsResponse (Maybe Text)
lsResourcePrefix = lens _lsResourcePrefix (\s a -> s {_lsResourcePrefix = a})

instance FromJSON LogSettingsResponse where
  parseJSON =
    withObject
      "LogSettingsResponse"
      ( \x ->
          LogSettingsResponse'
            <$> (x .:? "destination")
            <*> (x .:? "kmsKeyArn")
            <*> (x .:? "logType")
            <*> (x .:? "resourceArn")
            <*> (x .:? "resourcePrefix")
      )

instance Hashable LogSettingsResponse

instance NFData LogSettingsResponse
