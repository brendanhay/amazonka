{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LogConfig where

import Network.AWS.AppSync.Types.FieldLogLevel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The CloudWatch Logs configuration.
--
--
--
-- /See:/ 'logConfig' smart constructor.
data LogConfig = LogConfig'
  { _lcExcludeVerboseContent ::
      !(Maybe Bool),
    _lcFieldLogLevel :: !FieldLogLevel,
    _lcCloudWatchLogsRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcExcludeVerboseContent' - Set to TRUE to exclude sections that contain information such as headers, context, and evaluated mapping templates, regardless of logging level.
--
-- * 'lcFieldLogLevel' - The field logging level. Values can be NONE, ERROR, or ALL.      * __NONE__ : No field-level logs are captured.     * __ERROR__ : Logs the following information only for the fields that are in error:     * The error section in the server response.     * Field-level errors.     * The generated request/response functions that got resolved for error fields.     * __ALL__ : The following information is logged for all fields in the query:     * Field-level tracing information.     * The generated request/response functions that got resolved for each field.
--
-- * 'lcCloudWatchLogsRoleARN' - The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
logConfig ::
  -- | 'lcFieldLogLevel'
  FieldLogLevel ->
  -- | 'lcCloudWatchLogsRoleARN'
  Text ->
  LogConfig
logConfig pFieldLogLevel_ pCloudWatchLogsRoleARN_ =
  LogConfig'
    { _lcExcludeVerboseContent = Nothing,
      _lcFieldLogLevel = pFieldLogLevel_,
      _lcCloudWatchLogsRoleARN = pCloudWatchLogsRoleARN_
    }

-- | Set to TRUE to exclude sections that contain information such as headers, context, and evaluated mapping templates, regardless of logging level.
lcExcludeVerboseContent :: Lens' LogConfig (Maybe Bool)
lcExcludeVerboseContent = lens _lcExcludeVerboseContent (\s a -> s {_lcExcludeVerboseContent = a})

-- | The field logging level. Values can be NONE, ERROR, or ALL.      * __NONE__ : No field-level logs are captured.     * __ERROR__ : Logs the following information only for the fields that are in error:     * The error section in the server response.     * Field-level errors.     * The generated request/response functions that got resolved for error fields.     * __ALL__ : The following information is logged for all fields in the query:     * Field-level tracing information.     * The generated request/response functions that got resolved for each field.
lcFieldLogLevel :: Lens' LogConfig FieldLogLevel
lcFieldLogLevel = lens _lcFieldLogLevel (\s a -> s {_lcFieldLogLevel = a})

-- | The service role that AWS AppSync will assume to publish to Amazon CloudWatch logs in your account.
lcCloudWatchLogsRoleARN :: Lens' LogConfig Text
lcCloudWatchLogsRoleARN = lens _lcCloudWatchLogsRoleARN (\s a -> s {_lcCloudWatchLogsRoleARN = a})

instance FromJSON LogConfig where
  parseJSON =
    withObject
      "LogConfig"
      ( \x ->
          LogConfig'
            <$> (x .:? "excludeVerboseContent")
            <*> (x .: "fieldLogLevel")
            <*> (x .: "cloudWatchLogsRoleArn")
      )

instance Hashable LogConfig

instance NFData LogConfig

instance ToJSON LogConfig where
  toJSON LogConfig' {..} =
    object
      ( catMaybes
          [ ("excludeVerboseContent" .=) <$> _lcExcludeVerboseContent,
            Just ("fieldLogLevel" .= _lcFieldLogLevel),
            Just ("cloudWatchLogsRoleArn" .= _lcCloudWatchLogsRoleARN)
          ]
      )
