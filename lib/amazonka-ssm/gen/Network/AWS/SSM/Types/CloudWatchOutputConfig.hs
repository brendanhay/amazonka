{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CloudWatchOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CloudWatchOutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration options for sending command output to CloudWatch Logs.
--
--
--
-- /See:/ 'cloudWatchOutputConfig' smart constructor.
data CloudWatchOutputConfig = CloudWatchOutputConfig'
  { _cwocCloudWatchLogGroupName ::
      !(Maybe Text),
    _cwocCloudWatchOutputEnabled :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwocCloudWatchLogGroupName' - The name of the CloudWatch log group where you want to send command output. If you don't specify a group name, Systems Manager automatically creates a log group for you. The log group uses the following naming format: aws/ssm//SystemsManagerDocumentName/ .
--
-- * 'cwocCloudWatchOutputEnabled' - Enables Systems Manager to send command output to CloudWatch Logs.
cloudWatchOutputConfig ::
  CloudWatchOutputConfig
cloudWatchOutputConfig =
  CloudWatchOutputConfig'
    { _cwocCloudWatchLogGroupName = Nothing,
      _cwocCloudWatchOutputEnabled = Nothing
    }

-- | The name of the CloudWatch log group where you want to send command output. If you don't specify a group name, Systems Manager automatically creates a log group for you. The log group uses the following naming format: aws/ssm//SystemsManagerDocumentName/ .
cwocCloudWatchLogGroupName :: Lens' CloudWatchOutputConfig (Maybe Text)
cwocCloudWatchLogGroupName = lens _cwocCloudWatchLogGroupName (\s a -> s {_cwocCloudWatchLogGroupName = a})

-- | Enables Systems Manager to send command output to CloudWatch Logs.
cwocCloudWatchOutputEnabled :: Lens' CloudWatchOutputConfig (Maybe Bool)
cwocCloudWatchOutputEnabled = lens _cwocCloudWatchOutputEnabled (\s a -> s {_cwocCloudWatchOutputEnabled = a})

instance FromJSON CloudWatchOutputConfig where
  parseJSON =
    withObject
      "CloudWatchOutputConfig"
      ( \x ->
          CloudWatchOutputConfig'
            <$> (x .:? "CloudWatchLogGroupName")
            <*> (x .:? "CloudWatchOutputEnabled")
      )

instance Hashable CloudWatchOutputConfig

instance NFData CloudWatchOutputConfig

instance ToJSON CloudWatchOutputConfig where
  toJSON CloudWatchOutputConfig' {..} =
    object
      ( catMaybes
          [ ("CloudWatchLogGroupName" .=) <$> _cwocCloudWatchLogGroupName,
            ("CloudWatchOutputEnabled" .=) <$> _cwocCloudWatchOutputEnabled
          ]
      )
