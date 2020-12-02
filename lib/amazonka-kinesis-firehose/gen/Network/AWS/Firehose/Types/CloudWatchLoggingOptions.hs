{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.CloudWatchLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.CloudWatchLoggingOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon CloudWatch logging options for your delivery stream.
--
--
--
-- /See:/ 'cloudWatchLoggingOptions' smart constructor.
data CloudWatchLoggingOptions = CloudWatchLoggingOptions'
  { _cwloEnabled ::
      !(Maybe Bool),
    _cwloLogGroupName :: !(Maybe Text),
    _cwloLogStreamName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwloEnabled' - Enables or disables CloudWatch logging.
--
-- * 'cwloLogGroupName' - The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
--
-- * 'cwloLogStreamName' - The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
cloudWatchLoggingOptions ::
  CloudWatchLoggingOptions
cloudWatchLoggingOptions =
  CloudWatchLoggingOptions'
    { _cwloEnabled = Nothing,
      _cwloLogGroupName = Nothing,
      _cwloLogStreamName = Nothing
    }

-- | Enables or disables CloudWatch logging.
cwloEnabled :: Lens' CloudWatchLoggingOptions (Maybe Bool)
cwloEnabled = lens _cwloEnabled (\s a -> s {_cwloEnabled = a})

-- | The CloudWatch group name for logging. This value is required if CloudWatch logging is enabled.
cwloLogGroupName :: Lens' CloudWatchLoggingOptions (Maybe Text)
cwloLogGroupName = lens _cwloLogGroupName (\s a -> s {_cwloLogGroupName = a})

-- | The CloudWatch log stream name for logging. This value is required if CloudWatch logging is enabled.
cwloLogStreamName :: Lens' CloudWatchLoggingOptions (Maybe Text)
cwloLogStreamName = lens _cwloLogStreamName (\s a -> s {_cwloLogStreamName = a})

instance FromJSON CloudWatchLoggingOptions where
  parseJSON =
    withObject
      "CloudWatchLoggingOptions"
      ( \x ->
          CloudWatchLoggingOptions'
            <$> (x .:? "Enabled")
            <*> (x .:? "LogGroupName")
            <*> (x .:? "LogStreamName")
      )

instance Hashable CloudWatchLoggingOptions

instance NFData CloudWatchLoggingOptions

instance ToJSON CloudWatchLoggingOptions where
  toJSON CloudWatchLoggingOptions' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _cwloEnabled,
            ("LogGroupName" .=) <$> _cwloLogGroupName,
            ("LogStreamName" .=) <$> _cwloLogStreamName
          ]
      )
