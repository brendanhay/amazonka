{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOption where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Log Publishing option that is set for given domain.
--
-- Attributes and their details:     * CloudWatchLogsLogGroupArn: ARN of the Cloudwatch log group to which log needs to be published.    * Enabled: Whether the log publishing for given log type is enabled or not
--
--
--
--
-- /See:/ 'logPublishingOption' smart constructor.
data LogPublishingOption = LogPublishingOption'
  { _lpoEnabled ::
      !(Maybe Bool),
    _lpoCloudWatchLogsLogGroupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogPublishingOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpoEnabled' - Specifies whether given log publishing option is enabled or not.
--
-- * 'lpoCloudWatchLogsLogGroupARN' - Undocumented member.
logPublishingOption ::
  LogPublishingOption
logPublishingOption =
  LogPublishingOption'
    { _lpoEnabled = Nothing,
      _lpoCloudWatchLogsLogGroupARN = Nothing
    }

-- | Specifies whether given log publishing option is enabled or not.
lpoEnabled :: Lens' LogPublishingOption (Maybe Bool)
lpoEnabled = lens _lpoEnabled (\s a -> s {_lpoEnabled = a})

-- | Undocumented member.
lpoCloudWatchLogsLogGroupARN :: Lens' LogPublishingOption (Maybe Text)
lpoCloudWatchLogsLogGroupARN = lens _lpoCloudWatchLogsLogGroupARN (\s a -> s {_lpoCloudWatchLogsLogGroupARN = a})

instance FromJSON LogPublishingOption where
  parseJSON =
    withObject
      "LogPublishingOption"
      ( \x ->
          LogPublishingOption'
            <$> (x .:? "Enabled") <*> (x .:? "CloudWatchLogsLogGroupArn")
      )

instance Hashable LogPublishingOption

instance NFData LogPublishingOption

instance ToJSON LogPublishingOption where
  toJSON LogPublishingOption' {..} =
    object
      ( catMaybes
          [ ("Enabled" .=) <$> _lpoEnabled,
            ("CloudWatchLogsLogGroupArn" .=)
              <$> _lpoCloudWatchLogsLogGroupARN
          ]
      )
