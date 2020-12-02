{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CampaignLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CampaignLimits where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | For a campaign, specifies limits on the messages that the campaign can send. For an application, specifies the default limits for messages that campaigns in the application can send.
--
--
--
-- /See:/ 'campaignLimits' smart constructor.
data CampaignLimits = CampaignLimits'
  { _clMessagesPerSecond ::
      !(Maybe Int),
    _clDaily :: !(Maybe Int),
    _clTotal :: !(Maybe Int),
    _clMaximumDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CampaignLimits' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clMessagesPerSecond' - The maximum number of messages that a campaign can send each second. For an application, this value specifies the default limit for the number of messages that campaigns can send each second. The minimum value is 50. The maximum value is 20,000.
--
-- * 'clDaily' - The maximum number of messages that a campaign can send to a single endpoint during a 24-hour period. For an application, this value specifies the default limit for the number of messages that campaigns and journeys can send to a single endpoint during a 24-hour period. The maximum value is 100.
--
-- * 'clTotal' - The maximum number of messages that a campaign can send to a single endpoint during the course of the campaign. If a campaign recurs, this setting applies to all runs of the campaign. The maximum value is 100.
--
-- * 'clMaximumDuration' - The maximum amount of time, in seconds, that a campaign can attempt to deliver a message after the scheduled start time for the campaign. The minimum value is 60 seconds.
campaignLimits ::
  CampaignLimits
campaignLimits =
  CampaignLimits'
    { _clMessagesPerSecond = Nothing,
      _clDaily = Nothing,
      _clTotal = Nothing,
      _clMaximumDuration = Nothing
    }

-- | The maximum number of messages that a campaign can send each second. For an application, this value specifies the default limit for the number of messages that campaigns can send each second. The minimum value is 50. The maximum value is 20,000.
clMessagesPerSecond :: Lens' CampaignLimits (Maybe Int)
clMessagesPerSecond = lens _clMessagesPerSecond (\s a -> s {_clMessagesPerSecond = a})

-- | The maximum number of messages that a campaign can send to a single endpoint during a 24-hour period. For an application, this value specifies the default limit for the number of messages that campaigns and journeys can send to a single endpoint during a 24-hour period. The maximum value is 100.
clDaily :: Lens' CampaignLimits (Maybe Int)
clDaily = lens _clDaily (\s a -> s {_clDaily = a})

-- | The maximum number of messages that a campaign can send to a single endpoint during the course of the campaign. If a campaign recurs, this setting applies to all runs of the campaign. The maximum value is 100.
clTotal :: Lens' CampaignLimits (Maybe Int)
clTotal = lens _clTotal (\s a -> s {_clTotal = a})

-- | The maximum amount of time, in seconds, that a campaign can attempt to deliver a message after the scheduled start time for the campaign. The minimum value is 60 seconds.
clMaximumDuration :: Lens' CampaignLimits (Maybe Int)
clMaximumDuration = lens _clMaximumDuration (\s a -> s {_clMaximumDuration = a})

instance FromJSON CampaignLimits where
  parseJSON =
    withObject
      "CampaignLimits"
      ( \x ->
          CampaignLimits'
            <$> (x .:? "MessagesPerSecond")
            <*> (x .:? "Daily")
            <*> (x .:? "Total")
            <*> (x .:? "MaximumDuration")
      )

instance Hashable CampaignLimits

instance NFData CampaignLimits

instance ToJSON CampaignLimits where
  toJSON CampaignLimits' {..} =
    object
      ( catMaybes
          [ ("MessagesPerSecond" .=) <$> _clMessagesPerSecond,
            ("Daily" .=) <$> _clDaily,
            ("Total" .=) <$> _clTotal,
            ("MaximumDuration" .=) <$> _clMaximumDuration
          ]
      )
