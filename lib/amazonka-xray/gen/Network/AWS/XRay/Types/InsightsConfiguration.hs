{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightsConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure containing configurations related to insights.
--
--
--
-- /See:/ 'insightsConfiguration' smart constructor.
data InsightsConfiguration = InsightsConfiguration'
  { _icNotificationsEnabled ::
      !(Maybe Bool),
    _icInsightsEnabled :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icNotificationsEnabled' - Set the NotificationsEnabled value to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
-- * 'icInsightsEnabled' - Set the InsightsEnabled value to true to enable insights or false to disable insights.
insightsConfiguration ::
  InsightsConfiguration
insightsConfiguration =
  InsightsConfiguration'
    { _icNotificationsEnabled = Nothing,
      _icInsightsEnabled = Nothing
    }

-- | Set the NotificationsEnabled value to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
icNotificationsEnabled :: Lens' InsightsConfiguration (Maybe Bool)
icNotificationsEnabled = lens _icNotificationsEnabled (\s a -> s {_icNotificationsEnabled = a})

-- | Set the InsightsEnabled value to true to enable insights or false to disable insights.
icInsightsEnabled :: Lens' InsightsConfiguration (Maybe Bool)
icInsightsEnabled = lens _icInsightsEnabled (\s a -> s {_icInsightsEnabled = a})

instance FromJSON InsightsConfiguration where
  parseJSON =
    withObject
      "InsightsConfiguration"
      ( \x ->
          InsightsConfiguration'
            <$> (x .:? "NotificationsEnabled") <*> (x .:? "InsightsEnabled")
      )

instance Hashable InsightsConfiguration

instance NFData InsightsConfiguration

instance ToJSON InsightsConfiguration where
  toJSON InsightsConfiguration' {..} =
    object
      ( catMaybes
          [ ("NotificationsEnabled" .=) <$> _icNotificationsEnabled,
            ("InsightsEnabled" .=) <$> _icInsightsEnabled
          ]
      )
