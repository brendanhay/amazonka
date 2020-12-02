{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.SourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.SourceDetail where

import Network.AWS.Config.Types.EventSource
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.MessageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for @SourceDetail@ only for custom rules.
--
--
--
-- /See:/ 'sourceDetail' smart constructor.
data SourceDetail = SourceDetail'
  { _sdMessageType ::
      !(Maybe MessageType),
    _sdMaximumExecutionFrequency ::
      !(Maybe MaximumExecutionFrequency),
    _sdEventSource :: !(Maybe EventSource)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdMessageType' - The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .     * @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic evaluation when AWS Config delivers a configuration snapshot. If you want your custom rule to be triggered by configuration changes, specify two SourceDetail objects, one for @ConfigurationItemChangeNotification@ and one for @OversizedConfigurationItemChangeNotification@ .
--
-- * 'sdMaximumExecutionFrequency' - The frequency at which you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
--
-- * 'sdEventSource' - The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
sourceDetail ::
  SourceDetail
sourceDetail =
  SourceDetail'
    { _sdMessageType = Nothing,
      _sdMaximumExecutionFrequency = Nothing,
      _sdEventSource = Nothing
    }

-- | The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .     * @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic evaluation when AWS Config delivers a configuration snapshot. If you want your custom rule to be triggered by configuration changes, specify two SourceDetail objects, one for @ConfigurationItemChangeNotification@ and one for @OversizedConfigurationItemChangeNotification@ .
sdMessageType :: Lens' SourceDetail (Maybe MessageType)
sdMessageType = lens _sdMessageType (\s a -> s {_sdMessageType = a})

-- | The frequency at which you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
sdMaximumExecutionFrequency :: Lens' SourceDetail (Maybe MaximumExecutionFrequency)
sdMaximumExecutionFrequency = lens _sdMaximumExecutionFrequency (\s a -> s {_sdMaximumExecutionFrequency = a})

-- | The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
sdEventSource :: Lens' SourceDetail (Maybe EventSource)
sdEventSource = lens _sdEventSource (\s a -> s {_sdEventSource = a})

instance FromJSON SourceDetail where
  parseJSON =
    withObject
      "SourceDetail"
      ( \x ->
          SourceDetail'
            <$> (x .:? "MessageType")
            <*> (x .:? "MaximumExecutionFrequency")
            <*> (x .:? "EventSource")
      )

instance Hashable SourceDetail

instance NFData SourceDetail

instance ToJSON SourceDetail where
  toJSON SourceDetail' {..} =
    object
      ( catMaybes
          [ ("MessageType" .=) <$> _sdMessageType,
            ("MaximumExecutionFrequency" .=) <$> _sdMaximumExecutionFrequency,
            ("EventSource" .=) <$> _sdEventSource
          ]
      )
