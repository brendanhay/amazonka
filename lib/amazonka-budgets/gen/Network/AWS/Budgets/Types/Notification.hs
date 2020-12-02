{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Notification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Notification where

import Network.AWS.Budgets.Types.ComparisonOperator
import Network.AWS.Budgets.Types.NotificationState
import Network.AWS.Budgets.Types.NotificationType
import Network.AWS.Budgets.Types.ThresholdType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A notification that is associated with a budget. A budget can have up to ten notifications.
--
--
-- Each notification must have at least one subscriber. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
--
-- For example, if you have a budget for 200 dollars and you want to be notified when you go over 160 dollars, create a notification with the following parameters:
--
--     * A notificationType of @ACTUAL@
--
--     * A @thresholdType@ of @PERCENTAGE@
--
--     * A @comparisonOperator@ of @GREATER_THAN@
--
--     * A notification @threshold@ of @80@
--
--
--
--
-- /See:/ 'notification' smart constructor.
data Notification = Notification'
  { _nThresholdType ::
      !(Maybe ThresholdType),
    _nNotificationState :: !(Maybe NotificationState),
    _nNotificationType :: !NotificationType,
    _nComparisonOperator :: !ComparisonOperator,
    _nThreshold :: !Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nThresholdType' - The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
--
-- * 'nNotificationState' - Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
--
-- * 'nNotificationType' - Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
--
-- * 'nComparisonOperator' - The comparison that is used for this notification.
--
-- * 'nThreshold' - The threshold that is associated with a notification. Thresholds are always a percentage, and many customers find value being alerted between 50% - 200% of the budgeted amount. The maximum limit for your threshold is 1,000,000% above the budgeted amount.
notification ::
  -- | 'nNotificationType'
  NotificationType ->
  -- | 'nComparisonOperator'
  ComparisonOperator ->
  -- | 'nThreshold'
  Double ->
  Notification
notification pNotificationType_ pComparisonOperator_ pThreshold_ =
  Notification'
    { _nThresholdType = Nothing,
      _nNotificationState = Nothing,
      _nNotificationType = pNotificationType_,
      _nComparisonOperator = pComparisonOperator_,
      _nThreshold = pThreshold_
    }

-- | The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
nThresholdType :: Lens' Notification (Maybe ThresholdType)
nThresholdType = lens _nThresholdType (\s a -> s {_nThresholdType = a})

-- | Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
nNotificationState :: Lens' Notification (Maybe NotificationState)
nNotificationState = lens _nNotificationState (\s a -> s {_nNotificationState = a})

-- | Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
nNotificationType :: Lens' Notification NotificationType
nNotificationType = lens _nNotificationType (\s a -> s {_nNotificationType = a})

-- | The comparison that is used for this notification.
nComparisonOperator :: Lens' Notification ComparisonOperator
nComparisonOperator = lens _nComparisonOperator (\s a -> s {_nComparisonOperator = a})

-- | The threshold that is associated with a notification. Thresholds are always a percentage, and many customers find value being alerted between 50% - 200% of the budgeted amount. The maximum limit for your threshold is 1,000,000% above the budgeted amount.
nThreshold :: Lens' Notification Double
nThreshold = lens _nThreshold (\s a -> s {_nThreshold = a})

instance FromJSON Notification where
  parseJSON =
    withObject
      "Notification"
      ( \x ->
          Notification'
            <$> (x .:? "ThresholdType")
            <*> (x .:? "NotificationState")
            <*> (x .: "NotificationType")
            <*> (x .: "ComparisonOperator")
            <*> (x .: "Threshold")
      )

instance Hashable Notification

instance NFData Notification

instance ToJSON Notification where
  toJSON Notification' {..} =
    object
      ( catMaybes
          [ ("ThresholdType" .=) <$> _nThresholdType,
            ("NotificationState" .=) <$> _nNotificationState,
            Just ("NotificationType" .= _nNotificationType),
            Just ("ComparisonOperator" .= _nComparisonOperator),
            Just ("Threshold" .= _nThreshold)
          ]
      )
