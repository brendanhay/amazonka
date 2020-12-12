{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Notification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Notification
  ( Notification (..),

    -- * Smart constructor
    mkNotification,

    -- * Lenses
    nThresholdType,
    nNotificationState,
    nNotificationType,
    nComparisonOperator,
    nThreshold,
  )
where

import Network.AWS.Budgets.Types.ComparisonOperator
import Network.AWS.Budgets.Types.NotificationState
import Network.AWS.Budgets.Types.NotificationType
import Network.AWS.Budgets.Types.ThresholdType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A notification that is associated with a budget. A budget can have up to ten notifications.
--
-- Each notification must have at least one subscriber. A notification can have one SNS subscriber and up to 10 email subscribers, for a total of 11 subscribers.
-- For example, if you have a budget for 200 dollars and you want to be notified when you go over 160 dollars, create a notification with the following parameters:
--
--     * A notificationType of @ACTUAL@
--
--
--     * A @thresholdType@ of @PERCENTAGE@
--
--
--     * A @comparisonOperator@ of @GREATER_THAN@
--
--
--     * A notification @threshold@ of @80@
--
--
--
-- /See:/ 'mkNotification' smart constructor.
data Notification = Notification'
  { thresholdType ::
      Lude.Maybe ThresholdType,
    notificationState :: Lude.Maybe NotificationState,
    notificationType :: NotificationType,
    comparisonOperator :: ComparisonOperator,
    threshold :: Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Notification' with the minimum fields required to make a request.
--
-- * 'comparisonOperator' - The comparison that is used for this notification.
-- * 'notificationState' - Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
-- * 'notificationType' - Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
-- * 'threshold' - The threshold that is associated with a notification. Thresholds are always a percentage, and many customers find value being alerted between 50% - 200% of the budgeted amount. The maximum limit for your threshold is 1,000,000% above the budgeted amount.
-- * 'thresholdType' - The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
mkNotification ::
  -- | 'notificationType'
  NotificationType ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Lude.Double ->
  Notification
mkNotification pNotificationType_ pComparisonOperator_ pThreshold_ =
  Notification'
    { thresholdType = Lude.Nothing,
      notificationState = Lude.Nothing,
      notificationType = pNotificationType_,
      comparisonOperator = pComparisonOperator_,
      threshold = pThreshold_
    }

-- | The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
--
-- /Note:/ Consider using 'thresholdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nThresholdType :: Lens.Lens' Notification (Lude.Maybe ThresholdType)
nThresholdType = Lens.lens (thresholdType :: Notification -> Lude.Maybe ThresholdType) (\s a -> s {thresholdType = a} :: Notification)
{-# DEPRECATED nThresholdType "Use generic-lens or generic-optics with 'thresholdType' instead." #-}

-- | Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
--
-- /Note:/ Consider using 'notificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNotificationState :: Lens.Lens' Notification (Lude.Maybe NotificationState)
nNotificationState = Lens.lens (notificationState :: Notification -> Lude.Maybe NotificationState) (\s a -> s {notificationState = a} :: Notification)
{-# DEPRECATED nNotificationState "Use generic-lens or generic-optics with 'notificationState' instead." #-}

-- | Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNotificationType :: Lens.Lens' Notification NotificationType
nNotificationType = Lens.lens (notificationType :: Notification -> NotificationType) (\s a -> s {notificationType = a} :: Notification)
{-# DEPRECATED nNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | The comparison that is used for this notification.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nComparisonOperator :: Lens.Lens' Notification ComparisonOperator
nComparisonOperator = Lens.lens (comparisonOperator :: Notification -> ComparisonOperator) (\s a -> s {comparisonOperator = a} :: Notification)
{-# DEPRECATED nComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The threshold that is associated with a notification. Thresholds are always a percentage, and many customers find value being alerted between 50% - 200% of the budgeted amount. The maximum limit for your threshold is 1,000,000% above the budgeted amount.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nThreshold :: Lens.Lens' Notification Lude.Double
nThreshold = Lens.lens (threshold :: Notification -> Lude.Double) (\s a -> s {threshold = a} :: Notification)
{-# DEPRECATED nThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

instance Lude.FromJSON Notification where
  parseJSON =
    Lude.withObject
      "Notification"
      ( \x ->
          Notification'
            Lude.<$> (x Lude..:? "ThresholdType")
            Lude.<*> (x Lude..:? "NotificationState")
            Lude.<*> (x Lude..: "NotificationType")
            Lude.<*> (x Lude..: "ComparisonOperator")
            Lude.<*> (x Lude..: "Threshold")
      )

instance Lude.ToJSON Notification where
  toJSON Notification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ThresholdType" Lude..=) Lude.<$> thresholdType,
            ("NotificationState" Lude..=) Lude.<$> notificationState,
            Lude.Just ("NotificationType" Lude..= notificationType),
            Lude.Just ("ComparisonOperator" Lude..= comparisonOperator),
            Lude.Just ("Threshold" Lude..= threshold)
          ]
      )
