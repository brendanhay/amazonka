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
    nNotificationType,
    nComparisonOperator,
    nThreshold,
    nNotificationState,
    nThresholdType,
  )
where

import qualified Network.AWS.Budgets.Types.ComparisonOperator as Types
import qualified Network.AWS.Budgets.Types.NotificationState as Types
import qualified Network.AWS.Budgets.Types.NotificationType as Types
import qualified Network.AWS.Budgets.Types.ThresholdType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
    notificationType :: Types.NotificationType,
    -- | The comparison that is used for this notification.
    comparisonOperator :: Types.ComparisonOperator,
    -- | The threshold that is associated with a notification. Thresholds are always a percentage, and many customers find value being alerted between 50% - 200% of the budgeted amount. The maximum limit for your threshold is 1,000,000% above the budgeted amount.
    threshold :: Core.Double,
    -- | Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
    notificationState :: Core.Maybe Types.NotificationState,
    -- | The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
    thresholdType :: Core.Maybe Types.ThresholdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Notification' value with any optional fields omitted.
mkNotification ::
  -- | 'notificationType'
  Types.NotificationType ->
  -- | 'comparisonOperator'
  Types.ComparisonOperator ->
  -- | 'threshold'
  Core.Double ->
  Notification
mkNotification notificationType comparisonOperator threshold =
  Notification'
    { notificationType,
      comparisonOperator,
      threshold,
      notificationState = Core.Nothing,
      thresholdType = Core.Nothing
    }

-- | Whether the notification is for how much you have spent (@ACTUAL@ ) or for how much you're forecasted to spend (@FORECASTED@ ).
--
-- /Note:/ Consider using 'notificationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNotificationType :: Lens.Lens' Notification Types.NotificationType
nNotificationType = Lens.field @"notificationType"
{-# DEPRECATED nNotificationType "Use generic-lens or generic-optics with 'notificationType' instead." #-}

-- | The comparison that is used for this notification.
--
-- /Note:/ Consider using 'comparisonOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nComparisonOperator :: Lens.Lens' Notification Types.ComparisonOperator
nComparisonOperator = Lens.field @"comparisonOperator"
{-# DEPRECATED nComparisonOperator "Use generic-lens or generic-optics with 'comparisonOperator' instead." #-}

-- | The threshold that is associated with a notification. Thresholds are always a percentage, and many customers find value being alerted between 50% - 200% of the budgeted amount. The maximum limit for your threshold is 1,000,000% above the budgeted amount.
--
-- /Note:/ Consider using 'threshold' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nThreshold :: Lens.Lens' Notification Core.Double
nThreshold = Lens.field @"threshold"
{-# DEPRECATED nThreshold "Use generic-lens or generic-optics with 'threshold' instead." #-}

-- | Whether this notification is in alarm. If a budget notification is in the @ALARM@ state, you have passed the set threshold for the budget.
--
-- /Note:/ Consider using 'notificationState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNotificationState :: Lens.Lens' Notification (Core.Maybe Types.NotificationState)
nNotificationState = Lens.field @"notificationState"
{-# DEPRECATED nNotificationState "Use generic-lens or generic-optics with 'notificationState' instead." #-}

-- | The type of threshold for a notification. For @ABSOLUTE_VALUE@ thresholds, AWS notifies you when you go over or are forecasted to go over your total cost threshold. For @PERCENTAGE@ thresholds, AWS notifies you when you go over or are forecasted to go over a certain percentage of your forecasted spend. For example, if you have a budget for 200 dollars and you have a @PERCENTAGE@ threshold of 80%, AWS notifies you when you go over 160 dollars.
--
-- /Note:/ Consider using 'thresholdType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nThresholdType :: Lens.Lens' Notification (Core.Maybe Types.ThresholdType)
nThresholdType = Lens.field @"thresholdType"
{-# DEPRECATED nThresholdType "Use generic-lens or generic-optics with 'thresholdType' instead." #-}

instance Core.FromJSON Notification where
  toJSON Notification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NotificationType" Core..= notificationType),
            Core.Just ("ComparisonOperator" Core..= comparisonOperator),
            Core.Just ("Threshold" Core..= threshold),
            ("NotificationState" Core..=) Core.<$> notificationState,
            ("ThresholdType" Core..=) Core.<$> thresholdType
          ]
      )

instance Core.FromJSON Notification where
  parseJSON =
    Core.withObject "Notification" Core.$
      \x ->
        Notification'
          Core.<$> (x Core..: "NotificationType")
          Core.<*> (x Core..: "ComparisonOperator")
          Core.<*> (x Core..: "Threshold")
          Core.<*> (x Core..:? "NotificationState")
          Core.<*> (x Core..:? "ThresholdType")
