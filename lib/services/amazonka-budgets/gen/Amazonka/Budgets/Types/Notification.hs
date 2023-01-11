{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Budgets.Types.Notification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.Notification where

import Amazonka.Budgets.Types.ComparisonOperator
import Amazonka.Budgets.Types.NotificationState
import Amazonka.Budgets.Types.NotificationType
import Amazonka.Budgets.Types.ThresholdType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A notification that\'s associated with a budget. A budget can have up to
-- ten notifications.
--
-- Each notification must have at least one subscriber. A notification can
-- have one SNS subscriber and up to 10 email subscribers, for a total of
-- 11 subscribers.
--
-- For example, if you have a budget for 200 dollars and you want to be
-- notified when you go over 160 dollars, create a notification with the
-- following parameters:
--
-- -   A notificationType of @ACTUAL@
--
-- -   A @thresholdType@ of @PERCENTAGE@
--
-- -   A @comparisonOperator@ of @GREATER_THAN@
--
-- -   A notification @threshold@ of @80@
--
-- /See:/ 'newNotification' smart constructor.
data Notification = Notification'
  { -- | Specifies whether this notification is in alarm. If a budget
    -- notification is in the @ALARM@ state, you passed the set threshold for
    -- the budget.
    notificationState :: Prelude.Maybe NotificationState,
    -- | The type of threshold for a notification. For @ABSOLUTE_VALUE@
    -- thresholds, Amazon Web Services notifies you when you go over or are
    -- forecasted to go over your total cost threshold. For @PERCENTAGE@
    -- thresholds, Amazon Web Services notifies you when you go over or are
    -- forecasted to go over a certain percentage of your forecasted spend. For
    -- example, if you have a budget for 200 dollars and you have a
    -- @PERCENTAGE@ threshold of 80%, Amazon Web Services notifies you when you
    -- go over 160 dollars.
    thresholdType :: Prelude.Maybe ThresholdType,
    -- | Specifies whether the notification is for how much you have spent
    -- (@ACTUAL@) or for how much that you\'re forecasted to spend
    -- (@FORECASTED@).
    notificationType :: NotificationType,
    -- | The comparison that\'s used for this notification.
    comparisonOperator :: ComparisonOperator,
    -- | The threshold that\'s associated with a notification. Thresholds are
    -- always a percentage, and many customers find value being alerted between
    -- 50% - 200% of the budgeted amount. The maximum limit for your threshold
    -- is 1,000,000% above the budgeted amount.
    threshold :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Notification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationState', 'notification_notificationState' - Specifies whether this notification is in alarm. If a budget
-- notification is in the @ALARM@ state, you passed the set threshold for
-- the budget.
--
-- 'thresholdType', 'notification_thresholdType' - The type of threshold for a notification. For @ABSOLUTE_VALUE@
-- thresholds, Amazon Web Services notifies you when you go over or are
-- forecasted to go over your total cost threshold. For @PERCENTAGE@
-- thresholds, Amazon Web Services notifies you when you go over or are
-- forecasted to go over a certain percentage of your forecasted spend. For
-- example, if you have a budget for 200 dollars and you have a
-- @PERCENTAGE@ threshold of 80%, Amazon Web Services notifies you when you
-- go over 160 dollars.
--
-- 'notificationType', 'notification_notificationType' - Specifies whether the notification is for how much you have spent
-- (@ACTUAL@) or for how much that you\'re forecasted to spend
-- (@FORECASTED@).
--
-- 'comparisonOperator', 'notification_comparisonOperator' - The comparison that\'s used for this notification.
--
-- 'threshold', 'notification_threshold' - The threshold that\'s associated with a notification. Thresholds are
-- always a percentage, and many customers find value being alerted between
-- 50% - 200% of the budgeted amount. The maximum limit for your threshold
-- is 1,000,000% above the budgeted amount.
newNotification ::
  -- | 'notificationType'
  NotificationType ->
  -- | 'comparisonOperator'
  ComparisonOperator ->
  -- | 'threshold'
  Prelude.Double ->
  Notification
newNotification
  pNotificationType_
  pComparisonOperator_
  pThreshold_ =
    Notification'
      { notificationState = Prelude.Nothing,
        thresholdType = Prelude.Nothing,
        notificationType = pNotificationType_,
        comparisonOperator = pComparisonOperator_,
        threshold = pThreshold_
      }

-- | Specifies whether this notification is in alarm. If a budget
-- notification is in the @ALARM@ state, you passed the set threshold for
-- the budget.
notification_notificationState :: Lens.Lens' Notification (Prelude.Maybe NotificationState)
notification_notificationState = Lens.lens (\Notification' {notificationState} -> notificationState) (\s@Notification' {} a -> s {notificationState = a} :: Notification)

-- | The type of threshold for a notification. For @ABSOLUTE_VALUE@
-- thresholds, Amazon Web Services notifies you when you go over or are
-- forecasted to go over your total cost threshold. For @PERCENTAGE@
-- thresholds, Amazon Web Services notifies you when you go over or are
-- forecasted to go over a certain percentage of your forecasted spend. For
-- example, if you have a budget for 200 dollars and you have a
-- @PERCENTAGE@ threshold of 80%, Amazon Web Services notifies you when you
-- go over 160 dollars.
notification_thresholdType :: Lens.Lens' Notification (Prelude.Maybe ThresholdType)
notification_thresholdType = Lens.lens (\Notification' {thresholdType} -> thresholdType) (\s@Notification' {} a -> s {thresholdType = a} :: Notification)

-- | Specifies whether the notification is for how much you have spent
-- (@ACTUAL@) or for how much that you\'re forecasted to spend
-- (@FORECASTED@).
notification_notificationType :: Lens.Lens' Notification NotificationType
notification_notificationType = Lens.lens (\Notification' {notificationType} -> notificationType) (\s@Notification' {} a -> s {notificationType = a} :: Notification)

-- | The comparison that\'s used for this notification.
notification_comparisonOperator :: Lens.Lens' Notification ComparisonOperator
notification_comparisonOperator = Lens.lens (\Notification' {comparisonOperator} -> comparisonOperator) (\s@Notification' {} a -> s {comparisonOperator = a} :: Notification)

-- | The threshold that\'s associated with a notification. Thresholds are
-- always a percentage, and many customers find value being alerted between
-- 50% - 200% of the budgeted amount. The maximum limit for your threshold
-- is 1,000,000% above the budgeted amount.
notification_threshold :: Lens.Lens' Notification Prelude.Double
notification_threshold = Lens.lens (\Notification' {threshold} -> threshold) (\s@Notification' {} a -> s {threshold = a} :: Notification)

instance Data.FromJSON Notification where
  parseJSON =
    Data.withObject
      "Notification"
      ( \x ->
          Notification'
            Prelude.<$> (x Data..:? "NotificationState")
            Prelude.<*> (x Data..:? "ThresholdType")
            Prelude.<*> (x Data..: "NotificationType")
            Prelude.<*> (x Data..: "ComparisonOperator")
            Prelude.<*> (x Data..: "Threshold")
      )

instance Prelude.Hashable Notification where
  hashWithSalt _salt Notification' {..} =
    _salt `Prelude.hashWithSalt` notificationState
      `Prelude.hashWithSalt` thresholdType
      `Prelude.hashWithSalt` notificationType
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` threshold

instance Prelude.NFData Notification where
  rnf Notification' {..} =
    Prelude.rnf notificationState
      `Prelude.seq` Prelude.rnf thresholdType
      `Prelude.seq` Prelude.rnf notificationType
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf threshold

instance Data.ToJSON Notification where
  toJSON Notification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotificationState" Data..=)
              Prelude.<$> notificationState,
            ("ThresholdType" Data..=) Prelude.<$> thresholdType,
            Prelude.Just
              ("NotificationType" Data..= notificationType),
            Prelude.Just
              ("ComparisonOperator" Data..= comparisonOperator),
            Prelude.Just ("Threshold" Data..= threshold)
          ]
      )
