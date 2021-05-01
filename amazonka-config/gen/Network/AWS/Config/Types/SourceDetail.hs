{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.SourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.SourceDetail where

import Network.AWS.Config.Types.EventSource
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.MessageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides the source and the message types that trigger AWS Config to
-- evaluate your AWS resources against a rule. It also provides the
-- frequency with which you want AWS Config to run evaluations for the rule
-- if the trigger type is periodic. You can specify the parameter values
-- for @SourceDetail@ only for custom rules.
--
-- /See:/ 'newSourceDetail' smart constructor.
data SourceDetail = SourceDetail'
  { -- | The source of the event, such as an AWS service, that triggers AWS
    -- Config to evaluate your AWS resources.
    eventSource :: Prelude.Maybe EventSource,
    -- | The frequency at which you want AWS Config to run evaluations for a
    -- custom rule with a periodic trigger. If you specify a value for
    -- @MaximumExecutionFrequency@, then @MessageType@ must use the
    -- @ScheduledNotification@ value.
    --
    -- By default, rules with a periodic trigger are evaluated every 24 hours.
    -- To change the frequency, specify a valid value for the
    -- @MaximumExecutionFrequency@ parameter.
    --
    -- Based on the valid value you choose, AWS Config runs evaluations once
    -- for each valid value. For example, if you choose @Three_Hours@, AWS
    -- Config runs evaluations once every three hours. In this case,
    -- @Three_Hours@ is the frequency of this rule.
    maximumExecutionFrequency :: Prelude.Maybe MaximumExecutionFrequency,
    -- | The type of notification that triggers AWS Config to run an evaluation
    -- for a rule. You can specify the following notification types:
    --
    -- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
    --     AWS Config delivers a configuration item as a result of a resource
    --     change.
    --
    -- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
    --     evaluation when AWS Config delivers an oversized configuration item.
    --     AWS Config may generate this notification type when a resource
    --     changes and the notification exceeds the maximum size allowed by
    --     Amazon SNS.
    --
    -- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
    --     frequency specified for @MaximumExecutionFrequency@.
    --
    -- -   @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic
    --     evaluation when AWS Config delivers a configuration snapshot.
    --
    -- If you want your custom rule to be triggered by configuration changes,
    -- specify two SourceDetail objects, one for
    -- @ConfigurationItemChangeNotification@ and one for
    -- @OversizedConfigurationItemChangeNotification@.
    messageType :: Prelude.Maybe MessageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSource', 'sourceDetail_eventSource' - The source of the event, such as an AWS service, that triggers AWS
-- Config to evaluate your AWS resources.
--
-- 'maximumExecutionFrequency', 'sourceDetail_maximumExecutionFrequency' - The frequency at which you want AWS Config to run evaluations for a
-- custom rule with a periodic trigger. If you specify a value for
-- @MaximumExecutionFrequency@, then @MessageType@ must use the
-- @ScheduledNotification@ value.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- Based on the valid value you choose, AWS Config runs evaluations once
-- for each valid value. For example, if you choose @Three_Hours@, AWS
-- Config runs evaluations once every three hours. In this case,
-- @Three_Hours@ is the frequency of this rule.
--
-- 'messageType', 'sourceDetail_messageType' - The type of notification that triggers AWS Config to run an evaluation
-- for a rule. You can specify the following notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
--     AWS Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
--     evaluation when AWS Config delivers an oversized configuration item.
--     AWS Config may generate this notification type when a resource
--     changes and the notification exceeds the maximum size allowed by
--     Amazon SNS.
--
-- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
--     frequency specified for @MaximumExecutionFrequency@.
--
-- -   @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic
--     evaluation when AWS Config delivers a configuration snapshot.
--
-- If you want your custom rule to be triggered by configuration changes,
-- specify two SourceDetail objects, one for
-- @ConfigurationItemChangeNotification@ and one for
-- @OversizedConfigurationItemChangeNotification@.
newSourceDetail ::
  SourceDetail
newSourceDetail =
  SourceDetail'
    { eventSource = Prelude.Nothing,
      maximumExecutionFrequency = Prelude.Nothing,
      messageType = Prelude.Nothing
    }

-- | The source of the event, such as an AWS service, that triggers AWS
-- Config to evaluate your AWS resources.
sourceDetail_eventSource :: Lens.Lens' SourceDetail (Prelude.Maybe EventSource)
sourceDetail_eventSource = Lens.lens (\SourceDetail' {eventSource} -> eventSource) (\s@SourceDetail' {} a -> s {eventSource = a} :: SourceDetail)

-- | The frequency at which you want AWS Config to run evaluations for a
-- custom rule with a periodic trigger. If you specify a value for
-- @MaximumExecutionFrequency@, then @MessageType@ must use the
-- @ScheduledNotification@ value.
--
-- By default, rules with a periodic trigger are evaluated every 24 hours.
-- To change the frequency, specify a valid value for the
-- @MaximumExecutionFrequency@ parameter.
--
-- Based on the valid value you choose, AWS Config runs evaluations once
-- for each valid value. For example, if you choose @Three_Hours@, AWS
-- Config runs evaluations once every three hours. In this case,
-- @Three_Hours@ is the frequency of this rule.
sourceDetail_maximumExecutionFrequency :: Lens.Lens' SourceDetail (Prelude.Maybe MaximumExecutionFrequency)
sourceDetail_maximumExecutionFrequency = Lens.lens (\SourceDetail' {maximumExecutionFrequency} -> maximumExecutionFrequency) (\s@SourceDetail' {} a -> s {maximumExecutionFrequency = a} :: SourceDetail)

-- | The type of notification that triggers AWS Config to run an evaluation
-- for a rule. You can specify the following notification types:
--
-- -   @ConfigurationItemChangeNotification@ - Triggers an evaluation when
--     AWS Config delivers a configuration item as a result of a resource
--     change.
--
-- -   @OversizedConfigurationItemChangeNotification@ - Triggers an
--     evaluation when AWS Config delivers an oversized configuration item.
--     AWS Config may generate this notification type when a resource
--     changes and the notification exceeds the maximum size allowed by
--     Amazon SNS.
--
-- -   @ScheduledNotification@ - Triggers a periodic evaluation at the
--     frequency specified for @MaximumExecutionFrequency@.
--
-- -   @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic
--     evaluation when AWS Config delivers a configuration snapshot.
--
-- If you want your custom rule to be triggered by configuration changes,
-- specify two SourceDetail objects, one for
-- @ConfigurationItemChangeNotification@ and one for
-- @OversizedConfigurationItemChangeNotification@.
sourceDetail_messageType :: Lens.Lens' SourceDetail (Prelude.Maybe MessageType)
sourceDetail_messageType = Lens.lens (\SourceDetail' {messageType} -> messageType) (\s@SourceDetail' {} a -> s {messageType = a} :: SourceDetail)

instance Prelude.FromJSON SourceDetail where
  parseJSON =
    Prelude.withObject
      "SourceDetail"
      ( \x ->
          SourceDetail'
            Prelude.<$> (x Prelude..:? "EventSource")
            Prelude.<*> (x Prelude..:? "MaximumExecutionFrequency")
            Prelude.<*> (x Prelude..:? "MessageType")
      )

instance Prelude.Hashable SourceDetail

instance Prelude.NFData SourceDetail

instance Prelude.ToJSON SourceDetail where
  toJSON SourceDetail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EventSource" Prelude..=) Prelude.<$> eventSource,
            ("MaximumExecutionFrequency" Prelude..=)
              Prelude.<$> maximumExecutionFrequency,
            ("MessageType" Prelude..=) Prelude.<$> messageType
          ]
      )
