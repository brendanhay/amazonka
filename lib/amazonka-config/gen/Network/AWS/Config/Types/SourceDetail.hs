{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.SourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.SourceDetail
  ( SourceDetail (..),

    -- * Smart constructor
    mkSourceDetail,

    -- * Lenses
    sdMessageType,
    sdMaximumExecutionFrequency,
    sdEventSource,
  )
where

import Network.AWS.Config.Types.EventSource
import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Config.Types.MessageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for @SourceDetail@ only for custom rules.
--
-- /See:/ 'mkSourceDetail' smart constructor.
data SourceDetail = SourceDetail'
  { messageType ::
      Lude.Maybe MessageType,
    maximumExecutionFrequency :: Lude.Maybe MaximumExecutionFrequency,
    eventSource :: Lude.Maybe EventSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceDetail' with the minimum fields required to make a request.
--
-- * 'eventSource' - The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
-- * 'maximumExecutionFrequency' - The frequency at which you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
-- * 'messageType' - The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:
--
--
--     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.
--
--
--     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.
--
--
--     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .
--
--
--     * @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic evaluation when AWS Config delivers a configuration snapshot.
--
--
-- If you want your custom rule to be triggered by configuration changes, specify two SourceDetail objects, one for @ConfigurationItemChangeNotification@ and one for @OversizedConfigurationItemChangeNotification@ .
mkSourceDetail ::
  SourceDetail
mkSourceDetail =
  SourceDetail'
    { messageType = Lude.Nothing,
      maximumExecutionFrequency = Lude.Nothing,
      eventSource = Lude.Nothing
    }

-- | The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:
--
--
--     * @ConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers a configuration item as a result of a resource change.
--
--
--     * @OversizedConfigurationItemChangeNotification@ - Triggers an evaluation when AWS Config delivers an oversized configuration item. AWS Config may generate this notification type when a resource changes and the notification exceeds the maximum size allowed by Amazon SNS.
--
--
--     * @ScheduledNotification@ - Triggers a periodic evaluation at the frequency specified for @MaximumExecutionFrequency@ .
--
--
--     * @ConfigurationSnapshotDeliveryCompleted@ - Triggers a periodic evaluation when AWS Config delivers a configuration snapshot.
--
--
-- If you want your custom rule to be triggered by configuration changes, specify two SourceDetail objects, one for @ConfigurationItemChangeNotification@ and one for @OversizedConfigurationItemChangeNotification@ .
--
-- /Note:/ Consider using 'messageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMessageType :: Lens.Lens' SourceDetail (Lude.Maybe MessageType)
sdMessageType = Lens.lens (messageType :: SourceDetail -> Lude.Maybe MessageType) (\s a -> s {messageType = a} :: SourceDetail)
{-# DEPRECATED sdMessageType "Use generic-lens or generic-optics with 'messageType' instead." #-}

-- | The frequency at which you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMaximumExecutionFrequency :: Lens.Lens' SourceDetail (Lude.Maybe MaximumExecutionFrequency)
sdMaximumExecutionFrequency = Lens.lens (maximumExecutionFrequency :: SourceDetail -> Lude.Maybe MaximumExecutionFrequency) (\s a -> s {maximumExecutionFrequency = a} :: SourceDetail)
{-# DEPRECATED sdMaximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead." #-}

-- | The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEventSource :: Lens.Lens' SourceDetail (Lude.Maybe EventSource)
sdEventSource = Lens.lens (eventSource :: SourceDetail -> Lude.Maybe EventSource) (\s a -> s {eventSource = a} :: SourceDetail)
{-# DEPRECATED sdEventSource "Use generic-lens or generic-optics with 'eventSource' instead." #-}

instance Lude.FromJSON SourceDetail where
  parseJSON =
    Lude.withObject
      "SourceDetail"
      ( \x ->
          SourceDetail'
            Lude.<$> (x Lude..:? "MessageType")
            Lude.<*> (x Lude..:? "MaximumExecutionFrequency")
            Lude.<*> (x Lude..:? "EventSource")
      )

instance Lude.ToJSON SourceDetail where
  toJSON SourceDetail' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MessageType" Lude..=) Lude.<$> messageType,
            ("MaximumExecutionFrequency" Lude..=)
              Lude.<$> maximumExecutionFrequency,
            ("EventSource" Lude..=) Lude.<$> eventSource
          ]
      )
