{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.SourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.SourceDetail
  ( SourceDetail (..)
  -- * Smart constructor
  , mkSourceDetail
  -- * Lenses
  , sdEventSource
  , sdMaximumExecutionFrequency
  , sdMessageType
  ) where

import qualified Network.AWS.Config.Types.EventSource as Types
import qualified Network.AWS.Config.Types.MaximumExecutionFrequency as Types
import qualified Network.AWS.Config.Types.MessageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for @SourceDetail@ only for custom rules. 
--
-- /See:/ 'mkSourceDetail' smart constructor.
data SourceDetail = SourceDetail'
  { eventSource :: Core.Maybe Types.EventSource
    -- ^ The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
  , maximumExecutionFrequency :: Core.Maybe Types.MaximumExecutionFrequency
    -- ^ The frequency at which you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
  , messageType :: Core.Maybe Types.MessageType
    -- ^ The type of notification that triggers AWS Config to run an evaluation for a rule. You can specify the following notification types:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceDetail' value with any optional fields omitted.
mkSourceDetail
    :: SourceDetail
mkSourceDetail
  = SourceDetail'{eventSource = Core.Nothing,
                  maximumExecutionFrequency = Core.Nothing,
                  messageType = Core.Nothing}

-- | The source of the event, such as an AWS service, that triggers AWS Config to evaluate your AWS resources.
--
-- /Note:/ Consider using 'eventSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEventSource :: Lens.Lens' SourceDetail (Core.Maybe Types.EventSource)
sdEventSource = Lens.field @"eventSource"
{-# INLINEABLE sdEventSource #-}
{-# DEPRECATED eventSource "Use generic-lens or generic-optics with 'eventSource' instead"  #-}

-- | The frequency at which you want AWS Config to run evaluations for a custom rule with a periodic trigger. If you specify a value for @MaximumExecutionFrequency@ , then @MessageType@ must use the @ScheduledNotification@ value.
--
-- /Note:/ Consider using 'maximumExecutionFrequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMaximumExecutionFrequency :: Lens.Lens' SourceDetail (Core.Maybe Types.MaximumExecutionFrequency)
sdMaximumExecutionFrequency = Lens.field @"maximumExecutionFrequency"
{-# INLINEABLE sdMaximumExecutionFrequency #-}
{-# DEPRECATED maximumExecutionFrequency "Use generic-lens or generic-optics with 'maximumExecutionFrequency' instead"  #-}

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
sdMessageType :: Lens.Lens' SourceDetail (Core.Maybe Types.MessageType)
sdMessageType = Lens.field @"messageType"
{-# INLINEABLE sdMessageType #-}
{-# DEPRECATED messageType "Use generic-lens or generic-optics with 'messageType' instead"  #-}

instance Core.FromJSON SourceDetail where
        toJSON SourceDetail{..}
          = Core.object
              (Core.catMaybes
                 [("EventSource" Core..=) Core.<$> eventSource,
                  ("MaximumExecutionFrequency" Core..=) Core.<$>
                    maximumExecutionFrequency,
                  ("MessageType" Core..=) Core.<$> messageType])

instance Core.FromJSON SourceDetail where
        parseJSON
          = Core.withObject "SourceDetail" Core.$
              \ x ->
                SourceDetail' Core.<$>
                  (x Core..:? "EventSource") Core.<*>
                    x Core..:? "MaximumExecutionFrequency"
                    Core.<*> x Core..:? "MessageType"
