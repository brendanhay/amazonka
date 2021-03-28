{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TriggerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TriggerConfig
  ( TriggerConfig (..)
  -- * Smart constructor
  , mkTriggerConfig
  -- * Lenses
  , tcTriggerEvents
  , tcTriggerName
  , tcTriggerTargetArn
  ) where

import qualified Network.AWS.CodeDeploy.Types.TriggerEventType as Types
import qualified Network.AWS.CodeDeploy.Types.TriggerName as Types
import qualified Network.AWS.CodeDeploy.Types.TriggerTargetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about notification triggers for the deployment group.
--
-- /See:/ 'mkTriggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { triggerEvents :: Core.Maybe [Types.TriggerEventType]
    -- ^ The event type or types for which notifications are triggered.
  , triggerName :: Core.Maybe Types.TriggerName
    -- ^ The name of the notification trigger.
  , triggerTargetArn :: Core.Maybe Types.TriggerTargetArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TriggerConfig' value with any optional fields omitted.
mkTriggerConfig
    :: TriggerConfig
mkTriggerConfig
  = TriggerConfig'{triggerEvents = Core.Nothing,
                   triggerName = Core.Nothing, triggerTargetArn = Core.Nothing}

-- | The event type or types for which notifications are triggered.
--
-- /Note:/ Consider using 'triggerEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTriggerEvents :: Lens.Lens' TriggerConfig (Core.Maybe [Types.TriggerEventType])
tcTriggerEvents = Lens.field @"triggerEvents"
{-# INLINEABLE tcTriggerEvents #-}
{-# DEPRECATED triggerEvents "Use generic-lens or generic-optics with 'triggerEvents' instead"  #-}

-- | The name of the notification trigger.
--
-- /Note:/ Consider using 'triggerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTriggerName :: Lens.Lens' TriggerConfig (Core.Maybe Types.TriggerName)
tcTriggerName = Lens.field @"triggerName"
{-# INLINEABLE tcTriggerName #-}
{-# DEPRECATED triggerName "Use generic-lens or generic-optics with 'triggerName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service topic through which notifications about deployment or instance events are sent.
--
-- /Note:/ Consider using 'triggerTargetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTriggerTargetArn :: Lens.Lens' TriggerConfig (Core.Maybe Types.TriggerTargetArn)
tcTriggerTargetArn = Lens.field @"triggerTargetArn"
{-# INLINEABLE tcTriggerTargetArn #-}
{-# DEPRECATED triggerTargetArn "Use generic-lens or generic-optics with 'triggerTargetArn' instead"  #-}

instance Core.FromJSON TriggerConfig where
        toJSON TriggerConfig{..}
          = Core.object
              (Core.catMaybes
                 [("triggerEvents" Core..=) Core.<$> triggerEvents,
                  ("triggerName" Core..=) Core.<$> triggerName,
                  ("triggerTargetArn" Core..=) Core.<$> triggerTargetArn])

instance Core.FromJSON TriggerConfig where
        parseJSON
          = Core.withObject "TriggerConfig" Core.$
              \ x ->
                TriggerConfig' Core.<$>
                  (x Core..:? "triggerEvents") Core.<*> x Core..:? "triggerName"
                    Core.<*> x Core..:? "triggerTargetArn"
