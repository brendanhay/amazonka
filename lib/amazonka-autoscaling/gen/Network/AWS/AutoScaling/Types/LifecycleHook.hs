{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LifecycleHook
  ( LifecycleHook (..)
  -- * Smart constructor
  , mkLifecycleHook
  -- * Lenses
  , lhAutoScalingGroupName
  , lhDefaultResult
  , lhGlobalTimeout
  , lhHeartbeatTimeout
  , lhLifecycleHookName
  , lhLifecycleTransition
  , lhNotificationMetadata
  , lhNotificationTargetARN
  , lhRoleARN
  ) where

import qualified Network.AWS.AutoScaling.Types.AsciiStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.LifecycleActionResult as Types
import qualified Network.AWS.AutoScaling.Types.LifecycleTransition as Types
import qualified Network.AWS.AutoScaling.Types.ResourceName as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen1023 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a lifecycle hook, which tells Amazon EC2 Auto Scaling that you want to perform an action whenever it launches instances or terminates instances.
--
-- /See:/ 'mkLifecycleHook' smart constructor.
data LifecycleHook = LifecycleHook'
  { autoScalingGroupName :: Core.Maybe Types.ResourceName
    -- ^ The name of the Auto Scaling group for the lifecycle hook.
  , defaultResult :: Core.Maybe Types.LifecycleActionResult
    -- ^ Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
  , globalTimeout :: Core.Maybe Core.Int
    -- ^ The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
  , heartbeatTimeout :: Core.Maybe Core.Int
    -- ^ The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
  , lifecycleHookName :: Core.Maybe Types.AsciiStringMaxLen255
    -- ^ The name of the lifecycle hook.
  , lifecycleTransition :: Core.Maybe Types.LifecycleTransition
    -- ^ The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
  , notificationMetadata :: Core.Maybe Types.XmlStringMaxLen1023
    -- ^ Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
  , notificationTargetARN :: Core.Maybe Types.ResourceName
    -- ^ The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
  , roleARN :: Core.Maybe Types.ResourceName
    -- ^ The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LifecycleHook' value with any optional fields omitted.
mkLifecycleHook
    :: LifecycleHook
mkLifecycleHook
  = LifecycleHook'{autoScalingGroupName = Core.Nothing,
                   defaultResult = Core.Nothing, globalTimeout = Core.Nothing,
                   heartbeatTimeout = Core.Nothing, lifecycleHookName = Core.Nothing,
                   lifecycleTransition = Core.Nothing,
                   notificationMetadata = Core.Nothing,
                   notificationTargetARN = Core.Nothing, roleARN = Core.Nothing}

-- | The name of the Auto Scaling group for the lifecycle hook.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhAutoScalingGroupName :: Lens.Lens' LifecycleHook (Core.Maybe Types.ResourceName)
lhAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE lhAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The possible values are @CONTINUE@ and @ABANDON@ .
--
-- /Note:/ Consider using 'defaultResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhDefaultResult :: Lens.Lens' LifecycleHook (Core.Maybe Types.LifecycleActionResult)
lhDefaultResult = Lens.field @"defaultResult"
{-# INLINEABLE lhDefaultResult #-}
{-# DEPRECATED defaultResult "Use generic-lens or generic-optics with 'defaultResult' instead"  #-}

-- | The maximum time, in seconds, that an instance can remain in a @Pending:Wait@ or @Terminating:Wait@ state. The maximum is 172800 seconds (48 hours) or 100 times @HeartbeatTimeout@ , whichever is smaller.
--
-- /Note:/ Consider using 'globalTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhGlobalTimeout :: Lens.Lens' LifecycleHook (Core.Maybe Core.Int)
lhGlobalTimeout = Lens.field @"globalTimeout"
{-# INLINEABLE lhGlobalTimeout #-}
{-# DEPRECATED globalTimeout "Use generic-lens or generic-optics with 'globalTimeout' instead"  #-}

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out. If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter.
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhHeartbeatTimeout :: Lens.Lens' LifecycleHook (Core.Maybe Core.Int)
lhHeartbeatTimeout = Lens.field @"heartbeatTimeout"
{-# INLINEABLE lhHeartbeatTimeout #-}
{-# DEPRECATED heartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead"  #-}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhLifecycleHookName :: Lens.Lens' LifecycleHook (Core.Maybe Types.AsciiStringMaxLen255)
lhLifecycleHookName = Lens.field @"lifecycleHookName"
{-# INLINEABLE lhLifecycleHookName #-}
{-# DEPRECATED lifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead"  #-}

-- | The state of the EC2 instance to which to attach the lifecycle hook. The following are possible values:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
--
-- /Note:/ Consider using 'lifecycleTransition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhLifecycleTransition :: Lens.Lens' LifecycleHook (Core.Maybe Types.LifecycleTransition)
lhLifecycleTransition = Lens.field @"lifecycleTransition"
{-# INLINEABLE lhLifecycleTransition #-}
{-# DEPRECATED lifecycleTransition "Use generic-lens or generic-optics with 'lifecycleTransition' instead"  #-}

-- | Additional information that is included any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- /Note:/ Consider using 'notificationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhNotificationMetadata :: Lens.Lens' LifecycleHook (Core.Maybe Types.XmlStringMaxLen1023)
lhNotificationMetadata = Lens.field @"notificationMetadata"
{-# INLINEABLE lhNotificationMetadata #-}
{-# DEPRECATED notificationMetadata "Use generic-lens or generic-optics with 'notificationMetadata' instead"  #-}

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- /Note:/ Consider using 'notificationTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhNotificationTargetARN :: Lens.Lens' LifecycleHook (Core.Maybe Types.ResourceName)
lhNotificationTargetARN = Lens.field @"notificationTargetARN"
{-# INLINEABLE lhNotificationTargetARN #-}
{-# DEPRECATED notificationTargetARN "Use generic-lens or generic-optics with 'notificationTargetARN' instead"  #-}

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhRoleARN :: Lens.Lens' LifecycleHook (Core.Maybe Types.ResourceName)
lhRoleARN = Lens.field @"roleARN"
{-# INLINEABLE lhRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromXML LifecycleHook where
        parseXML x
          = LifecycleHook' Core.<$>
              (x Core..@? "AutoScalingGroupName") Core.<*>
                x Core..@? "DefaultResult"
                Core.<*> x Core..@? "GlobalTimeout"
                Core.<*> x Core..@? "HeartbeatTimeout"
                Core.<*> x Core..@? "LifecycleHookName"
                Core.<*> x Core..@? "LifecycleTransition"
                Core.<*> x Core..@? "NotificationMetadata"
                Core.<*> x Core..@? "NotificationTargetARN"
                Core.<*> x Core..@? "RoleARN"
