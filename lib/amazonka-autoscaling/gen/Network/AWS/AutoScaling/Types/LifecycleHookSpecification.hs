{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.LifecycleHookSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScaling.Types.LifecycleHookSpecification
  ( LifecycleHookSpecification (..)
  -- * Smart constructor
  , mkLifecycleHookSpecification
  -- * Lenses
  , lhsLifecycleHookName
  , lhsLifecycleTransition
  , lhsDefaultResult
  , lhsHeartbeatTimeout
  , lhsNotificationMetadata
  , lhsNotificationTargetARN
  , lhsRoleARN
  ) where

import qualified Network.AWS.AutoScaling.Types.AsciiStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.LifecycleActionResult as Types
import qualified Network.AWS.AutoScaling.Types.LifecycleTransition as Types
import qualified Network.AWS.AutoScaling.Types.NotificationTargetARN as Types
import qualified Network.AWS.AutoScaling.Types.ResourceName as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen1023 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes information used to specify a lifecycle hook for an Auto Scaling group.
--
-- A lifecycle hook tells Amazon EC2 Auto Scaling to perform an action on an instance when the instance launches (before it is put into service) or as the instance terminates (before it is fully terminated).
-- This step is a part of the procedure for creating a lifecycle hook for an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Amazon EC2 Auto Scaling launches or terminates instances.
--
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Amazon EC2 Auto Scaling to publish lifecycle notifications to the target.
--
--
--     * __Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.__ 
--
--
--     * If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.
--
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/lifecycle-hooks.html Amazon EC2 Auto Scaling lifecycle hooks> in the /Amazon EC2 Auto Scaling User Guide/ .
--
-- /See:/ 'mkLifecycleHookSpecification' smart constructor.
data LifecycleHookSpecification = LifecycleHookSpecification'
  { lifecycleHookName :: Types.AsciiStringMaxLen255
    -- ^ The name of the lifecycle hook.
  , lifecycleTransition :: Types.LifecycleTransition
    -- ^ The state of the EC2 instance to which you want to attach the lifecycle hook. The valid values are:
--
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
--
--
  , defaultResult :: Core.Maybe Types.LifecycleActionResult
    -- ^ Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @ABANDON@ .
  , heartbeatTimeout :: Core.Maybe Core.Int
    -- ^ The maximum time, in seconds, that can elapse before the lifecycle hook times out.
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
  , notificationMetadata :: Core.Maybe Types.XmlStringMaxLen1023
    -- ^ Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
  , notificationTargetARN :: Core.Maybe Types.NotificationTargetARN
    -- ^ The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
  , roleARN :: Core.Maybe Types.ResourceName
    -- ^ The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LifecycleHookSpecification' value with any optional fields omitted.
mkLifecycleHookSpecification
    :: Types.AsciiStringMaxLen255 -- ^ 'lifecycleHookName'
    -> Types.LifecycleTransition -- ^ 'lifecycleTransition'
    -> LifecycleHookSpecification
mkLifecycleHookSpecification lifecycleHookName lifecycleTransition
  = LifecycleHookSpecification'{lifecycleHookName,
                                lifecycleTransition, defaultResult = Core.Nothing,
                                heartbeatTimeout = Core.Nothing,
                                notificationMetadata = Core.Nothing,
                                notificationTargetARN = Core.Nothing, roleARN = Core.Nothing}

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsLifecycleHookName :: Lens.Lens' LifecycleHookSpecification Types.AsciiStringMaxLen255
lhsLifecycleHookName = Lens.field @"lifecycleHookName"
{-# INLINEABLE lhsLifecycleHookName #-}
{-# DEPRECATED lifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead"  #-}

-- | The state of the EC2 instance to which you want to attach the lifecycle hook. The valid values are:
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
lhsLifecycleTransition :: Lens.Lens' LifecycleHookSpecification Types.LifecycleTransition
lhsLifecycleTransition = Lens.field @"lifecycleTransition"
{-# INLINEABLE lhsLifecycleTransition #-}
{-# DEPRECATED lifecycleTransition "Use generic-lens or generic-optics with 'lifecycleTransition' instead"  #-}

-- | Defines the action the Auto Scaling group should take when the lifecycle hook timeout elapses or if an unexpected failure occurs. The valid values are @CONTINUE@ and @ABANDON@ . The default value is @ABANDON@ .
--
-- /Note:/ Consider using 'defaultResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsDefaultResult :: Lens.Lens' LifecycleHookSpecification (Core.Maybe Types.LifecycleActionResult)
lhsDefaultResult = Lens.field @"defaultResult"
{-# INLINEABLE lhsDefaultResult #-}
{-# DEPRECATED defaultResult "Use generic-lens or generic-optics with 'defaultResult' instead"  #-}

-- | The maximum time, in seconds, that can elapse before the lifecycle hook times out.
--
-- If the lifecycle hook times out, Amazon EC2 Auto Scaling performs the action that you specified in the @DefaultResult@ parameter. You can prevent the lifecycle hook from timing out by calling 'RecordLifecycleActionHeartbeat' .
--
-- /Note:/ Consider using 'heartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsHeartbeatTimeout :: Lens.Lens' LifecycleHookSpecification (Core.Maybe Core.Int)
lhsHeartbeatTimeout = Lens.field @"heartbeatTimeout"
{-# INLINEABLE lhsHeartbeatTimeout #-}
{-# DEPRECATED heartbeatTimeout "Use generic-lens or generic-optics with 'heartbeatTimeout' instead"  #-}

-- | Additional information that you want to include any time Amazon EC2 Auto Scaling sends a message to the notification target.
--
-- /Note:/ Consider using 'notificationMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsNotificationMetadata :: Lens.Lens' LifecycleHookSpecification (Core.Maybe Types.XmlStringMaxLen1023)
lhsNotificationMetadata = Lens.field @"notificationMetadata"
{-# INLINEABLE lhsNotificationMetadata #-}
{-# DEPRECATED notificationMetadata "Use generic-lens or generic-optics with 'notificationMetadata' instead"  #-}

-- | The ARN of the target that Amazon EC2 Auto Scaling sends notifications to when an instance is in the transition state for the lifecycle hook. The notification target can be either an SQS queue or an SNS topic.
--
-- /Note:/ Consider using 'notificationTargetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsNotificationTargetARN :: Lens.Lens' LifecycleHookSpecification (Core.Maybe Types.NotificationTargetARN)
lhsNotificationTargetARN = Lens.field @"notificationTargetARN"
{-# INLINEABLE lhsNotificationTargetARN #-}
{-# DEPRECATED notificationTargetARN "Use generic-lens or generic-optics with 'notificationTargetARN' instead"  #-}

-- | The ARN of the IAM role that allows the Auto Scaling group to publish to the specified notification target, for example, an Amazon SNS topic or an Amazon SQS queue.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhsRoleARN :: Lens.Lens' LifecycleHookSpecification (Core.Maybe Types.ResourceName)
lhsRoleARN = Lens.field @"roleARN"
{-# INLINEABLE lhsRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.ToQuery LifecycleHookSpecification where
        toQuery LifecycleHookSpecification{..}
          = Core.toQueryPair "LifecycleHookName" lifecycleHookName Core.<>
              Core.toQueryPair "LifecycleTransition" lifecycleTransition
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DefaultResult")
                defaultResult
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HeartbeatTimeout")
                heartbeatTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NotificationMetadata")
                notificationMetadata
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NotificationTargetARN")
                notificationTargetARN
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "RoleARN") roleARN
