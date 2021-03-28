{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the time-based auto scaling configuration for a specified instance. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
    (
    -- * Creating a request
      SetTimeBasedAutoScaling (..)
    , mkSetTimeBasedAutoScaling
    -- ** Request lenses
    , stbasInstanceId
    , stbasAutoScalingSchedule

    -- * Destructuring the response
    , SetTimeBasedAutoScalingResponse (..)
    , mkSetTimeBasedAutoScalingResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetTimeBasedAutoScaling' smart constructor.
data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling'
  { instanceId :: Core.Text
    -- ^ The instance ID.
  , autoScalingSchedule :: Core.Maybe Types.WeeklyAutoScalingSchedule
    -- ^ An @AutoScalingSchedule@ with the instance schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTimeBasedAutoScaling' value with any optional fields omitted.
mkSetTimeBasedAutoScaling
    :: Core.Text -- ^ 'instanceId'
    -> SetTimeBasedAutoScaling
mkSetTimeBasedAutoScaling instanceId
  = SetTimeBasedAutoScaling'{instanceId,
                             autoScalingSchedule = Core.Nothing}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stbasInstanceId :: Lens.Lens' SetTimeBasedAutoScaling Core.Text
stbasInstanceId = Lens.field @"instanceId"
{-# INLINEABLE stbasInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | An @AutoScalingSchedule@ with the instance schedule.
--
-- /Note:/ Consider using 'autoScalingSchedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stbasAutoScalingSchedule :: Lens.Lens' SetTimeBasedAutoScaling (Core.Maybe Types.WeeklyAutoScalingSchedule)
stbasAutoScalingSchedule = Lens.field @"autoScalingSchedule"
{-# INLINEABLE stbasAutoScalingSchedule #-}
{-# DEPRECATED autoScalingSchedule "Use generic-lens or generic-optics with 'autoScalingSchedule' instead"  #-}

instance Core.ToQuery SetTimeBasedAutoScaling where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetTimeBasedAutoScaling where
        toHeaders SetTimeBasedAutoScaling{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.SetTimeBasedAutoScaling")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SetTimeBasedAutoScaling where
        toJSON SetTimeBasedAutoScaling{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("InstanceId" Core..= instanceId),
                  ("AutoScalingSchedule" Core..=) Core.<$> autoScalingSchedule])

instance Core.AWSRequest SetTimeBasedAutoScaling where
        type Rs SetTimeBasedAutoScaling = SetTimeBasedAutoScalingResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull SetTimeBasedAutoScalingResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetTimeBasedAutoScalingResponse' smart constructor.
data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetTimeBasedAutoScalingResponse' value with any optional fields omitted.
mkSetTimeBasedAutoScalingResponse
    :: SetTimeBasedAutoScalingResponse
mkSetTimeBasedAutoScalingResponse
  = SetTimeBasedAutoScalingResponse'
