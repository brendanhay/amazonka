{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the load-based auto scaling configuration for a specified layer. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
  ( -- * Creating a request
    SetLoadBasedAutoScaling (..),
    mkSetLoadBasedAutoScaling,

    -- ** Request lenses
    slbasLayerId,
    slbasDownScaling,
    slbasEnable,
    slbasUpScaling,

    -- * Destructuring the response
    SetLoadBasedAutoScalingResponse (..),
    mkSetLoadBasedAutoScalingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetLoadBasedAutoScaling' smart constructor.
data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling'
  { -- | The layer ID.
    layerId :: Types.String,
    -- | An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
    downScaling :: Core.Maybe Types.AutoScalingThresholds,
    -- | Enables load-based auto scaling for the layer.
    enable :: Core.Maybe Core.Bool,
    -- | An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
    upScaling :: Core.Maybe Types.AutoScalingThresholds
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBasedAutoScaling' value with any optional fields omitted.
mkSetLoadBasedAutoScaling ::
  -- | 'layerId'
  Types.String ->
  SetLoadBasedAutoScaling
mkSetLoadBasedAutoScaling layerId =
  SetLoadBasedAutoScaling'
    { layerId,
      downScaling = Core.Nothing,
      enable = Core.Nothing,
      upScaling = Core.Nothing
    }

-- | The layer ID.
--
-- /Note:/ Consider using 'layerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasLayerId :: Lens.Lens' SetLoadBasedAutoScaling Types.String
slbasLayerId = Lens.field @"layerId"
{-# DEPRECATED slbasLayerId "Use generic-lens or generic-optics with 'layerId' instead." #-}

-- | An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
--
-- /Note:/ Consider using 'downScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasDownScaling :: Lens.Lens' SetLoadBasedAutoScaling (Core.Maybe Types.AutoScalingThresholds)
slbasDownScaling = Lens.field @"downScaling"
{-# DEPRECATED slbasDownScaling "Use generic-lens or generic-optics with 'downScaling' instead." #-}

-- | Enables load-based auto scaling for the layer.
--
-- /Note:/ Consider using 'enable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasEnable :: Lens.Lens' SetLoadBasedAutoScaling (Core.Maybe Core.Bool)
slbasEnable = Lens.field @"enable"
{-# DEPRECATED slbasEnable "Use generic-lens or generic-optics with 'enable' instead." #-}

-- | An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
--
-- /Note:/ Consider using 'upScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slbasUpScaling :: Lens.Lens' SetLoadBasedAutoScaling (Core.Maybe Types.AutoScalingThresholds)
slbasUpScaling = Lens.field @"upScaling"
{-# DEPRECATED slbasUpScaling "Use generic-lens or generic-optics with 'upScaling' instead." #-}

instance Core.FromJSON SetLoadBasedAutoScaling where
  toJSON SetLoadBasedAutoScaling {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("LayerId" Core..= layerId),
            ("DownScaling" Core..=) Core.<$> downScaling,
            ("Enable" Core..=) Core.<$> enable,
            ("UpScaling" Core..=) Core.<$> upScaling
          ]
      )

instance Core.AWSRequest SetLoadBasedAutoScaling where
  type Rs SetLoadBasedAutoScaling = SetLoadBasedAutoScalingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.SetLoadBasedAutoScaling")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SetLoadBasedAutoScalingResponse'

-- | /See:/ 'mkSetLoadBasedAutoScalingResponse' smart constructor.
data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBasedAutoScalingResponse' value with any optional fields omitted.
mkSetLoadBasedAutoScalingResponse ::
  SetLoadBasedAutoScalingResponse
mkSetLoadBasedAutoScalingResponse =
  SetLoadBasedAutoScalingResponse'
