{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes load-based auto scaling configurations for specified layers.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeLoadBasedAutoScaling
  ( -- * Creating a request
    DescribeLoadBasedAutoScaling (..),
    mkDescribeLoadBasedAutoScaling,

    -- ** Request lenses
    dlbasLayerIds,

    -- * Destructuring the response
    DescribeLoadBasedAutoScalingResponse (..),
    mkDescribeLoadBasedAutoScalingResponse,

    -- ** Response lenses
    dlbasrrsLoadBasedAutoScalingConfigurations,
    dlbasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLoadBasedAutoScaling' smart constructor.
newtype DescribeLoadBasedAutoScaling = DescribeLoadBasedAutoScaling'
  { -- | An array of layer IDs.
    layerIds :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBasedAutoScaling' value with any optional fields omitted.
mkDescribeLoadBasedAutoScaling ::
  DescribeLoadBasedAutoScaling
mkDescribeLoadBasedAutoScaling =
  DescribeLoadBasedAutoScaling' {layerIds = Core.mempty}

-- | An array of layer IDs.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbasLayerIds :: Lens.Lens' DescribeLoadBasedAutoScaling [Types.String]
dlbasLayerIds = Lens.field @"layerIds"
{-# DEPRECATED dlbasLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

instance Core.FromJSON DescribeLoadBasedAutoScaling where
  toJSON DescribeLoadBasedAutoScaling {..} =
    Core.object
      (Core.catMaybes [Core.Just ("LayerIds" Core..= layerIds)])

instance Core.AWSRequest DescribeLoadBasedAutoScaling where
  type
    Rs DescribeLoadBasedAutoScaling =
      DescribeLoadBasedAutoScalingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "OpsWorks_20130218.DescribeLoadBasedAutoScaling")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoadBasedAutoScalingResponse'
            Core.<$> (x Core..:? "LoadBasedAutoScalingConfigurations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeLoadBasedAutoScaling@ request.
--
-- /See:/ 'mkDescribeLoadBasedAutoScalingResponse' smart constructor.
data DescribeLoadBasedAutoScalingResponse = DescribeLoadBasedAutoScalingResponse'
  { -- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
    loadBasedAutoScalingConfigurations :: Core.Maybe [Types.LoadBasedAutoScalingConfiguration],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoadBasedAutoScalingResponse' value with any optional fields omitted.
mkDescribeLoadBasedAutoScalingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLoadBasedAutoScalingResponse
mkDescribeLoadBasedAutoScalingResponse responseStatus =
  DescribeLoadBasedAutoScalingResponse'
    { loadBasedAutoScalingConfigurations =
        Core.Nothing,
      responseStatus
    }

-- | An array of @LoadBasedAutoScalingConfiguration@ objects that describe each layer's configuration.
--
-- /Note:/ Consider using 'loadBasedAutoScalingConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbasrrsLoadBasedAutoScalingConfigurations :: Lens.Lens' DescribeLoadBasedAutoScalingResponse (Core.Maybe [Types.LoadBasedAutoScalingConfiguration])
dlbasrrsLoadBasedAutoScalingConfigurations = Lens.field @"loadBasedAutoScalingConfigurations"
{-# DEPRECATED dlbasrrsLoadBasedAutoScalingConfigurations "Use generic-lens or generic-optics with 'loadBasedAutoScalingConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbasrrsResponseStatus :: Lens.Lens' DescribeLoadBasedAutoScalingResponse Core.Int
dlbasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlbasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
