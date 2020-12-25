{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DescribeLayers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more layers in a specified stack.
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DescribeLayers
  ( -- * Creating a request
    DescribeLayers (..),
    mkDescribeLayers,

    -- ** Request lenses
    dlLayerIds,
    dlStackId,

    -- * Destructuring the response
    DescribeLayersResponse (..),
    mkDescribeLayersResponse,

    -- ** Response lenses
    dlrrsLayers,
    dlrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { -- | An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
    layerIds :: Core.Maybe [Types.String],
    -- | The stack ID.
    stackId :: Core.Maybe Types.StackId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLayers' value with any optional fields omitted.
mkDescribeLayers ::
  DescribeLayers
mkDescribeLayers =
  DescribeLayers' {layerIds = Core.Nothing, stackId = Core.Nothing}

-- | An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLayerIds :: Lens.Lens' DescribeLayers (Core.Maybe [Types.String])
dlLayerIds = Lens.field @"layerIds"
{-# DEPRECATED dlLayerIds "Use generic-lens or generic-optics with 'layerIds' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlStackId :: Lens.Lens' DescribeLayers (Core.Maybe Types.StackId)
dlStackId = Lens.field @"stackId"
{-# DEPRECATED dlStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Core.FromJSON DescribeLayers where
  toJSON DescribeLayers {..} =
    Core.object
      ( Core.catMaybes
          [ ("LayerIds" Core..=) Core.<$> layerIds,
            ("StackId" Core..=) Core.<$> stackId
          ]
      )

instance Core.AWSRequest DescribeLayers where
  type Rs DescribeLayers = DescribeLayersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeLayers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLayersResponse'
            Core.<$> (x Core..:? "Layers") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the response to a @DescribeLayers@ request.
--
-- /See:/ 'mkDescribeLayersResponse' smart constructor.
data DescribeLayersResponse = DescribeLayersResponse'
  { -- | An array of @Layer@ objects that describe the layers.
    layers :: Core.Maybe [Types.Layer],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLayersResponse' value with any optional fields omitted.
mkDescribeLayersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeLayersResponse
mkDescribeLayersResponse responseStatus =
  DescribeLayersResponse' {layers = Core.Nothing, responseStatus}

-- | An array of @Layer@ objects that describe the layers.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLayers :: Lens.Lens' DescribeLayersResponse (Core.Maybe [Types.Layer])
dlrrsLayers = Lens.field @"layers"
{-# DEPRECATED dlrrsLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DescribeLayersResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
