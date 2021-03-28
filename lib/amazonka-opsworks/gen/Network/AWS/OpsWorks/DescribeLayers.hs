{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeLayers (..)
    , mkDescribeLayers
    -- ** Request lenses
    , dlLayerIds
    , dlStackId

    -- * Destructuring the response
    , DescribeLayersResponse (..)
    , mkDescribeLayersResponse
    -- ** Response lenses
    , dlrrsLayers
    , dlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeLayers' smart constructor.
data DescribeLayers = DescribeLayers'
  { layerIds :: Core.Maybe [Core.Text]
    -- ^ An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
  , stackId :: Core.Maybe Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLayers' value with any optional fields omitted.
mkDescribeLayers
    :: DescribeLayers
mkDescribeLayers
  = DescribeLayers'{layerIds = Core.Nothing, stackId = Core.Nothing}

-- | An array of layer IDs that specify the layers to be described. If you omit this parameter, @DescribeLayers@ returns a description of every layer in the specified stack.
--
-- /Note:/ Consider using 'layerIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLayerIds :: Lens.Lens' DescribeLayers (Core.Maybe [Core.Text])
dlLayerIds = Lens.field @"layerIds"
{-# INLINEABLE dlLayerIds #-}
{-# DEPRECATED layerIds "Use generic-lens or generic-optics with 'layerIds' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlStackId :: Lens.Lens' DescribeLayers (Core.Maybe Core.Text)
dlStackId = Lens.field @"stackId"
{-# INLINEABLE dlStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery DescribeLayers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeLayers where
        toHeaders DescribeLayers{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.DescribeLayers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeLayers where
        toJSON DescribeLayers{..}
          = Core.object
              (Core.catMaybes
                 [("LayerIds" Core..=) Core.<$> layerIds,
                  ("StackId" Core..=) Core.<$> stackId])

instance Core.AWSRequest DescribeLayers where
        type Rs DescribeLayers = DescribeLayersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeLayersResponse' Core.<$>
                   (x Core..:? "Layers") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @DescribeLayers@ request.
--
-- /See:/ 'mkDescribeLayersResponse' smart constructor.
data DescribeLayersResponse = DescribeLayersResponse'
  { layers :: Core.Maybe [Types.Layer]
    -- ^ An array of @Layer@ objects that describe the layers.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLayersResponse' value with any optional fields omitted.
mkDescribeLayersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeLayersResponse
mkDescribeLayersResponse responseStatus
  = DescribeLayersResponse'{layers = Core.Nothing, responseStatus}

-- | An array of @Layer@ objects that describe the layers.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsLayers :: Lens.Lens' DescribeLayersResponse (Core.Maybe [Types.Layer])
dlrrsLayers = Lens.field @"layers"
{-# INLINEABLE dlrrsLayers #-}
{-# DEPRECATED layers "Use generic-lens or generic-optics with 'layers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DescribeLayersResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
