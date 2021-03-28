{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update an attribute on an Amazon ECS resource. If the attribute does not exist, it is created. If the attribute exists, its value is replaced with the specified value. To delete an attribute, use 'DeleteAttributes' . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.PutAttributes
    (
    -- * Creating a request
      PutAttributes (..)
    , mkPutAttributes
    -- ** Request lenses
    , paAttributes
    , paCluster

    -- * Destructuring the response
    , PutAttributesResponse (..)
    , mkPutAttributesResponse
    -- ** Response lenses
    , parrsAttributes
    , parrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { attributes :: [Types.Attribute]
    -- ^ The attributes to apply to your resource. You can specify up to 10 custom attributes per resource. You can specify up to 10 attributes in a single call.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to apply attributes. If you do not specify a cluster, the default cluster is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAttributes' value with any optional fields omitted.
mkPutAttributes
    :: PutAttributes
mkPutAttributes
  = PutAttributes'{attributes = Core.mempty, cluster = Core.Nothing}

-- | The attributes to apply to your resource. You can specify up to 10 custom attributes per resource. You can specify up to 10 attributes in a single call.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributes :: Lens.Lens' PutAttributes [Types.Attribute]
paAttributes = Lens.field @"attributes"
{-# INLINEABLE paAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to apply attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paCluster :: Lens.Lens' PutAttributes (Core.Maybe Core.Text)
paCluster = Lens.field @"cluster"
{-# INLINEABLE paCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

instance Core.ToQuery PutAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutAttributes where
        toHeaders PutAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.PutAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutAttributes where
        toJSON PutAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("attributes" Core..= attributes),
                  ("cluster" Core..=) Core.<$> cluster])

instance Core.AWSRequest PutAttributes where
        type Rs PutAttributes = PutAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutAttributesResponse' Core.<$>
                   (x Core..:? "attributes") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
  { attributes :: Core.Maybe [Types.Attribute]
    -- ^ The attributes applied to your resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAttributesResponse' value with any optional fields omitted.
mkPutAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutAttributesResponse
mkPutAttributesResponse responseStatus
  = PutAttributesResponse'{attributes = Core.Nothing, responseStatus}

-- | The attributes applied to your resource.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsAttributes :: Lens.Lens' PutAttributesResponse (Core.Maybe [Types.Attribute])
parrsAttributes = Lens.field @"attributes"
{-# INLINEABLE parrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parrsResponseStatus :: Lens.Lens' PutAttributesResponse Core.Int
parrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE parrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
