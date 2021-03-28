{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more custom attributes from an Amazon ECS resource.
module Network.AWS.ECS.DeleteAttributes
    (
    -- * Creating a request
      DeleteAttributes (..)
    , mkDeleteAttributes
    -- ** Request lenses
    , daAttributes
    , daCluster

    -- * Destructuring the response
    , DeleteAttributesResponse (..)
    , mkDeleteAttributesResponse
    -- ** Response lenses
    , darrsAttributes
    , darrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { attributes :: [Types.Attribute]
    -- ^ The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAttributes' value with any optional fields omitted.
mkDeleteAttributes
    :: DeleteAttributes
mkDeleteAttributes
  = DeleteAttributes'{attributes = Core.mempty,
                      cluster = Core.Nothing}

-- | The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttributes :: Lens.Lens' DeleteAttributes [Types.Attribute]
daAttributes = Lens.field @"attributes"
{-# INLINEABLE daAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daCluster :: Lens.Lens' DeleteAttributes (Core.Maybe Core.Text)
daCluster = Lens.field @"cluster"
{-# INLINEABLE daCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

instance Core.ToQuery DeleteAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteAttributes where
        toHeaders DeleteAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.DeleteAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteAttributes where
        toJSON DeleteAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("attributes" Core..= attributes),
                  ("cluster" Core..=) Core.<$> cluster])

instance Core.AWSRequest DeleteAttributes where
        type Rs DeleteAttributes = DeleteAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteAttributesResponse' Core.<$>
                   (x Core..:? "attributes") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  { attributes :: Core.Maybe [Types.Attribute]
    -- ^ A list of attribute objects that were successfully deleted from your resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAttributesResponse' value with any optional fields omitted.
mkDeleteAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteAttributesResponse
mkDeleteAttributesResponse responseStatus
  = DeleteAttributesResponse'{attributes = Core.Nothing,
                              responseStatus}

-- | A list of attribute objects that were successfully deleted from your resource.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAttributes :: Lens.Lens' DeleteAttributesResponse (Core.Maybe [Types.Attribute])
darrsAttributes = Lens.field @"attributes"
{-# INLINEABLE darrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAttributesResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
