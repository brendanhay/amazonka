{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.DeleteNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a namespace from the current account. If the namespace still contains one or more services, the request fails.
module Network.AWS.Route53AutoNaming.DeleteNamespace
    (
    -- * Creating a request
      DeleteNamespace (..)
    , mkDeleteNamespace
    -- ** Request lenses
    , dnId

    -- * Destructuring the response
    , DeleteNamespaceResponse (..)
    , mkDeleteNamespaceResponse
    -- ** Response lenses
    , dnrrsOperationId
    , dnrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkDeleteNamespace' smart constructor.
newtype DeleteNamespace = DeleteNamespace'
  { id :: Types.ResourceId
    -- ^ The ID of the namespace that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamespace' value with any optional fields omitted.
mkDeleteNamespace
    :: Types.ResourceId -- ^ 'id'
    -> DeleteNamespace
mkDeleteNamespace id = DeleteNamespace'{id}

-- | The ID of the namespace that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnId :: Lens.Lens' DeleteNamespace Types.ResourceId
dnId = Lens.field @"id"
{-# INLINEABLE dnId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteNamespace where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteNamespace where
        toHeaders DeleteNamespace{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.DeleteNamespace")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteNamespace where
        toJSON DeleteNamespace{..}
          = Core.object (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.AWSRequest DeleteNamespace where
        type Rs DeleteNamespace = DeleteNamespaceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteNamespaceResponse' Core.<$>
                   (x Core..:? "OperationId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNamespaceResponse' smart constructor.
data DeleteNamespaceResponse = DeleteNamespaceResponse'
  { operationId :: Core.Maybe Types.OperationId
    -- ^ A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNamespaceResponse' value with any optional fields omitted.
mkDeleteNamespaceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteNamespaceResponse
mkDeleteNamespaceResponse responseStatus
  = DeleteNamespaceResponse'{operationId = Core.Nothing,
                             responseStatus}

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsOperationId :: Lens.Lens' DeleteNamespaceResponse (Core.Maybe Types.OperationId)
dnrrsOperationId = Lens.field @"operationId"
{-# INLINEABLE dnrrsOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnrrsResponseStatus :: Lens.Lens' DeleteNamespaceResponse Core.Int
dnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
