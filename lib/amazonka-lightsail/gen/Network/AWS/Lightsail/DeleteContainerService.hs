{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteContainerService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerService
    (
    -- * Creating a request
      DeleteContainerService (..)
    , mkDeleteContainerService
    -- ** Request lenses
    , dcsServiceName

    -- * Destructuring the response
    , DeleteContainerServiceResponse (..)
    , mkDeleteContainerServiceResponse
    -- ** Response lenses
    , dcsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteContainerService' smart constructor.
newtype DeleteContainerService = DeleteContainerService'
  { serviceName :: Types.ServiceName
    -- ^ The name of the container service to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerService' value with any optional fields omitted.
mkDeleteContainerService
    :: Types.ServiceName -- ^ 'serviceName'
    -> DeleteContainerService
mkDeleteContainerService serviceName
  = DeleteContainerService'{serviceName}

-- | The name of the container service to delete.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsServiceName :: Lens.Lens' DeleteContainerService Types.ServiceName
dcsServiceName = Lens.field @"serviceName"
{-# INLINEABLE dcsServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

instance Core.ToQuery DeleteContainerService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteContainerService where
        toHeaders DeleteContainerService{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteContainerService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteContainerService where
        toJSON DeleteContainerService{..}
          = Core.object
              (Core.catMaybes [Core.Just ("serviceName" Core..= serviceName)])

instance Core.AWSRequest DeleteContainerService where
        type Rs DeleteContainerService = DeleteContainerServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteContainerServiceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteContainerServiceResponse' smart constructor.
newtype DeleteContainerServiceResponse = DeleteContainerServiceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteContainerServiceResponse' value with any optional fields omitted.
mkDeleteContainerServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteContainerServiceResponse
mkDeleteContainerServiceResponse responseStatus
  = DeleteContainerServiceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrrsResponseStatus :: Lens.Lens' DeleteContainerServiceResponse Core.Int
dcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
