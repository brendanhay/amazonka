{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint configuration. The @DeleteEndpointConfig@ API deletes only the specified configuration. It does not delete endpoints created using the configuration. 
--
-- You must not delete an @EndpointConfig@ in use by an endpoint that is live or while the @UpdateEndpoint@ or @CreateEndpoint@ operations are being performed on the endpoint. If you delete the @EndpointConfig@ of an endpoint that is active or being created or updated you may lose visibility into the instance type the endpoint is using. The endpoint must be deleted in order to stop incurring charges.
module Network.AWS.SageMaker.DeleteEndpointConfig
    (
    -- * Creating a request
      DeleteEndpointConfig (..)
    , mkDeleteEndpointConfig
    -- ** Request lenses
    , dEndpointConfigName

    -- * Destructuring the response
    , DeleteEndpointConfigResponse (..)
    , mkDeleteEndpointConfigResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteEndpointConfig' smart constructor.
newtype DeleteEndpointConfig = DeleteEndpointConfig'
  { endpointConfigName :: Types.EndpointConfigName
    -- ^ The name of the endpoint configuration that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpointConfig' value with any optional fields omitted.
mkDeleteEndpointConfig
    :: Types.EndpointConfigName -- ^ 'endpointConfigName'
    -> DeleteEndpointConfig
mkDeleteEndpointConfig endpointConfigName
  = DeleteEndpointConfig'{endpointConfigName}

-- | The name of the endpoint configuration that you want to delete.
--
-- /Note:/ Consider using 'endpointConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEndpointConfigName :: Lens.Lens' DeleteEndpointConfig Types.EndpointConfigName
dEndpointConfigName = Lens.field @"endpointConfigName"
{-# INLINEABLE dEndpointConfigName #-}
{-# DEPRECATED endpointConfigName "Use generic-lens or generic-optics with 'endpointConfigName' instead"  #-}

instance Core.ToQuery DeleteEndpointConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEndpointConfig where
        toHeaders DeleteEndpointConfig{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DeleteEndpointConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteEndpointConfig where
        toJSON DeleteEndpointConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EndpointConfigName" Core..= endpointConfigName)])

instance Core.AWSRequest DeleteEndpointConfig where
        type Rs DeleteEndpointConfig = DeleteEndpointConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteEndpointConfigResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteEndpointConfigResponse' smart constructor.
data DeleteEndpointConfigResponse = DeleteEndpointConfigResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEndpointConfigResponse' value with any optional fields omitted.
mkDeleteEndpointConfigResponse
    :: DeleteEndpointConfigResponse
mkDeleteEndpointConfigResponse = DeleteEndpointConfigResponse'
