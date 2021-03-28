{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerServiceDeployments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the deployments for your Amazon Lightsail container service
--
-- A deployment specifies the settings, such as the ports and launch command, of containers that are deployed to your container service.
-- The deployments are ordered by version in ascending order. The newest version is listed at the top of the response.
module Network.AWS.Lightsail.GetContainerServiceDeployments
    (
    -- * Creating a request
      GetContainerServiceDeployments (..)
    , mkGetContainerServiceDeployments
    -- ** Request lenses
    , gcsdServiceName

    -- * Destructuring the response
    , GetContainerServiceDeploymentsResponse (..)
    , mkGetContainerServiceDeploymentsResponse
    -- ** Response lenses
    , gcsdrrsDeployments
    , gcsdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerServiceDeployments' smart constructor.
newtype GetContainerServiceDeployments = GetContainerServiceDeployments'
  { serviceName :: Types.ServiceName
    -- ^ The name of the container service for which to return deployments.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetContainerServiceDeployments' value with any optional fields omitted.
mkGetContainerServiceDeployments
    :: Types.ServiceName -- ^ 'serviceName'
    -> GetContainerServiceDeployments
mkGetContainerServiceDeployments serviceName
  = GetContainerServiceDeployments'{serviceName}

-- | The name of the container service for which to return deployments.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsdServiceName :: Lens.Lens' GetContainerServiceDeployments Types.ServiceName
gcsdServiceName = Lens.field @"serviceName"
{-# INLINEABLE gcsdServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

instance Core.ToQuery GetContainerServiceDeployments where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetContainerServiceDeployments where
        toHeaders GetContainerServiceDeployments{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetContainerServiceDeployments")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetContainerServiceDeployments where
        toJSON GetContainerServiceDeployments{..}
          = Core.object
              (Core.catMaybes [Core.Just ("serviceName" Core..= serviceName)])

instance Core.AWSRequest GetContainerServiceDeployments where
        type Rs GetContainerServiceDeployments =
             GetContainerServiceDeploymentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetContainerServiceDeploymentsResponse' Core.<$>
                   (x Core..:? "deployments") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetContainerServiceDeploymentsResponse' smart constructor.
data GetContainerServiceDeploymentsResponse = GetContainerServiceDeploymentsResponse'
  { deployments :: Core.Maybe [Types.ContainerServiceDeployment]
    -- ^ An array of objects that describe deployments for a container service.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetContainerServiceDeploymentsResponse' value with any optional fields omitted.
mkGetContainerServiceDeploymentsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetContainerServiceDeploymentsResponse
mkGetContainerServiceDeploymentsResponse responseStatus
  = GetContainerServiceDeploymentsResponse'{deployments =
                                              Core.Nothing,
                                            responseStatus}

-- | An array of objects that describe deployments for a container service.
--
-- /Note:/ Consider using 'deployments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsdrrsDeployments :: Lens.Lens' GetContainerServiceDeploymentsResponse (Core.Maybe [Types.ContainerServiceDeployment])
gcsdrrsDeployments = Lens.field @"deployments"
{-# INLINEABLE gcsdrrsDeployments #-}
{-# DEPRECATED deployments "Use generic-lens or generic-optics with 'deployments' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsdrrsResponseStatus :: Lens.Lens' GetContainerServiceDeploymentsResponse Core.Int
gcsdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcsdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
