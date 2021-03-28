{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CloseInstancePublicPorts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Closes ports for a specific Amazon Lightsail instance.
--
-- The @CloseInstancePublicPorts@ action supports tag-based access control via resource tags applied to the resource identified by @instanceName@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CloseInstancePublicPorts
    (
    -- * Creating a request
      CloseInstancePublicPorts (..)
    , mkCloseInstancePublicPorts
    -- ** Request lenses
    , cippPortInfo
    , cippInstanceName

    -- * Destructuring the response
    , CloseInstancePublicPortsResponse (..)
    , mkCloseInstancePublicPortsResponse
    -- ** Response lenses
    , cipprrsOperation
    , cipprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCloseInstancePublicPorts' smart constructor.
data CloseInstancePublicPorts = CloseInstancePublicPorts'
  { portInfo :: Types.PortInfo
    -- ^ An object to describe the ports to close for the specified instance.
  , instanceName :: Types.ResourceName
    -- ^ The name of the instance for which to close ports.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloseInstancePublicPorts' value with any optional fields omitted.
mkCloseInstancePublicPorts
    :: Types.PortInfo -- ^ 'portInfo'
    -> Types.ResourceName -- ^ 'instanceName'
    -> CloseInstancePublicPorts
mkCloseInstancePublicPorts portInfo instanceName
  = CloseInstancePublicPorts'{portInfo, instanceName}

-- | An object to describe the ports to close for the specified instance.
--
-- /Note:/ Consider using 'portInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cippPortInfo :: Lens.Lens' CloseInstancePublicPorts Types.PortInfo
cippPortInfo = Lens.field @"portInfo"
{-# INLINEABLE cippPortInfo #-}
{-# DEPRECATED portInfo "Use generic-lens or generic-optics with 'portInfo' instead"  #-}

-- | The name of the instance for which to close ports.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cippInstanceName :: Lens.Lens' CloseInstancePublicPorts Types.ResourceName
cippInstanceName = Lens.field @"instanceName"
{-# INLINEABLE cippInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.ToQuery CloseInstancePublicPorts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CloseInstancePublicPorts where
        toHeaders CloseInstancePublicPorts{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CloseInstancePublicPorts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CloseInstancePublicPorts where
        toJSON CloseInstancePublicPorts{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("portInfo" Core..= portInfo),
                  Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest CloseInstancePublicPorts where
        type Rs CloseInstancePublicPorts = CloseInstancePublicPortsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CloseInstancePublicPortsResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCloseInstancePublicPortsResponse' smart constructor.
data CloseInstancePublicPortsResponse = CloseInstancePublicPortsResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CloseInstancePublicPortsResponse' value with any optional fields omitted.
mkCloseInstancePublicPortsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CloseInstancePublicPortsResponse
mkCloseInstancePublicPortsResponse responseStatus
  = CloseInstancePublicPortsResponse'{operation = Core.Nothing,
                                      responseStatus}

-- | An object that describes the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipprrsOperation :: Lens.Lens' CloseInstancePublicPortsResponse (Core.Maybe Types.Operation)
cipprrsOperation = Lens.field @"operation"
{-# INLINEABLE cipprrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipprrsResponseStatus :: Lens.Lens' CloseInstancePublicPortsResponse Core.Int
cipprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cipprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
