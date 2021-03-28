{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates engine-specific attributes on a specified server. The server enters the @MODIFYING@ state when this operation is in progress. Only one update can occur at a time. You can use this command to reset a Chef server's public key (@CHEF_PIVOTAL_KEY@ ) or a Puppet server's admin password (@PUPPET_ADMIN_PASSWORD@ ). 
--
-- This operation is asynchronous. 
-- This operation can only be called for servers in @HEALTHY@ or @UNHEALTHY@ states. Otherwise, an @InvalidStateException@ is raised. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid. 
module Network.AWS.OpsWorksCM.UpdateServerEngineAttributes
    (
    -- * Creating a request
      UpdateServerEngineAttributes (..)
    , mkUpdateServerEngineAttributes
    -- ** Request lenses
    , useaServerName
    , useaAttributeName
    , useaAttributeValue

    -- * Destructuring the response
    , UpdateServerEngineAttributesResponse (..)
    , mkUpdateServerEngineAttributesResponse
    -- ** Response lenses
    , usearrsServer
    , usearrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateServerEngineAttributes' smart constructor.
data UpdateServerEngineAttributes = UpdateServerEngineAttributes'
  { serverName :: Types.ServerName
    -- ^ The name of the server to update. 
  , attributeName :: Types.AttributeName
    -- ^ The name of the engine attribute to update. 
  , attributeValue :: Core.Maybe Types.AttributeValue
    -- ^ The value to set for the attribute. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateServerEngineAttributes' value with any optional fields omitted.
mkUpdateServerEngineAttributes
    :: Types.ServerName -- ^ 'serverName'
    -> Types.AttributeName -- ^ 'attributeName'
    -> UpdateServerEngineAttributes
mkUpdateServerEngineAttributes serverName attributeName
  = UpdateServerEngineAttributes'{serverName, attributeName,
                                  attributeValue = Core.Nothing}

-- | The name of the server to update. 
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
useaServerName :: Lens.Lens' UpdateServerEngineAttributes Types.ServerName
useaServerName = Lens.field @"serverName"
{-# INLINEABLE useaServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | The name of the engine attribute to update. 
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
useaAttributeName :: Lens.Lens' UpdateServerEngineAttributes Types.AttributeName
useaAttributeName = Lens.field @"attributeName"
{-# INLINEABLE useaAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The value to set for the attribute. 
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
useaAttributeValue :: Lens.Lens' UpdateServerEngineAttributes (Core.Maybe Types.AttributeValue)
useaAttributeValue = Lens.field @"attributeValue"
{-# INLINEABLE useaAttributeValue #-}
{-# DEPRECATED attributeValue "Use generic-lens or generic-optics with 'attributeValue' instead"  #-}

instance Core.ToQuery UpdateServerEngineAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateServerEngineAttributes where
        toHeaders UpdateServerEngineAttributes{..}
          = Core.pure
              ("X-Amz-Target",
               "OpsWorksCM_V2016_11_01.UpdateServerEngineAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateServerEngineAttributes where
        toJSON UpdateServerEngineAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ServerName" Core..= serverName),
                  Core.Just ("AttributeName" Core..= attributeName),
                  ("AttributeValue" Core..=) Core.<$> attributeValue])

instance Core.AWSRequest UpdateServerEngineAttributes where
        type Rs UpdateServerEngineAttributes =
             UpdateServerEngineAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateServerEngineAttributesResponse' Core.<$>
                   (x Core..:? "Server") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateServerEngineAttributesResponse' smart constructor.
data UpdateServerEngineAttributesResponse = UpdateServerEngineAttributesResponse'
  { server :: Core.Maybe Types.Server
    -- ^ Contains the response to an @UpdateServerEngineAttributes@ request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateServerEngineAttributesResponse' value with any optional fields omitted.
mkUpdateServerEngineAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateServerEngineAttributesResponse
mkUpdateServerEngineAttributesResponse responseStatus
  = UpdateServerEngineAttributesResponse'{server = Core.Nothing,
                                          responseStatus}

-- | Contains the response to an @UpdateServerEngineAttributes@ request. 
--
-- /Note:/ Consider using 'server' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usearrsServer :: Lens.Lens' UpdateServerEngineAttributesResponse (Core.Maybe Types.Server)
usearrsServer = Lens.field @"server"
{-# INLINEABLE usearrsServer #-}
{-# DEPRECATED server "Use generic-lens or generic-optics with 'server' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usearrsResponseStatus :: Lens.Lens' UpdateServerEngineAttributesResponse Core.Int
usearrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE usearrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
