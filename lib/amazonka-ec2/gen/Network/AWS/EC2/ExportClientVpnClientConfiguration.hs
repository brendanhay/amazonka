{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportClientVpnClientConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the contents of the Client VPN endpoint configuration file for the specified Client VPN endpoint. The Client VPN endpoint configuration file includes the Client VPN endpoint and certificate information clients need to establish a connection with the Client VPN endpoint.
module Network.AWS.EC2.ExportClientVpnClientConfiguration
    (
    -- * Creating a request
      ExportClientVpnClientConfiguration (..)
    , mkExportClientVpnClientConfiguration
    -- ** Request lenses
    , ecvccClientVpnEndpointId
    , ecvccDryRun

    -- * Destructuring the response
    , ExportClientVpnClientConfigurationResponse (..)
    , mkExportClientVpnClientConfigurationResponse
    -- ** Response lenses
    , ecvccrrsClientConfiguration
    , ecvccrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportClientVpnClientConfiguration' smart constructor.
data ExportClientVpnClientConfiguration = ExportClientVpnClientConfiguration'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportClientVpnClientConfiguration' value with any optional fields omitted.
mkExportClientVpnClientConfiguration
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> ExportClientVpnClientConfiguration
mkExportClientVpnClientConfiguration clientVpnEndpointId
  = ExportClientVpnClientConfiguration'{clientVpnEndpointId,
                                        dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccClientVpnEndpointId :: Lens.Lens' ExportClientVpnClientConfiguration Types.ClientVpnEndpointId
ecvccClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE ecvccClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccDryRun :: Lens.Lens' ExportClientVpnClientConfiguration (Core.Maybe Core.Bool)
ecvccDryRun = Lens.field @"dryRun"
{-# INLINEABLE ecvccDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ExportClientVpnClientConfiguration where
        toQuery ExportClientVpnClientConfiguration{..}
          = Core.toQueryPair "Action"
              ("ExportClientVpnClientConfiguration" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ExportClientVpnClientConfiguration where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ExportClientVpnClientConfiguration where
        type Rs ExportClientVpnClientConfiguration =
             ExportClientVpnClientConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ExportClientVpnClientConfigurationResponse' Core.<$>
                   (x Core..@? "clientConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExportClientVpnClientConfigurationResponse' smart constructor.
data ExportClientVpnClientConfigurationResponse = ExportClientVpnClientConfigurationResponse'
  { clientConfiguration :: Core.Maybe Core.Text
    -- ^ The contents of the Client VPN endpoint configuration file.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportClientVpnClientConfigurationResponse' value with any optional fields omitted.
mkExportClientVpnClientConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExportClientVpnClientConfigurationResponse
mkExportClientVpnClientConfigurationResponse responseStatus
  = ExportClientVpnClientConfigurationResponse'{clientConfiguration =
                                                  Core.Nothing,
                                                responseStatus}

-- | The contents of the Client VPN endpoint configuration file.
--
-- /Note:/ Consider using 'clientConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrrsClientConfiguration :: Lens.Lens' ExportClientVpnClientConfigurationResponse (Core.Maybe Core.Text)
ecvccrrsClientConfiguration = Lens.field @"clientConfiguration"
{-# INLINEABLE ecvccrrsClientConfiguration #-}
{-# DEPRECATED clientConfiguration "Use generic-lens or generic-optics with 'clientConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrrsResponseStatus :: Lens.Lens' ExportClientVpnClientConfigurationResponse Core.Int
ecvccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ecvccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
