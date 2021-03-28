{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the client certificate revocation list for the specified Client VPN endpoint.
module Network.AWS.EC2.ExportClientVpnClientCertificateRevocationList
    (
    -- * Creating a request
      ExportClientVpnClientCertificateRevocationList (..)
    , mkExportClientVpnClientCertificateRevocationList
    -- ** Request lenses
    , ecvccrlClientVpnEndpointId
    , ecvccrlDryRun

    -- * Destructuring the response
    , ExportClientVpnClientCertificateRevocationListResponse (..)
    , mkExportClientVpnClientCertificateRevocationListResponse
    -- ** Response lenses
    , ecvccrlrrsCertificateRevocationList
    , ecvccrlrrsStatus
    , ecvccrlrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportClientVpnClientCertificateRevocationList' smart constructor.
data ExportClientVpnClientCertificateRevocationList = ExportClientVpnClientCertificateRevocationList'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportClientVpnClientCertificateRevocationList' value with any optional fields omitted.
mkExportClientVpnClientCertificateRevocationList
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> ExportClientVpnClientCertificateRevocationList
mkExportClientVpnClientCertificateRevocationList
  clientVpnEndpointId
  = ExportClientVpnClientCertificateRevocationList'{clientVpnEndpointId,
                                                    dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlClientVpnEndpointId :: Lens.Lens' ExportClientVpnClientCertificateRevocationList Types.ClientVpnEndpointId
ecvccrlClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE ecvccrlClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlDryRun :: Lens.Lens' ExportClientVpnClientCertificateRevocationList (Core.Maybe Core.Bool)
ecvccrlDryRun = Lens.field @"dryRun"
{-# INLINEABLE ecvccrlDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery
           ExportClientVpnClientCertificateRevocationList
         where
        toQuery ExportClientVpnClientCertificateRevocationList{..}
          = Core.toQueryPair "Action"
              ("ExportClientVpnClientCertificateRevocationList" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders
           ExportClientVpnClientCertificateRevocationList
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           ExportClientVpnClientCertificateRevocationList
         where
        type Rs ExportClientVpnClientCertificateRevocationList =
             ExportClientVpnClientCertificateRevocationListResponse
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
                 ExportClientVpnClientCertificateRevocationListResponse' Core.<$>
                   (x Core..@? "certificateRevocationList") Core.<*>
                     x Core..@? "status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExportClientVpnClientCertificateRevocationListResponse' smart constructor.
data ExportClientVpnClientCertificateRevocationListResponse = ExportClientVpnClientCertificateRevocationListResponse'
  { certificateRevocationList :: Core.Maybe Core.Text
    -- ^ Information about the client certificate revocation list.
  , status :: Core.Maybe Types.ClientCertificateRevocationListStatus
    -- ^ The current state of the client certificate revocation list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportClientVpnClientCertificateRevocationListResponse' value with any optional fields omitted.
mkExportClientVpnClientCertificateRevocationListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExportClientVpnClientCertificateRevocationListResponse
mkExportClientVpnClientCertificateRevocationListResponse
  responseStatus
  = ExportClientVpnClientCertificateRevocationListResponse'{certificateRevocationList
                                                              = Core.Nothing,
                                                            status = Core.Nothing, responseStatus}

-- | Information about the client certificate revocation list.
--
-- /Note:/ Consider using 'certificateRevocationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlrrsCertificateRevocationList :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse (Core.Maybe Core.Text)
ecvccrlrrsCertificateRevocationList = Lens.field @"certificateRevocationList"
{-# INLINEABLE ecvccrlrrsCertificateRevocationList #-}
{-# DEPRECATED certificateRevocationList "Use generic-lens or generic-optics with 'certificateRevocationList' instead"  #-}

-- | The current state of the client certificate revocation list.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlrrsStatus :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse (Core.Maybe Types.ClientCertificateRevocationListStatus)
ecvccrlrrsStatus = Lens.field @"status"
{-# INLINEABLE ecvccrlrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecvccrlrrsResponseStatus :: Lens.Lens' ExportClientVpnClientCertificateRevocationListResponse Core.Int
ecvccrlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ecvccrlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
