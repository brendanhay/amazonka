{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a client certificate revocation list to the specified Client VPN endpoint. Uploading a client certificate revocation list overwrites the existing client certificate revocation list.
--
-- Uploading a client certificate revocation list resets existing client connections.
module Network.AWS.EC2.ImportClientVpnClientCertificateRevocationList
    (
    -- * Creating a request
      ImportClientVpnClientCertificateRevocationList (..)
    , mkImportClientVpnClientCertificateRevocationList
    -- ** Request lenses
    , icvccrlClientVpnEndpointId
    , icvccrlCertificateRevocationList
    , icvccrlDryRun

    -- * Destructuring the response
    , ImportClientVpnClientCertificateRevocationListResponse (..)
    , mkImportClientVpnClientCertificateRevocationListResponse
    -- ** Response lenses
    , icvccrlrrsReturn
    , icvccrlrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportClientVpnClientCertificateRevocationList' smart constructor.
data ImportClientVpnClientCertificateRevocationList = ImportClientVpnClientCertificateRevocationList'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint to which the client certificate revocation list applies.
  , certificateRevocationList :: Core.Text
    -- ^ The client certificate revocation list file. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List> in the /AWS Client VPN Administrator Guide/ .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportClientVpnClientCertificateRevocationList' value with any optional fields omitted.
mkImportClientVpnClientCertificateRevocationList
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Core.Text -- ^ 'certificateRevocationList'
    -> ImportClientVpnClientCertificateRevocationList
mkImportClientVpnClientCertificateRevocationList
  clientVpnEndpointId certificateRevocationList
  = ImportClientVpnClientCertificateRevocationList'{clientVpnEndpointId,
                                                    certificateRevocationList,
                                                    dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint to which the client certificate revocation list applies.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlClientVpnEndpointId :: Lens.Lens' ImportClientVpnClientCertificateRevocationList Types.ClientVpnEndpointId
icvccrlClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE icvccrlClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The client certificate revocation list file. For more information, see <https://docs.aws.amazon.com/vpn/latest/clientvpn-admin/cvpn-working-certificates.html#cvpn-working-certificates-generate Generate a Client Certificate Revocation List> in the /AWS Client VPN Administrator Guide/ .
--
-- /Note:/ Consider using 'certificateRevocationList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlCertificateRevocationList :: Lens.Lens' ImportClientVpnClientCertificateRevocationList Core.Text
icvccrlCertificateRevocationList = Lens.field @"certificateRevocationList"
{-# INLINEABLE icvccrlCertificateRevocationList #-}
{-# DEPRECATED certificateRevocationList "Use generic-lens or generic-optics with 'certificateRevocationList' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlDryRun :: Lens.Lens' ImportClientVpnClientCertificateRevocationList (Core.Maybe Core.Bool)
icvccrlDryRun = Lens.field @"dryRun"
{-# INLINEABLE icvccrlDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery
           ImportClientVpnClientCertificateRevocationList
         where
        toQuery ImportClientVpnClientCertificateRevocationList{..}
          = Core.toQueryPair "Action"
              ("ImportClientVpnClientCertificateRevocationList" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<>
              Core.toQueryPair "CertificateRevocationList"
                certificateRevocationList
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders
           ImportClientVpnClientCertificateRevocationList
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           ImportClientVpnClientCertificateRevocationList
         where
        type Rs ImportClientVpnClientCertificateRevocationList =
             ImportClientVpnClientCertificateRevocationListResponse
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
                 ImportClientVpnClientCertificateRevocationListResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportClientVpnClientCertificateRevocationListResponse' smart constructor.
data ImportClientVpnClientCertificateRevocationListResponse = ImportClientVpnClientCertificateRevocationListResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportClientVpnClientCertificateRevocationListResponse' value with any optional fields omitted.
mkImportClientVpnClientCertificateRevocationListResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportClientVpnClientCertificateRevocationListResponse
mkImportClientVpnClientCertificateRevocationListResponse
  responseStatus
  = ImportClientVpnClientCertificateRevocationListResponse'{return =
                                                              Core.Nothing,
                                                            responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlrrsReturn :: Lens.Lens' ImportClientVpnClientCertificateRevocationListResponse (Core.Maybe Core.Bool)
icvccrlrrsReturn = Lens.field @"return"
{-# INLINEABLE icvccrlrrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icvccrlrrsResponseStatus :: Lens.Lens' ImportClientVpnClientCertificateRevocationListResponse Core.Int
icvccrlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE icvccrlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
