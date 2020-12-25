{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the certificate that terminates the specified listener's SSL connections. The specified certificate replaces any prior certificate that was used on the same load balancer and port.
--
-- For more information about updating your SSL certificate, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-update-ssl-cert.html Replace the SSL Certificate for Your Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
  ( -- * Creating a request
    SetLoadBalancerListenerSSLCertificate (..),
    mkSetLoadBalancerListenerSSLCertificate,

    -- ** Request lenses
    slblsslcLoadBalancerName,
    slblsslcLoadBalancerPort,
    slblsslcSSLCertificateId,

    -- * Destructuring the response
    SetLoadBalancerListenerSSLCertificateResponse (..),
    mkSetLoadBalancerListenerSSLCertificateResponse,

    -- ** Response lenses
    slblsslcrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'mkSetLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The port that uses the specified SSL certificate.
    loadBalancerPort :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the SSL certificate.
    sSLCertificateId :: Types.SSLCertificateId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerListenerSSLCertificate' value with any optional fields omitted.
mkSetLoadBalancerListenerSSLCertificate ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  -- | 'loadBalancerPort'
  Core.Int ->
  -- | 'sSLCertificateId'
  Types.SSLCertificateId ->
  SetLoadBalancerListenerSSLCertificate
mkSetLoadBalancerListenerSSLCertificate
  loadBalancerName
  loadBalancerPort
  sSLCertificateId =
    SetLoadBalancerListenerSSLCertificate'
      { loadBalancerName,
        loadBalancerPort,
        sSLCertificateId
      }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcLoadBalancerName :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Types.AccessPointName
slblsslcLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED slblsslcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The port that uses the specified SSL certificate.
--
-- /Note:/ Consider using 'loadBalancerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcLoadBalancerPort :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Core.Int
slblsslcLoadBalancerPort = Lens.field @"loadBalancerPort"
{-# DEPRECATED slblsslcLoadBalancerPort "Use generic-lens or generic-optics with 'loadBalancerPort' instead." #-}

-- | The Amazon Resource Name (ARN) of the SSL certificate.
--
-- /Note:/ Consider using 'sSLCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcSSLCertificateId :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Types.SSLCertificateId
slblsslcSSLCertificateId = Lens.field @"sSLCertificateId"
{-# DEPRECATED slblsslcSSLCertificateId "Use generic-lens or generic-optics with 'sSLCertificateId' instead." #-}

instance Core.AWSRequest SetLoadBalancerListenerSSLCertificate where
  type
    Rs SetLoadBalancerListenerSSLCertificate =
      SetLoadBalancerListenerSSLCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "SetLoadBalancerListenerSSLCertificate")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> (Core.toQueryValue "LoadBalancerPort" loadBalancerPort)
                Core.<> (Core.toQueryValue "SSLCertificateId" sSLCertificateId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetLoadBalancerListenerSSLCertificateResult"
      ( \s h x ->
          SetLoadBalancerListenerSSLCertificateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'mkSetLoadBalancerListenerSSLCertificateResponse' smart constructor.
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerListenerSSLCertificateResponse' value with any optional fields omitted.
mkSetLoadBalancerListenerSSLCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetLoadBalancerListenerSSLCertificateResponse
mkSetLoadBalancerListenerSSLCertificateResponse responseStatus =
  SetLoadBalancerListenerSSLCertificateResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcrrsResponseStatus :: Lens.Lens' SetLoadBalancerListenerSSLCertificateResponse Core.Int
slblsslcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED slblsslcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
