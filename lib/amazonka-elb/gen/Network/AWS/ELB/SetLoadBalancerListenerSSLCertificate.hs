{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      SetLoadBalancerListenerSSLCertificate (..)
    , mkSetLoadBalancerListenerSSLCertificate
    -- ** Request lenses
    , slblsslcLoadBalancerName
    , slblsslcLoadBalancerPort
    , slblsslcSSLCertificateId

    -- * Destructuring the response
    , SetLoadBalancerListenerSSLCertificateResponse (..)
    , mkSetLoadBalancerListenerSSLCertificateResponse
    -- ** Response lenses
    , slblsslcrrsResponseStatus
    ) where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'mkSetLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
  { loadBalancerName :: Types.AccessPointName
    -- ^ The name of the load balancer.
  , loadBalancerPort :: Core.Int
    -- ^ The port that uses the specified SSL certificate.
  , sSLCertificateId :: Types.SSLCertificateId
    -- ^ The Amazon Resource Name (ARN) of the SSL certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerListenerSSLCertificate' value with any optional fields omitted.
mkSetLoadBalancerListenerSSLCertificate
    :: Types.AccessPointName -- ^ 'loadBalancerName'
    -> Core.Int -- ^ 'loadBalancerPort'
    -> Types.SSLCertificateId -- ^ 'sSLCertificateId'
    -> SetLoadBalancerListenerSSLCertificate
mkSetLoadBalancerListenerSSLCertificate loadBalancerName
  loadBalancerPort sSLCertificateId
  = SetLoadBalancerListenerSSLCertificate'{loadBalancerName,
                                           loadBalancerPort, sSLCertificateId}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcLoadBalancerName :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Types.AccessPointName
slblsslcLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE slblsslcLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The port that uses the specified SSL certificate.
--
-- /Note:/ Consider using 'loadBalancerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcLoadBalancerPort :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Core.Int
slblsslcLoadBalancerPort = Lens.field @"loadBalancerPort"
{-# INLINEABLE slblsslcLoadBalancerPort #-}
{-# DEPRECATED loadBalancerPort "Use generic-lens or generic-optics with 'loadBalancerPort' instead"  #-}

-- | The Amazon Resource Name (ARN) of the SSL certificate.
--
-- /Note:/ Consider using 'sSLCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcSSLCertificateId :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Types.SSLCertificateId
slblsslcSSLCertificateId = Lens.field @"sSLCertificateId"
{-# INLINEABLE slblsslcSSLCertificateId #-}
{-# DEPRECATED sSLCertificateId "Use generic-lens or generic-optics with 'sSLCertificateId' instead"  #-}

instance Core.ToQuery SetLoadBalancerListenerSSLCertificate where
        toQuery SetLoadBalancerListenerSSLCertificate{..}
          = Core.toQueryPair "Action"
              ("SetLoadBalancerListenerSSLCertificate" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "LoadBalancerName" loadBalancerName
              Core.<> Core.toQueryPair "LoadBalancerPort" loadBalancerPort
              Core.<> Core.toQueryPair "SSLCertificateId" sSLCertificateId

instance Core.ToHeaders SetLoadBalancerListenerSSLCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetLoadBalancerListenerSSLCertificate
         where
        type Rs SetLoadBalancerListenerSSLCertificate =
             SetLoadBalancerListenerSSLCertificateResponse
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
          = Response.receiveXMLWrapper
              "SetLoadBalancerListenerSSLCertificateResult"
              (\ s h x ->
                 SetLoadBalancerListenerSSLCertificateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'mkSetLoadBalancerListenerSSLCertificateResponse' smart constructor.
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoadBalancerListenerSSLCertificateResponse' value with any optional fields omitted.
mkSetLoadBalancerListenerSSLCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SetLoadBalancerListenerSSLCertificateResponse
mkSetLoadBalancerListenerSSLCertificateResponse responseStatus
  = SetLoadBalancerListenerSSLCertificateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblsslcrrsResponseStatus :: Lens.Lens' SetLoadBalancerListenerSSLCertificateResponse Core.Int
slblsslcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE slblsslcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
