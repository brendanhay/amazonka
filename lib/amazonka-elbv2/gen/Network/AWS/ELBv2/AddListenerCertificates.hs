{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.AddListenerCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified SSL server certificate to the certificate list for the specified HTTPS or TLS listener.
--
-- If the certificate in already in the certificate list, the call is successful but the certificate is not added again.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html HTTPS listeners> in the /Application Load Balancers Guide/ or <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/create-tls-listener.html TLS listeners> in the /Network Load Balancers Guide/ .
module Network.AWS.ELBv2.AddListenerCertificates
    (
    -- * Creating a request
      AddListenerCertificates (..)
    , mkAddListenerCertificates
    -- ** Request lenses
    , alcListenerArn
    , alcCertificates

    -- * Destructuring the response
    , AddListenerCertificatesResponse (..)
    , mkAddListenerCertificatesResponse
    -- ** Response lenses
    , alcrrsCertificates
    , alcrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddListenerCertificates' smart constructor.
data AddListenerCertificates = AddListenerCertificates'
  { listenerArn :: Types.ListenerArn
    -- ^ The Amazon Resource Name (ARN) of the listener.
  , certificates :: [Types.Certificate]
    -- ^ The certificate to add. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddListenerCertificates' value with any optional fields omitted.
mkAddListenerCertificates
    :: Types.ListenerArn -- ^ 'listenerArn'
    -> AddListenerCertificates
mkAddListenerCertificates listenerArn
  = AddListenerCertificates'{listenerArn, certificates = Core.mempty}

-- | The Amazon Resource Name (ARN) of the listener.
--
-- /Note:/ Consider using 'listenerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcListenerArn :: Lens.Lens' AddListenerCertificates Types.ListenerArn
alcListenerArn = Lens.field @"listenerArn"
{-# INLINEABLE alcListenerArn #-}
{-# DEPRECATED listenerArn "Use generic-lens or generic-optics with 'listenerArn' instead"  #-}

-- | The certificate to add. You can specify one certificate per call. Set @CertificateArn@ to the certificate ARN but do not set @IsDefault@ .
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcCertificates :: Lens.Lens' AddListenerCertificates [Types.Certificate]
alcCertificates = Lens.field @"certificates"
{-# INLINEABLE alcCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

instance Core.ToQuery AddListenerCertificates where
        toQuery AddListenerCertificates{..}
          = Core.toQueryPair "Action"
              ("AddListenerCertificates" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ListenerArn" listenerArn
              Core.<>
              Core.toQueryPair "Certificates"
                (Core.toQueryList "member" certificates)

instance Core.ToHeaders AddListenerCertificates where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AddListenerCertificates where
        type Rs AddListenerCertificates = AddListenerCertificatesResponse
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
          = Response.receiveXMLWrapper "AddListenerCertificatesResult"
              (\ s h x ->
                 AddListenerCertificatesResponse' Core.<$>
                   (x Core..@? "Certificates" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddListenerCertificatesResponse' smart constructor.
data AddListenerCertificatesResponse = AddListenerCertificatesResponse'
  { certificates :: Core.Maybe [Types.Certificate]
    -- ^ Information about the certificates in the certificate list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddListenerCertificatesResponse' value with any optional fields omitted.
mkAddListenerCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddListenerCertificatesResponse
mkAddListenerCertificatesResponse responseStatus
  = AddListenerCertificatesResponse'{certificates = Core.Nothing,
                                     responseStatus}

-- | Information about the certificates in the certificate list.
--
-- /Note:/ Consider using 'certificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcrrsCertificates :: Lens.Lens' AddListenerCertificatesResponse (Core.Maybe [Types.Certificate])
alcrrsCertificates = Lens.field @"certificates"
{-# INLINEABLE alcrrsCertificates #-}
{-# DEPRECATED certificates "Use generic-lens or generic-optics with 'certificates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alcrrsResponseStatus :: Lens.Lens' AddListenerCertificatesResponse Core.Int
alcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE alcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
