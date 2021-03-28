{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the verification process to prove that the service provider owns the private DNS name domain for the endpoint service.
--
-- The service provider must successfully perform the verification before the consumer can use the name to access the service.
-- Before the service provider runs this command, they must add a record to the DNS server. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html#add-dns-txt-record Adding a TXT Record to Your Domain's DNS Server > in the /Amazon VPC User Guide/ .
module Network.AWS.EC2.StartVpcEndpointServicePrivateDnsVerification
    (
    -- * Creating a request
      StartVpcEndpointServicePrivateDnsVerification (..)
    , mkStartVpcEndpointServicePrivateDnsVerification
    -- ** Request lenses
    , svespdvServiceId
    , svespdvDryRun

    -- * Destructuring the response
    , StartVpcEndpointServicePrivateDnsVerificationResponse (..)
    , mkStartVpcEndpointServicePrivateDnsVerificationResponse
    -- ** Response lenses
    , svespdvrrsReturnValue
    , svespdvrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartVpcEndpointServicePrivateDnsVerification' smart constructor.
data StartVpcEndpointServicePrivateDnsVerification = StartVpcEndpointServicePrivateDnsVerification'
  { serviceId :: Types.ServiceId
    -- ^ The ID of the endpoint service.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartVpcEndpointServicePrivateDnsVerification' value with any optional fields omitted.
mkStartVpcEndpointServicePrivateDnsVerification
    :: Types.ServiceId -- ^ 'serviceId'
    -> StartVpcEndpointServicePrivateDnsVerification
mkStartVpcEndpointServicePrivateDnsVerification serviceId
  = StartVpcEndpointServicePrivateDnsVerification'{serviceId,
                                                   dryRun = Core.Nothing}

-- | The ID of the endpoint service.
--
-- /Note:/ Consider using 'serviceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvServiceId :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerification Types.ServiceId
svespdvServiceId = Lens.field @"serviceId"
{-# INLINEABLE svespdvServiceId #-}
{-# DEPRECATED serviceId "Use generic-lens or generic-optics with 'serviceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvDryRun :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerification (Core.Maybe Core.Bool)
svespdvDryRun = Lens.field @"dryRun"
{-# INLINEABLE svespdvDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery StartVpcEndpointServicePrivateDnsVerification
         where
        toQuery StartVpcEndpointServicePrivateDnsVerification{..}
          = Core.toQueryPair "Action"
              ("StartVpcEndpointServicePrivateDnsVerification" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ServiceId" serviceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders
           StartVpcEndpointServicePrivateDnsVerification
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           StartVpcEndpointServicePrivateDnsVerification
         where
        type Rs StartVpcEndpointServicePrivateDnsVerification =
             StartVpcEndpointServicePrivateDnsVerificationResponse
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
                 StartVpcEndpointServicePrivateDnsVerificationResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartVpcEndpointServicePrivateDnsVerificationResponse' smart constructor.
data StartVpcEndpointServicePrivateDnsVerificationResponse = StartVpcEndpointServicePrivateDnsVerificationResponse'
  { returnValue :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds; otherwise, it returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartVpcEndpointServicePrivateDnsVerificationResponse' value with any optional fields omitted.
mkStartVpcEndpointServicePrivateDnsVerificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartVpcEndpointServicePrivateDnsVerificationResponse
mkStartVpcEndpointServicePrivateDnsVerificationResponse
  responseStatus
  = StartVpcEndpointServicePrivateDnsVerificationResponse'{returnValue
                                                             = Core.Nothing,
                                                           responseStatus}

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvrrsReturnValue :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerificationResponse (Core.Maybe Core.Bool)
svespdvrrsReturnValue = Lens.field @"returnValue"
{-# INLINEABLE svespdvrrsReturnValue #-}
{-# DEPRECATED returnValue "Use generic-lens or generic-optics with 'returnValue' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svespdvrrsResponseStatus :: Lens.Lens' StartVpcEndpointServicePrivateDnsVerificationResponse Core.Int
svespdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE svespdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
