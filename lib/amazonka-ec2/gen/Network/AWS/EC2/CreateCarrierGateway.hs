{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateCarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a carrier gateway. For more information about carrier gateways, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#wavelength-carrier-gateway Carrier gateways> in the /AWS Wavelength Developer Guide/ .
module Network.AWS.EC2.CreateCarrierGateway
    (
    -- * Creating a request
      CreateCarrierGateway (..)
    , mkCreateCarrierGateway
    -- ** Request lenses
    , ccgfVpcId
    , ccgfClientToken
    , ccgfDryRun
    , ccgfTagSpecifications

    -- * Destructuring the response
    , CreateCarrierGatewayResponse (..)
    , mkCreateCarrierGatewayResponse
    -- ** Response lenses
    , ccgrfrsCarrierGateway
    , ccgrfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateCarrierGateway' smart constructor.
data CreateCarrierGateway = CreateCarrierGateway'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC to associate with the carrier gateway.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to associate with the carrier gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCarrierGateway' value with any optional fields omitted.
mkCreateCarrierGateway
    :: Types.VpcId -- ^ 'vpcId'
    -> CreateCarrierGateway
mkCreateCarrierGateway vpcId
  = CreateCarrierGateway'{vpcId, clientToken = Core.Nothing,
                          dryRun = Core.Nothing, tagSpecifications = Core.Nothing}

-- | The ID of the VPC to associate with the carrier gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfVpcId :: Lens.Lens' CreateCarrierGateway Types.VpcId
ccgfVpcId = Lens.field @"vpcId"
{-# INLINEABLE ccgfVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfClientToken :: Lens.Lens' CreateCarrierGateway (Core.Maybe Core.Text)
ccgfClientToken = Lens.field @"clientToken"
{-# INLINEABLE ccgfClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfDryRun :: Lens.Lens' CreateCarrierGateway (Core.Maybe Core.Bool)
ccgfDryRun = Lens.field @"dryRun"
{-# INLINEABLE ccgfDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to associate with the carrier gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgfTagSpecifications :: Lens.Lens' CreateCarrierGateway (Core.Maybe [Types.TagSpecification])
ccgfTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ccgfTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateCarrierGateway where
        toQuery CreateCarrierGateway{..}
          = Core.toQueryPair "Action" ("CreateCarrierGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateCarrierGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCarrierGateway where
        type Rs CreateCarrierGateway = CreateCarrierGatewayResponse
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
                 CreateCarrierGatewayResponse' Core.<$>
                   (x Core..@? "carrierGateway") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCarrierGatewayResponse' smart constructor.
data CreateCarrierGatewayResponse = CreateCarrierGatewayResponse'
  { carrierGateway :: Core.Maybe Types.CarrierGateway
    -- ^ Information about the carrier gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCarrierGatewayResponse' value with any optional fields omitted.
mkCreateCarrierGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCarrierGatewayResponse
mkCreateCarrierGatewayResponse responseStatus
  = CreateCarrierGatewayResponse'{carrierGateway = Core.Nothing,
                                  responseStatus}

-- | Information about the carrier gateway.
--
-- /Note:/ Consider using 'carrierGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgrfrsCarrierGateway :: Lens.Lens' CreateCarrierGatewayResponse (Core.Maybe Types.CarrierGateway)
ccgrfrsCarrierGateway = Lens.field @"carrierGateway"
{-# INLINEABLE ccgrfrsCarrierGateway #-}
{-# DEPRECATED carrierGateway "Use generic-lens or generic-optics with 'carrierGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccgrfrsResponseStatus :: Lens.Lens' CreateCarrierGatewayResponse Core.Int
ccgrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ccgrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
