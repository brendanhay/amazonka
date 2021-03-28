{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateClientVpnTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a target network with a Client VPN endpoint. A target network is a subnet in a VPC. You can associate multiple subnets from the same VPC with a Client VPN endpoint. You can associate only one subnet in each Availability Zone. We recommend that you associate at least two subnets to provide Availability Zone redundancy.
--
-- If you specified a VPC when you created the Client VPN endpoint or if you have previous subnet associations, the specified subnet must be in the same VPC. To specify a subnet that's in a different VPC, you must first modify the Client VPN endpoint ('ModifyClientVpnEndpoint' ) and change the VPC that's associated with it.
module Network.AWS.EC2.AssociateClientVpnTargetNetwork
    (
    -- * Creating a request
      AssociateClientVpnTargetNetwork (..)
    , mkAssociateClientVpnTargetNetwork
    -- ** Request lenses
    , acvtnClientVpnEndpointId
    , acvtnSubnetId
    , acvtnClientToken
    , acvtnDryRun

    -- * Destructuring the response
    , AssociateClientVpnTargetNetworkResponse (..)
    , mkAssociateClientVpnTargetNetworkResponse
    -- ** Response lenses
    , acvtnrrsAssociationId
    , acvtnrrsStatus
    , acvtnrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateClientVpnTargetNetwork' smart constructor.
data AssociateClientVpnTargetNetwork = AssociateClientVpnTargetNetwork'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , subnetId :: Types.SubnetId
    -- ^ The ID of the subnet to associate with the Client VPN endpoint.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateClientVpnTargetNetwork' value with any optional fields omitted.
mkAssociateClientVpnTargetNetwork
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Types.SubnetId -- ^ 'subnetId'
    -> AssociateClientVpnTargetNetwork
mkAssociateClientVpnTargetNetwork clientVpnEndpointId subnetId
  = AssociateClientVpnTargetNetwork'{clientVpnEndpointId, subnetId,
                                     clientToken = Core.Nothing, dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnClientVpnEndpointId :: Lens.Lens' AssociateClientVpnTargetNetwork Types.ClientVpnEndpointId
acvtnClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE acvtnClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The ID of the subnet to associate with the Client VPN endpoint.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnSubnetId :: Lens.Lens' AssociateClientVpnTargetNetwork Types.SubnetId
acvtnSubnetId = Lens.field @"subnetId"
{-# INLINEABLE acvtnSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnClientToken :: Lens.Lens' AssociateClientVpnTargetNetwork (Core.Maybe Core.Text)
acvtnClientToken = Lens.field @"clientToken"
{-# INLINEABLE acvtnClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnDryRun :: Lens.Lens' AssociateClientVpnTargetNetwork (Core.Maybe Core.Bool)
acvtnDryRun = Lens.field @"dryRun"
{-# INLINEABLE acvtnDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery AssociateClientVpnTargetNetwork where
        toQuery AssociateClientVpnTargetNetwork{..}
          = Core.toQueryPair "Action"
              ("AssociateClientVpnTargetNetwork" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.toQueryPair "SubnetId" subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders AssociateClientVpnTargetNetwork where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest AssociateClientVpnTargetNetwork where
        type Rs AssociateClientVpnTargetNetwork =
             AssociateClientVpnTargetNetworkResponse
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
                 AssociateClientVpnTargetNetworkResponse' Core.<$>
                   (x Core..@? "associationId") Core.<*> x Core..@? "status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateClientVpnTargetNetworkResponse' smart constructor.
data AssociateClientVpnTargetNetworkResponse = AssociateClientVpnTargetNetworkResponse'
  { associationId :: Core.Maybe Core.Text
    -- ^ The unique ID of the target network association.
  , status :: Core.Maybe Types.AssociationStatus
    -- ^ The current state of the target network association.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateClientVpnTargetNetworkResponse' value with any optional fields omitted.
mkAssociateClientVpnTargetNetworkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateClientVpnTargetNetworkResponse
mkAssociateClientVpnTargetNetworkResponse responseStatus
  = AssociateClientVpnTargetNetworkResponse'{associationId =
                                               Core.Nothing,
                                             status = Core.Nothing, responseStatus}

-- | The unique ID of the target network association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnrrsAssociationId :: Lens.Lens' AssociateClientVpnTargetNetworkResponse (Core.Maybe Core.Text)
acvtnrrsAssociationId = Lens.field @"associationId"
{-# INLINEABLE acvtnrrsAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The current state of the target network association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnrrsStatus :: Lens.Lens' AssociateClientVpnTargetNetworkResponse (Core.Maybe Types.AssociationStatus)
acvtnrrsStatus = Lens.field @"status"
{-# INLINEABLE acvtnrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acvtnrrsResponseStatus :: Lens.Lens' AssociateClientVpnTargetNetworkResponse Core.Int
acvtnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE acvtnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
