{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a security group to the association between the target network and the Client VPN endpoint. This action replaces the existing security groups with the specified security groups.
module Network.AWS.EC2.ApplySecurityGroupsToClientVpnTargetNetwork
    (
    -- * Creating a request
      ApplySecurityGroupsToClientVpnTargetNetwork (..)
    , mkApplySecurityGroupsToClientVpnTargetNetwork
    -- ** Request lenses
    , asgtcvtnClientVpnEndpointId
    , asgtcvtnVpcId
    , asgtcvtnSecurityGroupIds
    , asgtcvtnDryRun

    -- * Destructuring the response
    , ApplySecurityGroupsToClientVpnTargetNetworkResponse (..)
    , mkApplySecurityGroupsToClientVpnTargetNetworkResponse
    -- ** Response lenses
    , asgtcvtnrrsSecurityGroupIds
    , asgtcvtnrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkApplySecurityGroupsToClientVpnTargetNetwork' smart constructor.
data ApplySecurityGroupsToClientVpnTargetNetwork = ApplySecurityGroupsToClientVpnTargetNetwork'
  { clientVpnEndpointId :: Types.ClientVpnEndpointId
    -- ^ The ID of the Client VPN endpoint.
  , vpcId :: Types.VpcId
    -- ^ The ID of the VPC in which the associated target network is located.
  , securityGroupIds :: [Types.SecurityGroupId]
    -- ^ The IDs of the security groups to apply to the associated target network. Up to 5 security groups can be applied to an associated target network.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplySecurityGroupsToClientVpnTargetNetwork' value with any optional fields omitted.
mkApplySecurityGroupsToClientVpnTargetNetwork
    :: Types.ClientVpnEndpointId -- ^ 'clientVpnEndpointId'
    -> Types.VpcId -- ^ 'vpcId'
    -> ApplySecurityGroupsToClientVpnTargetNetwork
mkApplySecurityGroupsToClientVpnTargetNetwork clientVpnEndpointId
  vpcId
  = ApplySecurityGroupsToClientVpnTargetNetwork'{clientVpnEndpointId,
                                                 vpcId, securityGroupIds = Core.mempty,
                                                 dryRun = Core.Nothing}

-- | The ID of the Client VPN endpoint.
--
-- /Note:/ Consider using 'clientVpnEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnClientVpnEndpointId :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork Types.ClientVpnEndpointId
asgtcvtnClientVpnEndpointId = Lens.field @"clientVpnEndpointId"
{-# INLINEABLE asgtcvtnClientVpnEndpointId #-}
{-# DEPRECATED clientVpnEndpointId "Use generic-lens or generic-optics with 'clientVpnEndpointId' instead"  #-}

-- | The ID of the VPC in which the associated target network is located.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnVpcId :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork Types.VpcId
asgtcvtnVpcId = Lens.field @"vpcId"
{-# INLINEABLE asgtcvtnVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The IDs of the security groups to apply to the associated target network. Up to 5 security groups can be applied to an associated target network.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnSecurityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork [Types.SecurityGroupId]
asgtcvtnSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE asgtcvtnSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnDryRun :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetwork (Core.Maybe Core.Bool)
asgtcvtnDryRun = Lens.field @"dryRun"
{-# INLINEABLE asgtcvtnDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery ApplySecurityGroupsToClientVpnTargetNetwork
         where
        toQuery ApplySecurityGroupsToClientVpnTargetNetwork{..}
          = Core.toQueryPair "Action"
              ("ApplySecurityGroupsToClientVpnTargetNetwork" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ClientVpnEndpointId" clientVpnEndpointId
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<> Core.toQueryList "SecurityGroupId" securityGroupIds
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders ApplySecurityGroupsToClientVpnTargetNetwork
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest
           ApplySecurityGroupsToClientVpnTargetNetwork
         where
        type Rs ApplySecurityGroupsToClientVpnTargetNetwork =
             ApplySecurityGroupsToClientVpnTargetNetworkResponse
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
                 ApplySecurityGroupsToClientVpnTargetNetworkResponse' Core.<$>
                   (x Core..@? "securityGroupIds" Core..<@> Core.parseXMLList "item")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkApplySecurityGroupsToClientVpnTargetNetworkResponse' smart constructor.
data ApplySecurityGroupsToClientVpnTargetNetworkResponse = ApplySecurityGroupsToClientVpnTargetNetworkResponse'
  { securityGroupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the applied security groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplySecurityGroupsToClientVpnTargetNetworkResponse' value with any optional fields omitted.
mkApplySecurityGroupsToClientVpnTargetNetworkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ApplySecurityGroupsToClientVpnTargetNetworkResponse
mkApplySecurityGroupsToClientVpnTargetNetworkResponse
  responseStatus
  = ApplySecurityGroupsToClientVpnTargetNetworkResponse'{securityGroupIds
                                                           = Core.Nothing,
                                                         responseStatus}

-- | The IDs of the applied security groups.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnrrsSecurityGroupIds :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetworkResponse (Core.Maybe [Types.SecurityGroupId])
asgtcvtnrrsSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE asgtcvtnrrsSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgtcvtnrrsResponseStatus :: Lens.Lens' ApplySecurityGroupsToClientVpnTargetNetworkResponse Core.Int
asgtcvtnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asgtcvtnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
