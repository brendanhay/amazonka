{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateVpcPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests authorization to create or delete a peer connection between the VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in your AWS account. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. Once you've received authorization, call 'CreateVpcPeeringConnection' to establish the peering connection. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- You can peer with VPCs that are owned by any AWS account you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different Regions.
-- To request authorization to create a connection, call this operation from the AWS account with the VPC that you want to peer to your Amazon GameLift fleet. For example, to enable your game servers to retrieve data from a DynamoDB table, use the account that manages that DynamoDB resource. Identify the following values: (1) The ID of the VPC that you want to peer with, and (2) the ID of the AWS account that you use to manage Amazon GameLift. If successful, VPC peering is authorized for the specified VPC. 
-- To request authorization to delete a connection, call this operation from the AWS account with the VPC that is peered with your Amazon GameLift fleet. Identify the following values: (1) VPC ID that you want to delete the peering connection for, and (2) ID of the AWS account that you use to manage Amazon GameLift. 
-- The authorization remains valid for 24 hours unless it is canceled by a call to 'DeleteVpcPeeringAuthorization' . You must create or delete the peering connection while the authorization is valid. 
--
--     * 'CreateVpcPeeringAuthorization' 
--
--
--     * 'DescribeVpcPeeringAuthorizations' 
--
--
--     * 'DeleteVpcPeeringAuthorization' 
--
--
--     * 'CreateVpcPeeringConnection' 
--
--
--     * 'DescribeVpcPeeringConnections' 
--
--
--     * 'DeleteVpcPeeringConnection' 
--
--
module Network.AWS.GameLift.CreateVpcPeeringAuthorization
    (
    -- * Creating a request
      CreateVpcPeeringAuthorization (..)
    , mkCreateVpcPeeringAuthorization
    -- ** Request lenses
    , cvpaGameLiftAwsAccountId
    , cvpaPeerVpcId

    -- * Destructuring the response
    , CreateVpcPeeringAuthorizationResponse (..)
    , mkCreateVpcPeeringAuthorizationResponse
    -- ** Response lenses
    , cvparrsVpcPeeringAuthorization
    , cvparrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateVpcPeeringAuthorization' smart constructor.
data CreateVpcPeeringAuthorization = CreateVpcPeeringAuthorization'
  { gameLiftAwsAccountId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
  , peerVpcId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcPeeringAuthorization' value with any optional fields omitted.
mkCreateVpcPeeringAuthorization
    :: Types.NonZeroAndMaxString -- ^ 'gameLiftAwsAccountId'
    -> Types.NonZeroAndMaxString -- ^ 'peerVpcId'
    -> CreateVpcPeeringAuthorization
mkCreateVpcPeeringAuthorization gameLiftAwsAccountId peerVpcId
  = CreateVpcPeeringAuthorization'{gameLiftAwsAccountId, peerVpcId}

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'gameLiftAwsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpaGameLiftAwsAccountId :: Lens.Lens' CreateVpcPeeringAuthorization Types.NonZeroAndMaxString
cvpaGameLiftAwsAccountId = Lens.field @"gameLiftAwsAccountId"
{-# INLINEABLE cvpaGameLiftAwsAccountId #-}
{-# DEPRECATED gameLiftAwsAccountId "Use generic-lens or generic-optics with 'gameLiftAwsAccountId' instead"  #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpaPeerVpcId :: Lens.Lens' CreateVpcPeeringAuthorization Types.NonZeroAndMaxString
cvpaPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE cvpaPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

instance Core.ToQuery CreateVpcPeeringAuthorization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateVpcPeeringAuthorization where
        toHeaders CreateVpcPeeringAuthorization{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.CreateVpcPeeringAuthorization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateVpcPeeringAuthorization where
        toJSON CreateVpcPeeringAuthorization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameLiftAwsAccountId" Core..= gameLiftAwsAccountId),
                  Core.Just ("PeerVpcId" Core..= peerVpcId)])

instance Core.AWSRequest CreateVpcPeeringAuthorization where
        type Rs CreateVpcPeeringAuthorization =
             CreateVpcPeeringAuthorizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateVpcPeeringAuthorizationResponse' Core.<$>
                   (x Core..:? "VpcPeeringAuthorization") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateVpcPeeringAuthorizationResponse' smart constructor.
data CreateVpcPeeringAuthorizationResponse = CreateVpcPeeringAuthorizationResponse'
  { vpcPeeringAuthorization :: Core.Maybe Types.VpcPeeringAuthorization
    -- ^ Details on the requested VPC peering authorization, including expiration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateVpcPeeringAuthorizationResponse' value with any optional fields omitted.
mkCreateVpcPeeringAuthorizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpcPeeringAuthorizationResponse
mkCreateVpcPeeringAuthorizationResponse responseStatus
  = CreateVpcPeeringAuthorizationResponse'{vpcPeeringAuthorization =
                                             Core.Nothing,
                                           responseStatus}

-- | Details on the requested VPC peering authorization, including expiration.
--
-- /Note:/ Consider using 'vpcPeeringAuthorization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvparrsVpcPeeringAuthorization :: Lens.Lens' CreateVpcPeeringAuthorizationResponse (Core.Maybe Types.VpcPeeringAuthorization)
cvparrsVpcPeeringAuthorization = Lens.field @"vpcPeeringAuthorization"
{-# INLINEABLE cvparrsVpcPeeringAuthorization #-}
{-# DEPRECATED vpcPeeringAuthorization "Use generic-lens or generic-optics with 'vpcPeeringAuthorization' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvparrsResponseStatus :: Lens.Lens' CreateVpcPeeringAuthorizationResponse Core.Int
cvparrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvparrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
