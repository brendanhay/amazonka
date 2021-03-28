{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteVpcPeeringAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending VPC peering authorization for the specified VPC. If you need to delete an existing VPC peering connection, call 'DeleteVpcPeeringConnection' . 
--
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
module Network.AWS.GameLift.DeleteVpcPeeringAuthorization
    (
    -- * Creating a request
      DeleteVpcPeeringAuthorization (..)
    , mkDeleteVpcPeeringAuthorization
    -- ** Request lenses
    , dvpaGameLiftAwsAccountId
    , dvpaPeerVpcId

    -- * Destructuring the response
    , DeleteVpcPeeringAuthorizationResponse (..)
    , mkDeleteVpcPeeringAuthorizationResponse
    -- ** Response lenses
    , dvparfrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteVpcPeeringAuthorization' smart constructor.
data DeleteVpcPeeringAuthorization = DeleteVpcPeeringAuthorization'
  { gameLiftAwsAccountId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
  , peerVpcId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcPeeringAuthorization' value with any optional fields omitted.
mkDeleteVpcPeeringAuthorization
    :: Types.NonZeroAndMaxString -- ^ 'gameLiftAwsAccountId'
    -> Types.NonZeroAndMaxString -- ^ 'peerVpcId'
    -> DeleteVpcPeeringAuthorization
mkDeleteVpcPeeringAuthorization gameLiftAwsAccountId peerVpcId
  = DeleteVpcPeeringAuthorization'{gameLiftAwsAccountId, peerVpcId}

-- | A unique identifier for the AWS account that you use to manage your Amazon GameLift fleet. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'gameLiftAwsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpaGameLiftAwsAccountId :: Lens.Lens' DeleteVpcPeeringAuthorization Types.NonZeroAndMaxString
dvpaGameLiftAwsAccountId = Lens.field @"gameLiftAwsAccountId"
{-# INLINEABLE dvpaGameLiftAwsAccountId #-}
{-# DEPRECATED gameLiftAwsAccountId "Use generic-lens or generic-optics with 'gameLiftAwsAccountId' instead"  #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpaPeerVpcId :: Lens.Lens' DeleteVpcPeeringAuthorization Types.NonZeroAndMaxString
dvpaPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE dvpaPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

instance Core.ToQuery DeleteVpcPeeringAuthorization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVpcPeeringAuthorization where
        toHeaders DeleteVpcPeeringAuthorization{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.DeleteVpcPeeringAuthorization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteVpcPeeringAuthorization where
        toJSON DeleteVpcPeeringAuthorization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameLiftAwsAccountId" Core..= gameLiftAwsAccountId),
                  Core.Just ("PeerVpcId" Core..= peerVpcId)])

instance Core.AWSRequest DeleteVpcPeeringAuthorization where
        type Rs DeleteVpcPeeringAuthorization =
             DeleteVpcPeeringAuthorizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteVpcPeeringAuthorizationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcPeeringAuthorizationResponse' smart constructor.
newtype DeleteVpcPeeringAuthorizationResponse = DeleteVpcPeeringAuthorizationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcPeeringAuthorizationResponse' value with any optional fields omitted.
mkDeleteVpcPeeringAuthorizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVpcPeeringAuthorizationResponse
mkDeleteVpcPeeringAuthorizationResponse responseStatus
  = DeleteVpcPeeringAuthorizationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvparfrsResponseStatus :: Lens.Lens' DeleteVpcPeeringAuthorizationResponse Core.Int
dvparfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvparfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
