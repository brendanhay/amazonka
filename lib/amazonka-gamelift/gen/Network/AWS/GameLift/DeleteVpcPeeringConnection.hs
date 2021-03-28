{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteVpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a VPC peering connection. To delete the connection, you must have a valid authorization for the VPC peering connection that you want to delete. You can check for an authorization by calling 'DescribeVpcPeeringAuthorizations' or request a new one using 'CreateVpcPeeringAuthorization' . 
--
-- Once a valid authorization exists, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the connection to delete by the connection ID and fleet ID. If successful, the connection is removed. 
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
module Network.AWS.GameLift.DeleteVpcPeeringConnection
    (
    -- * Creating a request
      DeleteVpcPeeringConnection (..)
    , mkDeleteVpcPeeringConnection
    -- ** Request lenses
    , dvpcFleetId
    , dvpcVpcPeeringConnectionId

    -- * Destructuring the response
    , DeleteVpcPeeringConnectionResponse (..)
    , mkDeleteVpcPeeringConnectionResponse
    -- ** Response lenses
    , dvpcrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteVpcPeeringConnection' smart constructor.
data DeleteVpcPeeringConnection = DeleteVpcPeeringConnection'
  { fleetId :: Types.FleetId
    -- ^ A unique identifier for a fleet. This fleet specified must match the fleet referenced in the VPC peering connection record. You can use either the fleet ID or ARN value.
  , vpcPeeringConnectionId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for a VPC peering connection. This value is included in the 'VpcPeeringConnection' object, which can be retrieved by calling 'DescribeVpcPeeringConnections' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcPeeringConnection' value with any optional fields omitted.
mkDeleteVpcPeeringConnection
    :: Types.FleetId -- ^ 'fleetId'
    -> Types.NonZeroAndMaxString -- ^ 'vpcPeeringConnectionId'
    -> DeleteVpcPeeringConnection
mkDeleteVpcPeeringConnection fleetId vpcPeeringConnectionId
  = DeleteVpcPeeringConnection'{fleetId, vpcPeeringConnectionId}

-- | A unique identifier for a fleet. This fleet specified must match the fleet referenced in the VPC peering connection record. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcFleetId :: Lens.Lens' DeleteVpcPeeringConnection Types.FleetId
dvpcFleetId = Lens.field @"fleetId"
{-# INLINEABLE dvpcFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for a VPC peering connection. This value is included in the 'VpcPeeringConnection' object, which can be retrieved by calling 'DescribeVpcPeeringConnections' .
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcVpcPeeringConnectionId :: Lens.Lens' DeleteVpcPeeringConnection Types.NonZeroAndMaxString
dvpcVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE dvpcVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.ToQuery DeleteVpcPeeringConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteVpcPeeringConnection where
        toHeaders DeleteVpcPeeringConnection{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeleteVpcPeeringConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteVpcPeeringConnection where
        toJSON DeleteVpcPeeringConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetId" Core..= fleetId),
                  Core.Just
                    ("VpcPeeringConnectionId" Core..= vpcPeeringConnectionId)])

instance Core.AWSRequest DeleteVpcPeeringConnection where
        type Rs DeleteVpcPeeringConnection =
             DeleteVpcPeeringConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteVpcPeeringConnectionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteVpcPeeringConnectionResponse' smart constructor.
newtype DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteVpcPeeringConnectionResponse' value with any optional fields omitted.
mkDeleteVpcPeeringConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteVpcPeeringConnectionResponse
mkDeleteVpcPeeringConnectionResponse responseStatus
  = DeleteVpcPeeringConnectionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrrsResponseStatus :: Lens.Lens' DeleteVpcPeeringConnectionResponse Core.Int
dvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
