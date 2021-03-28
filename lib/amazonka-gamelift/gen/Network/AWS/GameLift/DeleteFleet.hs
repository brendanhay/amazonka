{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteFleet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes everything related to a fleet. Before deleting a fleet, you must set the fleet's desired capacity to zero. See 'UpdateFleetCapacity' .
--
-- If the fleet being deleted has a VPC peering connection, you first need to get a valid authorization (good for 24 hours) by calling 'CreateVpcPeeringAuthorization' . You do not need to explicitly delete the VPC peering connection--this is done as part of the delete fleet process.
-- This operation removes the fleet and its resources. Once a fleet is deleted, you can no longer use any of the resource in that fleet.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets> 
-- __Related operations__ 
--
--     * 'CreateFleet' 
--
--
--     * 'ListFleets' 
--
--
--     * 'DeleteFleet' 
--
--
--     * 'DescribeFleetAttributes' 
--
--
--     * 'UpdateFleetAttributes' 
--
--
--     * 'StartFleetActions' or 'StopFleetActions' 
--
--
module Network.AWS.GameLift.DeleteFleet
    (
    -- * Creating a request
      DeleteFleet (..)
    , mkDeleteFleet
    -- ** Request lenses
    , dfFleetId

    -- * Destructuring the response
    , DeleteFleetResponse (..)
    , mkDeleteFleetResponse
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteFleet' smart constructor.
newtype DeleteFleet = DeleteFleet'
  { fleetId :: Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleet' value with any optional fields omitted.
mkDeleteFleet
    :: Types.FleetIdOrArn -- ^ 'fleetId'
    -> DeleteFleet
mkDeleteFleet fleetId = DeleteFleet'{fleetId}

-- | A unique identifier for a fleet to be deleted. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFleetId :: Lens.Lens' DeleteFleet Types.FleetIdOrArn
dfFleetId = Lens.field @"fleetId"
{-# INLINEABLE dfFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

instance Core.ToQuery DeleteFleet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteFleet where
        toHeaders DeleteFleet{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeleteFleet") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteFleet where
        toJSON DeleteFleet{..}
          = Core.object
              (Core.catMaybes [Core.Just ("FleetId" Core..= fleetId)])

instance Core.AWSRequest DeleteFleet where
        type Rs DeleteFleet = DeleteFleetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteFleetResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFleetResponse' value with any optional fields omitted.
mkDeleteFleetResponse
    :: DeleteFleetResponse
mkDeleteFleetResponse = DeleteFleetResponse'
