{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StartFleetActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes activity on a fleet that was suspended with 'StopFleetActions' . Currently, this operation is used to restart a fleet's auto-scaling activity. 
--
-- To start fleet actions, specify the fleet ID and the type of actions to restart. When auto-scaling fleet actions are restarted, Amazon GameLift once again initiates scaling events as triggered by the fleet's scaling policies. If actions on the fleet were never stopped, this operation will have no effect. You can view a fleet's stopped actions using 'DescribeFleetAttributes' .
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
module Network.AWS.GameLift.StartFleetActions
    (
    -- * Creating a request
      StartFleetActions (..)
    , mkStartFleetActions
    -- ** Request lenses
    , sfaFleetId
    , sfaActions

    -- * Destructuring the response
    , StartFleetActionsResponse (..)
    , mkStartFleetActionsResponse
    -- ** Response lenses
    , sfarrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartFleetActions' smart constructor.
data StartFleetActions = StartFleetActions'
  { fleetId :: Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to start actions on. You can use either the fleet ID or ARN value.
  , actions :: Core.NonEmpty Types.FleetAction
    -- ^ List of actions to restart on the fleet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartFleetActions' value with any optional fields omitted.
mkStartFleetActions
    :: Types.FleetIdOrArn -- ^ 'fleetId'
    -> Core.NonEmpty Types.FleetAction -- ^ 'actions'
    -> StartFleetActions
mkStartFleetActions fleetId actions
  = StartFleetActions'{fleetId, actions}

-- | A unique identifier for a fleet to start actions on. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaFleetId :: Lens.Lens' StartFleetActions Types.FleetIdOrArn
sfaFleetId = Lens.field @"fleetId"
{-# INLINEABLE sfaFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | List of actions to restart on the fleet.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaActions :: Lens.Lens' StartFleetActions (Core.NonEmpty Types.FleetAction)
sfaActions = Lens.field @"actions"
{-# INLINEABLE sfaActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

instance Core.ToQuery StartFleetActions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartFleetActions where
        toHeaders StartFleetActions{..}
          = Core.pure ("X-Amz-Target", "GameLift.StartFleetActions") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartFleetActions where
        toJSON StartFleetActions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetId" Core..= fleetId),
                  Core.Just ("Actions" Core..= actions)])

instance Core.AWSRequest StartFleetActions where
        type Rs StartFleetActions = StartFleetActionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartFleetActionsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartFleetActionsResponse' smart constructor.
newtype StartFleetActionsResponse = StartFleetActionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartFleetActionsResponse' value with any optional fields omitted.
mkStartFleetActionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartFleetActionsResponse
mkStartFleetActionsResponse responseStatus
  = StartFleetActionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfarrsResponseStatus :: Lens.Lens' StartFleetActionsResponse Core.Int
sfarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sfarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
