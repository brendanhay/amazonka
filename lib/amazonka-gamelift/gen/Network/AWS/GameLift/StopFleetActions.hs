{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StopFleetActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends activity on a fleet. Currently, this operation is used to stop a fleet's auto-scaling activity. It is used to temporarily stop triggering scaling events. The policies can be retained and auto-scaling activity can be restarted using 'StartFleetActions' . You can view a fleet's stopped actions using 'DescribeFleetAttributes' .
--
-- To stop fleet actions, specify the fleet ID and the type of actions to suspend. When auto-scaling fleet actions are stopped, Amazon GameLift no longer initiates scaling events except in response to manual changes using 'UpdateFleetCapacity' .
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
module Network.AWS.GameLift.StopFleetActions
  ( -- * Creating a request
    StopFleetActions (..),
    mkStopFleetActions,

    -- ** Request lenses
    sFleetId,
    sActions,

    -- * Destructuring the response
    StopFleetActionsResponse (..),
    mkStopFleetActionsResponse,

    -- ** Response lenses
    sfarfrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopFleetActions' smart constructor.
data StopFleetActions = StopFleetActions'
  { -- | A unique identifier for a fleet to stop actions on. You can use either the fleet ID or ARN value.
    fleetId :: Types.FleetIdOrArn,
    -- | List of actions to suspend on the fleet.
    actions :: Core.NonEmpty Types.FleetAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopFleetActions' value with any optional fields omitted.
mkStopFleetActions ::
  -- | 'fleetId'
  Types.FleetIdOrArn ->
  -- | 'actions'
  Core.NonEmpty Types.FleetAction ->
  StopFleetActions
mkStopFleetActions fleetId actions =
  StopFleetActions' {fleetId, actions}

-- | A unique identifier for a fleet to stop actions on. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFleetId :: Lens.Lens' StopFleetActions Types.FleetIdOrArn
sFleetId = Lens.field @"fleetId"
{-# DEPRECATED sFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | List of actions to suspend on the fleet.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActions :: Lens.Lens' StopFleetActions (Core.NonEmpty Types.FleetAction)
sActions = Lens.field @"actions"
{-# DEPRECATED sActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Core.FromJSON StopFleetActions where
  toJSON StopFleetActions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            Core.Just ("Actions" Core..= actions)
          ]
      )

instance Core.AWSRequest StopFleetActions where
  type Rs StopFleetActions = StopFleetActionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.StopFleetActions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopFleetActionsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopFleetActionsResponse' smart constructor.
newtype StopFleetActionsResponse = StopFleetActionsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopFleetActionsResponse' value with any optional fields omitted.
mkStopFleetActionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopFleetActionsResponse
mkStopFleetActionsResponse responseStatus =
  StopFleetActionsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfarfrsResponseStatus :: Lens.Lens' StopFleetActionsResponse Core.Int
sfarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sfarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
