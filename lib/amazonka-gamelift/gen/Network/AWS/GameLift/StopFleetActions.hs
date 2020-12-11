{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    sfasrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopFleetActions' smart constructor.
data StopFleetActions = StopFleetActions'
  { fleetId :: Lude.Text,
    actions :: Lude.NonEmpty FleetAction
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopFleetActions' with the minimum fields required to make a request.
--
-- * 'actions' - List of actions to suspend on the fleet.
-- * 'fleetId' - A unique identifier for a fleet to stop actions on. You can use either the fleet ID or ARN value.
mkStopFleetActions ::
  -- | 'fleetId'
  Lude.Text ->
  -- | 'actions'
  Lude.NonEmpty FleetAction ->
  StopFleetActions
mkStopFleetActions pFleetId_ pActions_ =
  StopFleetActions' {fleetId = pFleetId_, actions = pActions_}

-- | A unique identifier for a fleet to stop actions on. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sFleetId :: Lens.Lens' StopFleetActions Lude.Text
sFleetId = Lens.lens (fleetId :: StopFleetActions -> Lude.Text) (\s a -> s {fleetId = a} :: StopFleetActions)
{-# DEPRECATED sFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

-- | List of actions to suspend on the fleet.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sActions :: Lens.Lens' StopFleetActions (Lude.NonEmpty FleetAction)
sActions = Lens.lens (actions :: StopFleetActions -> Lude.NonEmpty FleetAction) (\s a -> s {actions = a} :: StopFleetActions)
{-# DEPRECATED sActions "Use generic-lens or generic-optics with 'actions' instead." #-}

instance Lude.AWSRequest StopFleetActions where
  type Rs StopFleetActions = StopFleetActionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopFleetActionsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopFleetActions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StopFleetActions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopFleetActions where
  toJSON StopFleetActions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FleetId" Lude..= fleetId),
            Lude.Just ("Actions" Lude..= actions)
          ]
      )

instance Lude.ToPath StopFleetActions where
  toPath = Lude.const "/"

instance Lude.ToQuery StopFleetActions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopFleetActionsResponse' smart constructor.
newtype StopFleetActionsResponse = StopFleetActionsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopFleetActionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopFleetActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopFleetActionsResponse
mkStopFleetActionsResponse pResponseStatus_ =
  StopFleetActionsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfasrsResponseStatus :: Lens.Lens' StopFleetActionsResponse Lude.Int
sfasrsResponseStatus = Lens.lens (responseStatus :: StopFleetActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopFleetActionsResponse)
{-# DEPRECATED sfasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
