{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.GameLift.StartFleetActions
  ( -- * Creating a request
    StartFleetActions (..),
    mkStartFleetActions,

    -- ** Request lenses
    sfaActions,
    sfaFleetId,

    -- * Destructuring the response
    StartFleetActionsResponse (..),
    mkStartFleetActionsResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartFleetActions' smart constructor.
data StartFleetActions = StartFleetActions'
  { -- | List of actions to restart on the fleet.
    actions :: Lude.NonEmpty FleetAction,
    -- | A unique identifier for a fleet to start actions on. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFleetActions' with the minimum fields required to make a request.
--
-- * 'actions' - List of actions to restart on the fleet.
-- * 'fleetId' - A unique identifier for a fleet to start actions on. You can use either the fleet ID or ARN value.
mkStartFleetActions ::
  -- | 'actions'
  Lude.NonEmpty FleetAction ->
  -- | 'fleetId'
  Lude.Text ->
  StartFleetActions
mkStartFleetActions pActions_ pFleetId_ =
  StartFleetActions' {actions = pActions_, fleetId = pFleetId_}

-- | List of actions to restart on the fleet.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaActions :: Lens.Lens' StartFleetActions (Lude.NonEmpty FleetAction)
sfaActions = Lens.lens (actions :: StartFleetActions -> Lude.NonEmpty FleetAction) (\s a -> s {actions = a} :: StartFleetActions)
{-# DEPRECATED sfaActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | A unique identifier for a fleet to start actions on. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfaFleetId :: Lens.Lens' StartFleetActions Lude.Text
sfaFleetId = Lens.lens (fleetId :: StartFleetActions -> Lude.Text) (\s a -> s {fleetId = a} :: StartFleetActions)
{-# DEPRECATED sfaFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest StartFleetActions where
  type Rs StartFleetActions = StartFleetActionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartFleetActionsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartFleetActions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StartFleetActions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartFleetActions where
  toJSON StartFleetActions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Actions" Lude..= actions),
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath StartFleetActions where
  toPath = Lude.const "/"

instance Lude.ToQuery StartFleetActions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartFleetActionsResponse' smart constructor.
newtype StartFleetActionsResponse = StartFleetActionsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartFleetActionsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartFleetActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartFleetActionsResponse
mkStartFleetActionsResponse pResponseStatus_ =
  StartFleetActionsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartFleetActionsResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartFleetActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartFleetActionsResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
