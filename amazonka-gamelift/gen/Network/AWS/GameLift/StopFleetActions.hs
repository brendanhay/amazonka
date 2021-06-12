{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StopFleetActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Suspends activity on a fleet. Currently, this operation is used to stop
-- a fleet\'s auto-scaling activity. It is used to temporarily stop
-- triggering scaling events. The policies can be retained and auto-scaling
-- activity can be restarted using StartFleetActions. You can view a
-- fleet\'s stopped actions using DescribeFleetAttributes.
--
-- To stop fleet actions, specify the fleet ID and the type of actions to
-- suspend. When auto-scaling fleet actions are stopped, Amazon GameLift no
-- longer initiates scaling events except in response to manual changes
-- using UpdateFleetCapacity.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
-- -   CreateFleet
--
-- -   ListFleets
--
-- -   DeleteFleet
--
-- -   DescribeFleetAttributes
--
-- -   UpdateFleetAttributes
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.StopFleetActions
  ( -- * Creating a Request
    StopFleetActions (..),
    newStopFleetActions,

    -- * Request Lenses
    stopFleetActions_fleetId,
    stopFleetActions_actions,

    -- * Destructuring the Response
    StopFleetActionsResponse (..),
    newStopFleetActionsResponse,

    -- * Response Lenses
    stopFleetActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopFleetActions' smart constructor.
data StopFleetActions = StopFleetActions'
  { -- | A unique identifier for a fleet to stop actions on. You can use either
    -- the fleet ID or ARN value.
    fleetId :: Core.Text,
    -- | List of actions to suspend on the fleet.
    actions :: Core.NonEmpty FleetAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopFleetActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'stopFleetActions_fleetId' - A unique identifier for a fleet to stop actions on. You can use either
-- the fleet ID or ARN value.
--
-- 'actions', 'stopFleetActions_actions' - List of actions to suspend on the fleet.
newStopFleetActions ::
  -- | 'fleetId'
  Core.Text ->
  -- | 'actions'
  Core.NonEmpty FleetAction ->
  StopFleetActions
newStopFleetActions pFleetId_ pActions_ =
  StopFleetActions'
    { fleetId = pFleetId_,
      actions = Lens._Coerce Lens.# pActions_
    }

-- | A unique identifier for a fleet to stop actions on. You can use either
-- the fleet ID or ARN value.
stopFleetActions_fleetId :: Lens.Lens' StopFleetActions Core.Text
stopFleetActions_fleetId = Lens.lens (\StopFleetActions' {fleetId} -> fleetId) (\s@StopFleetActions' {} a -> s {fleetId = a} :: StopFleetActions)

-- | List of actions to suspend on the fleet.
stopFleetActions_actions :: Lens.Lens' StopFleetActions (Core.NonEmpty FleetAction)
stopFleetActions_actions = Lens.lens (\StopFleetActions' {actions} -> actions) (\s@StopFleetActions' {} a -> s {actions = a} :: StopFleetActions) Core.. Lens._Coerce

instance Core.AWSRequest StopFleetActions where
  type
    AWSResponse StopFleetActions =
      StopFleetActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopFleetActionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopFleetActions

instance Core.NFData StopFleetActions

instance Core.ToHeaders StopFleetActions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.StopFleetActions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopFleetActions where
  toJSON StopFleetActions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetId" Core..= fleetId),
            Core.Just ("Actions" Core..= actions)
          ]
      )

instance Core.ToPath StopFleetActions where
  toPath = Core.const "/"

instance Core.ToQuery StopFleetActions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopFleetActionsResponse' smart constructor.
data StopFleetActionsResponse = StopFleetActionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopFleetActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopFleetActionsResponse_httpStatus' - The response's http status code.
newStopFleetActionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopFleetActionsResponse
newStopFleetActionsResponse pHttpStatus_ =
  StopFleetActionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
stopFleetActionsResponse_httpStatus :: Lens.Lens' StopFleetActionsResponse Core.Int
stopFleetActionsResponse_httpStatus = Lens.lens (\StopFleetActionsResponse' {httpStatus} -> httpStatus) (\s@StopFleetActionsResponse' {} a -> s {httpStatus = a} :: StopFleetActionsResponse)

instance Core.NFData StopFleetActionsResponse
