{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GameLift.StartFleetActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes activity on a fleet that was suspended with StopFleetActions.
-- Currently, this operation is used to restart a fleet\'s auto-scaling
-- activity.
--
-- To start fleet actions, specify the fleet ID and the type of actions to
-- restart. When auto-scaling fleet actions are restarted, Amazon GameLift
-- once again initiates scaling events as triggered by the fleet\'s scaling
-- policies. If actions on the fleet were never stopped, this operation
-- will have no effect. You can view a fleet\'s stopped actions using
-- DescribeFleetAttributes.
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
module Network.AWS.GameLift.StartFleetActions
  ( -- * Creating a Request
    StartFleetActions (..),
    newStartFleetActions,

    -- * Request Lenses
    startFleetActions_fleetId,
    startFleetActions_actions,

    -- * Destructuring the Response
    StartFleetActionsResponse (..),
    newStartFleetActionsResponse,

    -- * Response Lenses
    startFleetActionsResponse_httpStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartFleetActions' smart constructor.
data StartFleetActions = StartFleetActions'
  { -- | A unique identifier for a fleet to start actions on. You can use either
    -- the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | List of actions to restart on the fleet.
    actions :: Prelude.NonEmpty FleetAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartFleetActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'startFleetActions_fleetId' - A unique identifier for a fleet to start actions on. You can use either
-- the fleet ID or ARN value.
--
-- 'actions', 'startFleetActions_actions' - List of actions to restart on the fleet.
newStartFleetActions ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'actions'
  Prelude.NonEmpty FleetAction ->
  StartFleetActions
newStartFleetActions pFleetId_ pActions_ =
  StartFleetActions'
    { fleetId = pFleetId_,
      actions = Prelude._Coerce Lens.# pActions_
    }

-- | A unique identifier for a fleet to start actions on. You can use either
-- the fleet ID or ARN value.
startFleetActions_fleetId :: Lens.Lens' StartFleetActions Prelude.Text
startFleetActions_fleetId = Lens.lens (\StartFleetActions' {fleetId} -> fleetId) (\s@StartFleetActions' {} a -> s {fleetId = a} :: StartFleetActions)

-- | List of actions to restart on the fleet.
startFleetActions_actions :: Lens.Lens' StartFleetActions (Prelude.NonEmpty FleetAction)
startFleetActions_actions = Lens.lens (\StartFleetActions' {actions} -> actions) (\s@StartFleetActions' {} a -> s {actions = a} :: StartFleetActions) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest StartFleetActions where
  type Rs StartFleetActions = StartFleetActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartFleetActionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFleetActions

instance Prelude.NFData StartFleetActions

instance Prelude.ToHeaders StartFleetActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.StartFleetActions" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartFleetActions where
  toJSON StartFleetActions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Prelude..= fleetId),
            Prelude.Just ("Actions" Prelude..= actions)
          ]
      )

instance Prelude.ToPath StartFleetActions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartFleetActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFleetActionsResponse' smart constructor.
data StartFleetActionsResponse = StartFleetActionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartFleetActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startFleetActionsResponse_httpStatus' - The response's http status code.
newStartFleetActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFleetActionsResponse
newStartFleetActionsResponse pHttpStatus_ =
  StartFleetActionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startFleetActionsResponse_httpStatus :: Lens.Lens' StartFleetActionsResponse Prelude.Int
startFleetActionsResponse_httpStatus = Lens.lens (\StartFleetActionsResponse' {httpStatus} -> httpStatus) (\s@StartFleetActionsResponse' {} a -> s {httpStatus = a} :: StartFleetActionsResponse)

instance Prelude.NFData StartFleetActionsResponse
