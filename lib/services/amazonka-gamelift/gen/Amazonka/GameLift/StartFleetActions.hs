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
-- Module      : Amazonka.GameLift.StartFleetActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes certain types of activity on fleet instances that were suspended
-- with
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_StopFleetActions.html StopFleetActions>.
-- For multi-location fleets, fleet actions are managed separately for each
-- location. Currently, this operation is used to restart a fleet\'s
-- auto-scaling activity.
--
-- This operation can be used in the following ways:
--
-- -   To restart actions on instances in the fleet\'s home Region, provide
--     a fleet ID and the type of actions to resume.
--
-- -   To restart actions on instances in one of the fleet\'s remote
--     locations, provide a fleet ID, a location name, and the type of
--     actions to resume.
--
-- If successful, GameLift once again initiates scaling events as triggered
-- by the fleet\'s scaling policies. If actions on the fleet location were
-- never stopped, this operation will have no effect.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
module Amazonka.GameLift.StartFleetActions
  ( -- * Creating a Request
    StartFleetActions (..),
    newStartFleetActions,

    -- * Request Lenses
    startFleetActions_location,
    startFleetActions_fleetId,
    startFleetActions_actions,

    -- * Destructuring the Response
    StartFleetActionsResponse (..),
    newStartFleetActionsResponse,

    -- * Response Lenses
    startFleetActionsResponse_fleetArn,
    startFleetActionsResponse_fleetId,
    startFleetActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartFleetActions' smart constructor.
data StartFleetActions = StartFleetActions'
  { -- | The fleet location to restart fleet actions for. Specify a location in
    -- the form of an Amazon Web Services Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to restart actions on. You can use
    -- either the fleet ID or ARN value.
    fleetId :: Prelude.Text,
    -- | List of actions to restart on the fleet.
    actions :: Prelude.NonEmpty FleetAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFleetActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'startFleetActions_location' - The fleet location to restart fleet actions for. Specify a location in
-- the form of an Amazon Web Services Region code, such as @us-west-2@.
--
-- 'fleetId', 'startFleetActions_fleetId' - A unique identifier for the fleet to restart actions on. You can use
-- either the fleet ID or ARN value.
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
    { location = Prelude.Nothing,
      fleetId = pFleetId_,
      actions = Lens.coerced Lens.# pActions_
    }

-- | The fleet location to restart fleet actions for. Specify a location in
-- the form of an Amazon Web Services Region code, such as @us-west-2@.
startFleetActions_location :: Lens.Lens' StartFleetActions (Prelude.Maybe Prelude.Text)
startFleetActions_location = Lens.lens (\StartFleetActions' {location} -> location) (\s@StartFleetActions' {} a -> s {location = a} :: StartFleetActions)

-- | A unique identifier for the fleet to restart actions on. You can use
-- either the fleet ID or ARN value.
startFleetActions_fleetId :: Lens.Lens' StartFleetActions Prelude.Text
startFleetActions_fleetId = Lens.lens (\StartFleetActions' {fleetId} -> fleetId) (\s@StartFleetActions' {} a -> s {fleetId = a} :: StartFleetActions)

-- | List of actions to restart on the fleet.
startFleetActions_actions :: Lens.Lens' StartFleetActions (Prelude.NonEmpty FleetAction)
startFleetActions_actions = Lens.lens (\StartFleetActions' {actions} -> actions) (\s@StartFleetActions' {} a -> s {actions = a} :: StartFleetActions) Prelude.. Lens.coerced

instance Core.AWSRequest StartFleetActions where
  type
    AWSResponse StartFleetActions =
      StartFleetActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartFleetActionsResponse'
            Prelude.<$> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartFleetActions where
  hashWithSalt _salt StartFleetActions' {..} =
    _salt
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` actions

instance Prelude.NFData StartFleetActions where
  rnf StartFleetActions' {..} =
    Prelude.rnf location `Prelude.seq`
      Prelude.rnf fleetId `Prelude.seq`
        Prelude.rnf actions

instance Data.ToHeaders StartFleetActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.StartFleetActions" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartFleetActions where
  toJSON StartFleetActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Location" Data..=) Prelude.<$> location,
            Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("Actions" Data..= actions)
          ]
      )

instance Data.ToPath StartFleetActions where
  toPath = Prelude.const "/"

instance Data.ToQuery StartFleetActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartFleetActionsResponse' smart constructor.
data StartFleetActionsResponse = StartFleetActionsResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet to restart actions on.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartFleetActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'startFleetActionsResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'startFleetActionsResponse_fleetId' - A unique identifier for the fleet to restart actions on.
--
-- 'httpStatus', 'startFleetActionsResponse_httpStatus' - The response's http status code.
newStartFleetActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartFleetActionsResponse
newStartFleetActionsResponse pHttpStatus_ =
  StartFleetActionsResponse'
    { fleetArn =
        Prelude.Nothing,
      fleetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
startFleetActionsResponse_fleetArn :: Lens.Lens' StartFleetActionsResponse (Prelude.Maybe Prelude.Text)
startFleetActionsResponse_fleetArn = Lens.lens (\StartFleetActionsResponse' {fleetArn} -> fleetArn) (\s@StartFleetActionsResponse' {} a -> s {fleetArn = a} :: StartFleetActionsResponse)

-- | A unique identifier for the fleet to restart actions on.
startFleetActionsResponse_fleetId :: Lens.Lens' StartFleetActionsResponse (Prelude.Maybe Prelude.Text)
startFleetActionsResponse_fleetId = Lens.lens (\StartFleetActionsResponse' {fleetId} -> fleetId) (\s@StartFleetActionsResponse' {} a -> s {fleetId = a} :: StartFleetActionsResponse)

-- | The response's http status code.
startFleetActionsResponse_httpStatus :: Lens.Lens' StartFleetActionsResponse Prelude.Int
startFleetActionsResponse_httpStatus = Lens.lens (\StartFleetActionsResponse' {httpStatus} -> httpStatus) (\s@StartFleetActionsResponse' {} a -> s {httpStatus = a} :: StartFleetActionsResponse)

instance Prelude.NFData StartFleetActionsResponse where
  rnf StartFleetActionsResponse' {..} =
    Prelude.rnf fleetArn `Prelude.seq`
      Prelude.rnf fleetId `Prelude.seq`
        Prelude.rnf httpStatus
