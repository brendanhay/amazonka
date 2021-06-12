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
-- Module      : Network.AWS.GameLift.UpdateFleetAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates fleet properties, including name and description, for a fleet.
-- To update metadata, specify the fleet ID and the property values that
-- you want to change. If successful, the fleet ID for the updated fleet is
-- returned.
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
-- -   Update fleets:
--
--     -   UpdateFleetAttributes
--
--     -   UpdateFleetCapacity
--
--     -   UpdateFleetPortSettings
--
--     -   UpdateRuntimeConfiguration
--
-- -   StartFleetActions or StopFleetActions
module Network.AWS.GameLift.UpdateFleetAttributes
  ( -- * Creating a Request
    UpdateFleetAttributes (..),
    newUpdateFleetAttributes,

    -- * Request Lenses
    updateFleetAttributes_newGameSessionProtectionPolicy,
    updateFleetAttributes_name,
    updateFleetAttributes_description,
    updateFleetAttributes_resourceCreationLimitPolicy,
    updateFleetAttributes_metricGroups,
    updateFleetAttributes_fleetId,

    -- * Destructuring the Response
    UpdateFleetAttributesResponse (..),
    newUpdateFleetAttributesResponse,

    -- * Response Lenses
    updateFleetAttributesResponse_fleetId,
    updateFleetAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newUpdateFleetAttributes' smart constructor.
data UpdateFleetAttributes = UpdateFleetAttributes'
  { -- | Game session protection policy to apply to all new instances created in
    -- this fleet. Instances that already exist are not affected. You can set
    -- protection for individual instances using UpdateGameSession.
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Core.Maybe ProtectionPolicy,
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Core.Maybe Core.Text,
    -- | Human-readable description of a fleet.
    description :: Core.Maybe Core.Text,
    -- | Policy that limits the number of game sessions an individual player can
    -- create over a span of time.
    resourceCreationLimitPolicy :: Core.Maybe ResourceCreationLimitPolicy,
    -- | Names of metric groups to include this fleet in. Amazon CloudWatch uses
    -- a fleet metric group is to aggregate metrics from multiple fleets. Use
    -- an existing metric group name to add this fleet to the group. Or use a
    -- new name to create a new metric group. A fleet can only be included in
    -- one metric group at a time.
    metricGroups :: Core.Maybe [Core.Text],
    -- | A unique identifier for a fleet to update attribute metadata for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newGameSessionProtectionPolicy'', 'updateFleetAttributes_newGameSessionProtectionPolicy' - Game session protection policy to apply to all new instances created in
-- this fleet. Instances that already exist are not affected. You can set
-- protection for individual instances using UpdateGameSession.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
--
-- 'name', 'updateFleetAttributes_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'description', 'updateFleetAttributes_description' - Human-readable description of a fleet.
--
-- 'resourceCreationLimitPolicy', 'updateFleetAttributes_resourceCreationLimitPolicy' - Policy that limits the number of game sessions an individual player can
-- create over a span of time.
--
-- 'metricGroups', 'updateFleetAttributes_metricGroups' - Names of metric groups to include this fleet in. Amazon CloudWatch uses
-- a fleet metric group is to aggregate metrics from multiple fleets. Use
-- an existing metric group name to add this fleet to the group. Or use a
-- new name to create a new metric group. A fleet can only be included in
-- one metric group at a time.
--
-- 'fleetId', 'updateFleetAttributes_fleetId' - A unique identifier for a fleet to update attribute metadata for. You
-- can use either the fleet ID or ARN value.
newUpdateFleetAttributes ::
  -- | 'fleetId'
  Core.Text ->
  UpdateFleetAttributes
newUpdateFleetAttributes pFleetId_ =
  UpdateFleetAttributes'
    { newGameSessionProtectionPolicy' =
        Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      resourceCreationLimitPolicy = Core.Nothing,
      metricGroups = Core.Nothing,
      fleetId = pFleetId_
    }

-- | Game session protection policy to apply to all new instances created in
-- this fleet. Instances that already exist are not affected. You can set
-- protection for individual instances using UpdateGameSession.
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
updateFleetAttributes_newGameSessionProtectionPolicy :: Lens.Lens' UpdateFleetAttributes (Core.Maybe ProtectionPolicy)
updateFleetAttributes_newGameSessionProtectionPolicy = Lens.lens (\UpdateFleetAttributes' {newGameSessionProtectionPolicy'} -> newGameSessionProtectionPolicy') (\s@UpdateFleetAttributes' {} a -> s {newGameSessionProtectionPolicy' = a} :: UpdateFleetAttributes)

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
updateFleetAttributes_name :: Lens.Lens' UpdateFleetAttributes (Core.Maybe Core.Text)
updateFleetAttributes_name = Lens.lens (\UpdateFleetAttributes' {name} -> name) (\s@UpdateFleetAttributes' {} a -> s {name = a} :: UpdateFleetAttributes)

-- | Human-readable description of a fleet.
updateFleetAttributes_description :: Lens.Lens' UpdateFleetAttributes (Core.Maybe Core.Text)
updateFleetAttributes_description = Lens.lens (\UpdateFleetAttributes' {description} -> description) (\s@UpdateFleetAttributes' {} a -> s {description = a} :: UpdateFleetAttributes)

-- | Policy that limits the number of game sessions an individual player can
-- create over a span of time.
updateFleetAttributes_resourceCreationLimitPolicy :: Lens.Lens' UpdateFleetAttributes (Core.Maybe ResourceCreationLimitPolicy)
updateFleetAttributes_resourceCreationLimitPolicy = Lens.lens (\UpdateFleetAttributes' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@UpdateFleetAttributes' {} a -> s {resourceCreationLimitPolicy = a} :: UpdateFleetAttributes)

-- | Names of metric groups to include this fleet in. Amazon CloudWatch uses
-- a fleet metric group is to aggregate metrics from multiple fleets. Use
-- an existing metric group name to add this fleet to the group. Or use a
-- new name to create a new metric group. A fleet can only be included in
-- one metric group at a time.
updateFleetAttributes_metricGroups :: Lens.Lens' UpdateFleetAttributes (Core.Maybe [Core.Text])
updateFleetAttributes_metricGroups = Lens.lens (\UpdateFleetAttributes' {metricGroups} -> metricGroups) (\s@UpdateFleetAttributes' {} a -> s {metricGroups = a} :: UpdateFleetAttributes) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for a fleet to update attribute metadata for. You
-- can use either the fleet ID or ARN value.
updateFleetAttributes_fleetId :: Lens.Lens' UpdateFleetAttributes Core.Text
updateFleetAttributes_fleetId = Lens.lens (\UpdateFleetAttributes' {fleetId} -> fleetId) (\s@UpdateFleetAttributes' {} a -> s {fleetId = a} :: UpdateFleetAttributes)

instance Core.AWSRequest UpdateFleetAttributes where
  type
    AWSResponse UpdateFleetAttributes =
      UpdateFleetAttributesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetAttributesResponse'
            Core.<$> (x Core..?> "FleetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFleetAttributes

instance Core.NFData UpdateFleetAttributes

instance Core.ToHeaders UpdateFleetAttributes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.UpdateFleetAttributes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFleetAttributes where
  toJSON UpdateFleetAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NewGameSessionProtectionPolicy" Core..=)
              Core.<$> newGameSessionProtectionPolicy',
            ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description,
            ("ResourceCreationLimitPolicy" Core..=)
              Core.<$> resourceCreationLimitPolicy,
            ("MetricGroups" Core..=) Core.<$> metricGroups,
            Core.Just ("FleetId" Core..= fleetId)
          ]
      )

instance Core.ToPath UpdateFleetAttributes where
  toPath = Core.const "/"

instance Core.ToQuery UpdateFleetAttributes where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newUpdateFleetAttributesResponse' smart constructor.
data UpdateFleetAttributesResponse = UpdateFleetAttributesResponse'
  { -- | A unique identifier for a fleet that was updated. Use either the fleet
    -- ID or ARN value.
    fleetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFleetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'updateFleetAttributesResponse_fleetId' - A unique identifier for a fleet that was updated. Use either the fleet
-- ID or ARN value.
--
-- 'httpStatus', 'updateFleetAttributesResponse_httpStatus' - The response's http status code.
newUpdateFleetAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateFleetAttributesResponse
newUpdateFleetAttributesResponse pHttpStatus_ =
  UpdateFleetAttributesResponse'
    { fleetId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique identifier for a fleet that was updated. Use either the fleet
-- ID or ARN value.
updateFleetAttributesResponse_fleetId :: Lens.Lens' UpdateFleetAttributesResponse (Core.Maybe Core.Text)
updateFleetAttributesResponse_fleetId = Lens.lens (\UpdateFleetAttributesResponse' {fleetId} -> fleetId) (\s@UpdateFleetAttributesResponse' {} a -> s {fleetId = a} :: UpdateFleetAttributesResponse)

-- | The response's http status code.
updateFleetAttributesResponse_httpStatus :: Lens.Lens' UpdateFleetAttributesResponse Core.Int
updateFleetAttributesResponse_httpStatus = Lens.lens (\UpdateFleetAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetAttributesResponse' {} a -> s {httpStatus = a} :: UpdateFleetAttributesResponse)

instance Core.NFData UpdateFleetAttributesResponse
