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
-- Module      : Amazonka.GameLift.UpdateFleetAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet\'s mutable attributes, including game session protection
-- and resource creation limits.
--
-- To update fleet attributes, specify the fleet ID and the property values
-- that you want to change.
--
-- If successful, an updated @FleetAttributes@ object is returned.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
module Amazonka.GameLift.UpdateFleetAttributes
  ( -- * Creating a Request
    UpdateFleetAttributes (..),
    newUpdateFleetAttributes,

    -- * Request Lenses
    updateFleetAttributes_anywhereConfiguration,
    updateFleetAttributes_description,
    updateFleetAttributes_metricGroups,
    updateFleetAttributes_name,
    updateFleetAttributes_newGameSessionProtectionPolicy,
    updateFleetAttributes_resourceCreationLimitPolicy,
    updateFleetAttributes_fleetId,

    -- * Destructuring the Response
    UpdateFleetAttributesResponse (..),
    newUpdateFleetAttributesResponse,

    -- * Response Lenses
    updateFleetAttributesResponse_fleetArn,
    updateFleetAttributesResponse_fleetId,
    updateFleetAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFleetAttributes' smart constructor.
data UpdateFleetAttributes = UpdateFleetAttributes'
  { -- | GameLift Anywhere configuration options.
    anywhereConfiguration :: Prelude.Maybe AnywhereConfiguration,
    -- | A human-readable description of a fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of a metric group to add this fleet to. Use a metric group in
    -- Amazon CloudWatch to aggregate the metrics from multiple fleets. Provide
    -- an existing metric group name, or create a new metric group by providing
    -- a new name. A fleet can only be in one metric group at a time.
    metricGroups :: Prelude.Maybe [Prelude.Text],
    -- | A descriptive label that is associated with a fleet. Fleet names do not
    -- need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The game session protection policy to apply to all new instances created
    -- in this fleet. Instances that already exist are not affected. You can
    -- set protection for individual instances using
    -- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UpdateGameSession.html UpdateGameSession>
    -- .
    --
    -- -   __NoProtection__ -- The game session can be terminated during a
    --     scale-down event.
    --
    -- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
    --     it cannot be terminated during a scale-down event.
    newGameSessionProtectionPolicy' :: Prelude.Maybe ProtectionPolicy,
    -- | Policy settings that limit the number of game sessions an individual
    -- player can create over a span of time.
    resourceCreationLimitPolicy :: Prelude.Maybe ResourceCreationLimitPolicy,
    -- | A unique identifier for the fleet to update attribute metadata for. You
    -- can use either the fleet ID or ARN value.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anywhereConfiguration', 'updateFleetAttributes_anywhereConfiguration' - GameLift Anywhere configuration options.
--
-- 'description', 'updateFleetAttributes_description' - A human-readable description of a fleet.
--
-- 'metricGroups', 'updateFleetAttributes_metricGroups' - The name of a metric group to add this fleet to. Use a metric group in
-- Amazon CloudWatch to aggregate the metrics from multiple fleets. Provide
-- an existing metric group name, or create a new metric group by providing
-- a new name. A fleet can only be in one metric group at a time.
--
-- 'name', 'updateFleetAttributes_name' - A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
--
-- 'newGameSessionProtectionPolicy'', 'updateFleetAttributes_newGameSessionProtectionPolicy' - The game session protection policy to apply to all new instances created
-- in this fleet. Instances that already exist are not affected. You can
-- set protection for individual instances using
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UpdateGameSession.html UpdateGameSession>
-- .
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
--
-- 'resourceCreationLimitPolicy', 'updateFleetAttributes_resourceCreationLimitPolicy' - Policy settings that limit the number of game sessions an individual
-- player can create over a span of time.
--
-- 'fleetId', 'updateFleetAttributes_fleetId' - A unique identifier for the fleet to update attribute metadata for. You
-- can use either the fleet ID or ARN value.
newUpdateFleetAttributes ::
  -- | 'fleetId'
  Prelude.Text ->
  UpdateFleetAttributes
newUpdateFleetAttributes pFleetId_ =
  UpdateFleetAttributes'
    { anywhereConfiguration =
        Prelude.Nothing,
      description = Prelude.Nothing,
      metricGroups = Prelude.Nothing,
      name = Prelude.Nothing,
      newGameSessionProtectionPolicy' = Prelude.Nothing,
      resourceCreationLimitPolicy = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | GameLift Anywhere configuration options.
updateFleetAttributes_anywhereConfiguration :: Lens.Lens' UpdateFleetAttributes (Prelude.Maybe AnywhereConfiguration)
updateFleetAttributes_anywhereConfiguration = Lens.lens (\UpdateFleetAttributes' {anywhereConfiguration} -> anywhereConfiguration) (\s@UpdateFleetAttributes' {} a -> s {anywhereConfiguration = a} :: UpdateFleetAttributes)

-- | A human-readable description of a fleet.
updateFleetAttributes_description :: Lens.Lens' UpdateFleetAttributes (Prelude.Maybe Prelude.Text)
updateFleetAttributes_description = Lens.lens (\UpdateFleetAttributes' {description} -> description) (\s@UpdateFleetAttributes' {} a -> s {description = a} :: UpdateFleetAttributes)

-- | The name of a metric group to add this fleet to. Use a metric group in
-- Amazon CloudWatch to aggregate the metrics from multiple fleets. Provide
-- an existing metric group name, or create a new metric group by providing
-- a new name. A fleet can only be in one metric group at a time.
updateFleetAttributes_metricGroups :: Lens.Lens' UpdateFleetAttributes (Prelude.Maybe [Prelude.Text])
updateFleetAttributes_metricGroups = Lens.lens (\UpdateFleetAttributes' {metricGroups} -> metricGroups) (\s@UpdateFleetAttributes' {} a -> s {metricGroups = a} :: UpdateFleetAttributes) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive label that is associated with a fleet. Fleet names do not
-- need to be unique.
updateFleetAttributes_name :: Lens.Lens' UpdateFleetAttributes (Prelude.Maybe Prelude.Text)
updateFleetAttributes_name = Lens.lens (\UpdateFleetAttributes' {name} -> name) (\s@UpdateFleetAttributes' {} a -> s {name = a} :: UpdateFleetAttributes)

-- | The game session protection policy to apply to all new instances created
-- in this fleet. Instances that already exist are not affected. You can
-- set protection for individual instances using
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_UpdateGameSession.html UpdateGameSession>
-- .
--
-- -   __NoProtection__ -- The game session can be terminated during a
--     scale-down event.
--
-- -   __FullProtection__ -- If the game session is in an @ACTIVE@ status,
--     it cannot be terminated during a scale-down event.
updateFleetAttributes_newGameSessionProtectionPolicy :: Lens.Lens' UpdateFleetAttributes (Prelude.Maybe ProtectionPolicy)
updateFleetAttributes_newGameSessionProtectionPolicy = Lens.lens (\UpdateFleetAttributes' {newGameSessionProtectionPolicy'} -> newGameSessionProtectionPolicy') (\s@UpdateFleetAttributes' {} a -> s {newGameSessionProtectionPolicy' = a} :: UpdateFleetAttributes)

-- | Policy settings that limit the number of game sessions an individual
-- player can create over a span of time.
updateFleetAttributes_resourceCreationLimitPolicy :: Lens.Lens' UpdateFleetAttributes (Prelude.Maybe ResourceCreationLimitPolicy)
updateFleetAttributes_resourceCreationLimitPolicy = Lens.lens (\UpdateFleetAttributes' {resourceCreationLimitPolicy} -> resourceCreationLimitPolicy) (\s@UpdateFleetAttributes' {} a -> s {resourceCreationLimitPolicy = a} :: UpdateFleetAttributes)

-- | A unique identifier for the fleet to update attribute metadata for. You
-- can use either the fleet ID or ARN value.
updateFleetAttributes_fleetId :: Lens.Lens' UpdateFleetAttributes Prelude.Text
updateFleetAttributes_fleetId = Lens.lens (\UpdateFleetAttributes' {fleetId} -> fleetId) (\s@UpdateFleetAttributes' {} a -> s {fleetId = a} :: UpdateFleetAttributes)

instance Core.AWSRequest UpdateFleetAttributes where
  type
    AWSResponse UpdateFleetAttributes =
      UpdateFleetAttributesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFleetAttributesResponse'
            Prelude.<$> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFleetAttributes where
  hashWithSalt _salt UpdateFleetAttributes' {..} =
    _salt `Prelude.hashWithSalt` anywhereConfiguration
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` metricGroups
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` newGameSessionProtectionPolicy'
      `Prelude.hashWithSalt` resourceCreationLimitPolicy
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData UpdateFleetAttributes where
  rnf UpdateFleetAttributes' {..} =
    Prelude.rnf anywhereConfiguration
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf metricGroups
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf newGameSessionProtectionPolicy'
      `Prelude.seq` Prelude.rnf resourceCreationLimitPolicy
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders UpdateFleetAttributes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.UpdateFleetAttributes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFleetAttributes where
  toJSON UpdateFleetAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnywhereConfiguration" Data..=)
              Prelude.<$> anywhereConfiguration,
            ("Description" Data..=) Prelude.<$> description,
            ("MetricGroups" Data..=) Prelude.<$> metricGroups,
            ("Name" Data..=) Prelude.<$> name,
            ("NewGameSessionProtectionPolicy" Data..=)
              Prelude.<$> newGameSessionProtectionPolicy',
            ("ResourceCreationLimitPolicy" Data..=)
              Prelude.<$> resourceCreationLimitPolicy,
            Prelude.Just ("FleetId" Data..= fleetId)
          ]
      )

instance Data.ToPath UpdateFleetAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFleetAttributes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFleetAttributesResponse' smart constructor.
data UpdateFleetAttributesResponse = UpdateFleetAttributesResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that was updated.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'updateFleetAttributesResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'updateFleetAttributesResponse_fleetId' - A unique identifier for the fleet that was updated.
--
-- 'httpStatus', 'updateFleetAttributesResponse_httpStatus' - The response's http status code.
newUpdateFleetAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFleetAttributesResponse
newUpdateFleetAttributesResponse pHttpStatus_ =
  UpdateFleetAttributesResponse'
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
updateFleetAttributesResponse_fleetArn :: Lens.Lens' UpdateFleetAttributesResponse (Prelude.Maybe Prelude.Text)
updateFleetAttributesResponse_fleetArn = Lens.lens (\UpdateFleetAttributesResponse' {fleetArn} -> fleetArn) (\s@UpdateFleetAttributesResponse' {} a -> s {fleetArn = a} :: UpdateFleetAttributesResponse)

-- | A unique identifier for the fleet that was updated.
updateFleetAttributesResponse_fleetId :: Lens.Lens' UpdateFleetAttributesResponse (Prelude.Maybe Prelude.Text)
updateFleetAttributesResponse_fleetId = Lens.lens (\UpdateFleetAttributesResponse' {fleetId} -> fleetId) (\s@UpdateFleetAttributesResponse' {} a -> s {fleetId = a} :: UpdateFleetAttributesResponse)

-- | The response's http status code.
updateFleetAttributesResponse_httpStatus :: Lens.Lens' UpdateFleetAttributesResponse Prelude.Int
updateFleetAttributesResponse_httpStatus = Lens.lens (\UpdateFleetAttributesResponse' {httpStatus} -> httpStatus) (\s@UpdateFleetAttributesResponse' {} a -> s {httpStatus = a} :: UpdateFleetAttributesResponse)

instance Prelude.NFData UpdateFleetAttributesResponse where
  rnf UpdateFleetAttributesResponse' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf httpStatus
