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
-- Module      : Amazonka.EC2.ModifyFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified EC2 Fleet.
--
-- You can only modify an EC2 Fleet request of type @maintain@.
--
-- While the EC2 Fleet is being modified, it is in the @modifying@ state.
--
-- To scale up your EC2 Fleet, increase its target capacity. The EC2 Fleet
-- launches the additional Spot Instances according to the allocation
-- strategy for the EC2 Fleet request. If the allocation strategy is
-- @lowest-price@, the EC2 Fleet launches instances using the Spot Instance
-- pool with the lowest price. If the allocation strategy is @diversified@,
-- the EC2 Fleet distributes the instances across the Spot Instance pools.
-- If the allocation strategy is @capacity-optimized@, EC2 Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
--
-- To scale down your EC2 Fleet, decrease its target capacity. First, the
-- EC2 Fleet cancels any open requests that exceed the new target capacity.
-- You can request that the EC2 Fleet terminate Spot Instances until the
-- size of the fleet no longer exceeds the new target capacity. If the
-- allocation strategy is @lowest-price@, the EC2 Fleet terminates the
-- instances with the highest price per unit. If the allocation strategy is
-- @capacity-optimized@, the EC2 Fleet terminates the instances in the Spot
-- Instance pools that have the least available Spot Instance capacity. If
-- the allocation strategy is @diversified@, the EC2 Fleet terminates
-- instances across the Spot Instance pools. Alternatively, you can request
-- that the EC2 Fleet keep the fleet at its current size, but not replace
-- any Spot Instances that are interrupted or that you terminate manually.
--
-- If you are finished with your EC2 Fleet for now, but will use it again
-- later, you can set the target capacity to 0.
module Amazonka.EC2.ModifyFleet
  ( -- * Creating a Request
    ModifyFleet (..),
    newModifyFleet,

    -- * Request Lenses
    modifyFleet_context,
    modifyFleet_dryRun,
    modifyFleet_excessCapacityTerminationPolicy,
    modifyFleet_launchTemplateConfigs,
    modifyFleet_targetCapacitySpecification,
    modifyFleet_fleetId,

    -- * Destructuring the Response
    ModifyFleetResponse (..),
    newModifyFleetResponse,

    -- * Response Lenses
    modifyFleetResponse_return,
    modifyFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyFleet' smart constructor.
data ModifyFleet = ModifyFleet'
  { -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether running instances should be terminated if the total
    -- target capacity of the EC2 Fleet is decreased below the current size of
    -- the EC2 Fleet.
    excessCapacityTerminationPolicy :: Prelude.Maybe FleetExcessCapacityTerminationPolicy,
    -- | The launch template and overrides.
    launchTemplateConfigs :: Prelude.Maybe [FleetLaunchTemplateConfigRequest],
    -- | The size of the EC2 Fleet.
    targetCapacitySpecification :: Prelude.Maybe TargetCapacitySpecificationRequest,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'modifyFleet_context' - Reserved.
--
-- 'dryRun', 'modifyFleet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'excessCapacityTerminationPolicy', 'modifyFleet_excessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total
-- target capacity of the EC2 Fleet is decreased below the current size of
-- the EC2 Fleet.
--
-- 'launchTemplateConfigs', 'modifyFleet_launchTemplateConfigs' - The launch template and overrides.
--
-- 'targetCapacitySpecification', 'modifyFleet_targetCapacitySpecification' - The size of the EC2 Fleet.
--
-- 'fleetId', 'modifyFleet_fleetId' - The ID of the EC2 Fleet.
newModifyFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  ModifyFleet
newModifyFleet pFleetId_ =
  ModifyFleet'
    { context = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      excessCapacityTerminationPolicy = Prelude.Nothing,
      launchTemplateConfigs = Prelude.Nothing,
      targetCapacitySpecification = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | Reserved.
modifyFleet_context :: Lens.Lens' ModifyFleet (Prelude.Maybe Prelude.Text)
modifyFleet_context = Lens.lens (\ModifyFleet' {context} -> context) (\s@ModifyFleet' {} a -> s {context = a} :: ModifyFleet)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyFleet_dryRun :: Lens.Lens' ModifyFleet (Prelude.Maybe Prelude.Bool)
modifyFleet_dryRun = Lens.lens (\ModifyFleet' {dryRun} -> dryRun) (\s@ModifyFleet' {} a -> s {dryRun = a} :: ModifyFleet)

-- | Indicates whether running instances should be terminated if the total
-- target capacity of the EC2 Fleet is decreased below the current size of
-- the EC2 Fleet.
modifyFleet_excessCapacityTerminationPolicy :: Lens.Lens' ModifyFleet (Prelude.Maybe FleetExcessCapacityTerminationPolicy)
modifyFleet_excessCapacityTerminationPolicy = Lens.lens (\ModifyFleet' {excessCapacityTerminationPolicy} -> excessCapacityTerminationPolicy) (\s@ModifyFleet' {} a -> s {excessCapacityTerminationPolicy = a} :: ModifyFleet)

-- | The launch template and overrides.
modifyFleet_launchTemplateConfigs :: Lens.Lens' ModifyFleet (Prelude.Maybe [FleetLaunchTemplateConfigRequest])
modifyFleet_launchTemplateConfigs = Lens.lens (\ModifyFleet' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@ModifyFleet' {} a -> s {launchTemplateConfigs = a} :: ModifyFleet) Prelude.. Lens.mapping Lens.coerced

-- | The size of the EC2 Fleet.
modifyFleet_targetCapacitySpecification :: Lens.Lens' ModifyFleet (Prelude.Maybe TargetCapacitySpecificationRequest)
modifyFleet_targetCapacitySpecification = Lens.lens (\ModifyFleet' {targetCapacitySpecification} -> targetCapacitySpecification) (\s@ModifyFleet' {} a -> s {targetCapacitySpecification = a} :: ModifyFleet)

-- | The ID of the EC2 Fleet.
modifyFleet_fleetId :: Lens.Lens' ModifyFleet Prelude.Text
modifyFleet_fleetId = Lens.lens (\ModifyFleet' {fleetId} -> fleetId) (\s@ModifyFleet' {} a -> s {fleetId = a} :: ModifyFleet)

instance Core.AWSRequest ModifyFleet where
  type AWSResponse ModifyFleet = ModifyFleetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyFleetResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyFleet where
  hashWithSalt _salt ModifyFleet' {..} =
    _salt `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` excessCapacityTerminationPolicy
      `Prelude.hashWithSalt` launchTemplateConfigs
      `Prelude.hashWithSalt` targetCapacitySpecification
      `Prelude.hashWithSalt` fleetId

instance Prelude.NFData ModifyFleet where
  rnf ModifyFleet' {..} =
    Prelude.rnf context
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf excessCapacityTerminationPolicy
      `Prelude.seq` Prelude.rnf launchTemplateConfigs
      `Prelude.seq` Prelude.rnf targetCapacitySpecification
      `Prelude.seq` Prelude.rnf fleetId

instance Data.ToHeaders ModifyFleet where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyFleet where
  toQuery ModifyFleet' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyFleet" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Context" Data.=: context,
        "DryRun" Data.=: dryRun,
        "ExcessCapacityTerminationPolicy"
          Data.=: excessCapacityTerminationPolicy,
        Data.toQuery
          ( Data.toQueryList "LaunchTemplateConfig"
              Prelude.<$> launchTemplateConfigs
          ),
        "TargetCapacitySpecification"
          Data.=: targetCapacitySpecification,
        "FleetId" Data.=: fleetId
      ]

-- | /See:/ 'newModifyFleetResponse' smart constructor.
data ModifyFleetResponse = ModifyFleetResponse'
  { -- | If the request succeeds, the response returns @true@. If the request
    -- fails, no response is returned, and instead an error message is
    -- returned.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyFleetResponse_return' - If the request succeeds, the response returns @true@. If the request
-- fails, no response is returned, and instead an error message is
-- returned.
--
-- 'httpStatus', 'modifyFleetResponse_httpStatus' - The response's http status code.
newModifyFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyFleetResponse
newModifyFleetResponse pHttpStatus_ =
  ModifyFleetResponse'
    { return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request succeeds, the response returns @true@. If the request
-- fails, no response is returned, and instead an error message is
-- returned.
modifyFleetResponse_return :: Lens.Lens' ModifyFleetResponse (Prelude.Maybe Prelude.Bool)
modifyFleetResponse_return = Lens.lens (\ModifyFleetResponse' {return'} -> return') (\s@ModifyFleetResponse' {} a -> s {return' = a} :: ModifyFleetResponse)

-- | The response's http status code.
modifyFleetResponse_httpStatus :: Lens.Lens' ModifyFleetResponse Prelude.Int
modifyFleetResponse_httpStatus = Lens.lens (\ModifyFleetResponse' {httpStatus} -> httpStatus) (\s@ModifyFleetResponse' {} a -> s {httpStatus = a} :: ModifyFleetResponse)

instance Prelude.NFData ModifyFleetResponse where
  rnf ModifyFleetResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
