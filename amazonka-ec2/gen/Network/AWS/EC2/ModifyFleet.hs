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
-- Module      : Network.AWS.EC2.ModifyFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.EC2.ModifyFleet
  ( -- * Creating a Request
    ModifyFleet (..),
    newModifyFleet,

    -- * Request Lenses
    modifyFleet_launchTemplateConfigs,
    modifyFleet_dryRun,
    modifyFleet_excessCapacityTerminationPolicy,
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyFleet' smart constructor.
data ModifyFleet = ModifyFleet'
  { -- | The launch template and overrides.
    launchTemplateConfigs :: Prelude.Maybe [FleetLaunchTemplateConfigRequest],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether running instances should be terminated if the total
    -- target capacity of the EC2 Fleet is decreased below the current size of
    -- the EC2 Fleet.
    excessCapacityTerminationPolicy :: Prelude.Maybe FleetExcessCapacityTerminationPolicy,
    -- | The size of the EC2 Fleet.
    targetCapacitySpecification :: Prelude.Maybe TargetCapacitySpecificationRequest,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateConfigs', 'modifyFleet_launchTemplateConfigs' - The launch template and overrides.
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
-- 'targetCapacitySpecification', 'modifyFleet_targetCapacitySpecification' - The size of the EC2 Fleet.
--
-- 'fleetId', 'modifyFleet_fleetId' - The ID of the EC2 Fleet.
newModifyFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  ModifyFleet
newModifyFleet pFleetId_ =
  ModifyFleet'
    { launchTemplateConfigs =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      excessCapacityTerminationPolicy = Prelude.Nothing,
      targetCapacitySpecification = Prelude.Nothing,
      fleetId = pFleetId_
    }

-- | The launch template and overrides.
modifyFleet_launchTemplateConfigs :: Lens.Lens' ModifyFleet (Prelude.Maybe [FleetLaunchTemplateConfigRequest])
modifyFleet_launchTemplateConfigs = Lens.lens (\ModifyFleet' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@ModifyFleet' {} a -> s {launchTemplateConfigs = a} :: ModifyFleet) Prelude.. Lens.mapping Prelude._Coerce

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

-- | The size of the EC2 Fleet.
modifyFleet_targetCapacitySpecification :: Lens.Lens' ModifyFleet (Prelude.Maybe TargetCapacitySpecificationRequest)
modifyFleet_targetCapacitySpecification = Lens.lens (\ModifyFleet' {targetCapacitySpecification} -> targetCapacitySpecification) (\s@ModifyFleet' {} a -> s {targetCapacitySpecification = a} :: ModifyFleet)

-- | The ID of the EC2 Fleet.
modifyFleet_fleetId :: Lens.Lens' ModifyFleet Prelude.Text
modifyFleet_fleetId = Lens.lens (\ModifyFleet' {fleetId} -> fleetId) (\s@ModifyFleet' {} a -> s {fleetId = a} :: ModifyFleet)

instance Prelude.AWSRequest ModifyFleet where
  type Rs ModifyFleet = ModifyFleetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyFleetResponse'
            Prelude.<$> (x Prelude..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyFleet

instance Prelude.NFData ModifyFleet

instance Prelude.ToHeaders ModifyFleet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyFleet where
  toQuery ModifyFleet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyFleet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "LaunchTemplateConfig"
              Prelude.<$> launchTemplateConfigs
          ),
        "DryRun" Prelude.=: dryRun,
        "ExcessCapacityTerminationPolicy"
          Prelude.=: excessCapacityTerminationPolicy,
        "TargetCapacitySpecification"
          Prelude.=: targetCapacitySpecification,
        "FleetId" Prelude.=: fleetId
      ]

-- | /See:/ 'newModifyFleetResponse' smart constructor.
data ModifyFleetResponse = ModifyFleetResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyFleetResponse_return' - Is @true@ if the request succeeds, and an error otherwise.
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

-- | Is @true@ if the request succeeds, and an error otherwise.
modifyFleetResponse_return :: Lens.Lens' ModifyFleetResponse (Prelude.Maybe Prelude.Bool)
modifyFleetResponse_return = Lens.lens (\ModifyFleetResponse' {return'} -> return') (\s@ModifyFleetResponse' {} a -> s {return' = a} :: ModifyFleetResponse)

-- | The response's http status code.
modifyFleetResponse_httpStatus :: Lens.Lens' ModifyFleetResponse Prelude.Int
modifyFleetResponse_httpStatus = Lens.lens (\ModifyFleetResponse' {httpStatus} -> httpStatus) (\s@ModifyFleetResponse' {} a -> s {httpStatus = a} :: ModifyFleetResponse)

instance Prelude.NFData ModifyFleetResponse
