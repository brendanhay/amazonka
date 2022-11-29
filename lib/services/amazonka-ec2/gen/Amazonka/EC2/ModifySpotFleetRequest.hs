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
-- Module      : Amazonka.EC2.ModifySpotFleetRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified Spot Fleet request.
--
-- You can only modify a Spot Fleet request of type @maintain@.
--
-- While the Spot Fleet request is being modified, it is in the @modifying@
-- state.
--
-- To scale up your Spot Fleet, increase its target capacity. The Spot
-- Fleet launches the additional Spot Instances according to the allocation
-- strategy for the Spot Fleet request. If the allocation strategy is
-- @lowestPrice@, the Spot Fleet launches instances using the Spot Instance
-- pool with the lowest price. If the allocation strategy is @diversified@,
-- the Spot Fleet distributes the instances across the Spot Instance pools.
-- If the allocation strategy is @capacityOptimized@, Spot Fleet launches
-- instances from Spot Instance pools with optimal capacity for the number
-- of instances that are launching.
--
-- To scale down your Spot Fleet, decrease its target capacity. First, the
-- Spot Fleet cancels any open requests that exceed the new target
-- capacity. You can request that the Spot Fleet terminate Spot Instances
-- until the size of the fleet no longer exceeds the new target capacity.
-- If the allocation strategy is @lowestPrice@, the Spot Fleet terminates
-- the instances with the highest price per unit. If the allocation
-- strategy is @capacityOptimized@, the Spot Fleet terminates the instances
-- in the Spot Instance pools that have the least available Spot Instance
-- capacity. If the allocation strategy is @diversified@, the Spot Fleet
-- terminates instances across the Spot Instance pools. Alternatively, you
-- can request that the Spot Fleet keep the fleet at its current size, but
-- not replace any Spot Instances that are interrupted or that you
-- terminate manually.
--
-- If you are finished with your Spot Fleet for now, but will use it again
-- later, you can set the target capacity to 0.
module Amazonka.EC2.ModifySpotFleetRequest
  ( -- * Creating a Request
    ModifySpotFleetRequest (..),
    newModifySpotFleetRequest,

    -- * Request Lenses
    modifySpotFleetRequest_excessCapacityTerminationPolicy,
    modifySpotFleetRequest_targetCapacity,
    modifySpotFleetRequest_context,
    modifySpotFleetRequest_launchTemplateConfigs,
    modifySpotFleetRequest_onDemandTargetCapacity,
    modifySpotFleetRequest_spotFleetRequestId,

    -- * Destructuring the Response
    ModifySpotFleetRequestResponse (..),
    newModifySpotFleetRequestResponse,

    -- * Response Lenses
    modifySpotFleetRequestResponse_return,
    modifySpotFleetRequestResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ModifySpotFleetRequest.
--
-- /See:/ 'newModifySpotFleetRequest' smart constructor.
data ModifySpotFleetRequest = ModifySpotFleetRequest'
  { -- | Indicates whether running Spot Instances should be terminated if the
    -- target capacity of the Spot Fleet request is decreased below the current
    -- size of the Spot Fleet.
    excessCapacityTerminationPolicy :: Prelude.Maybe ExcessCapacityTerminationPolicy,
    -- | The size of the fleet.
    targetCapacity :: Prelude.Maybe Prelude.Int,
    -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | The launch template and overrides. You can only use this parameter if
    -- you specified a launch template (@LaunchTemplateConfigs@) in your Spot
    -- Fleet request. If you specified @LaunchSpecifications@ in your Spot
    -- Fleet request, then omit this parameter.
    launchTemplateConfigs :: Prelude.Maybe [LaunchTemplateConfig],
    -- | The number of On-Demand Instances in the fleet.
    onDemandTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySpotFleetRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excessCapacityTerminationPolicy', 'modifySpotFleetRequest_excessCapacityTerminationPolicy' - Indicates whether running Spot Instances should be terminated if the
-- target capacity of the Spot Fleet request is decreased below the current
-- size of the Spot Fleet.
--
-- 'targetCapacity', 'modifySpotFleetRequest_targetCapacity' - The size of the fleet.
--
-- 'context', 'modifySpotFleetRequest_context' - Reserved.
--
-- 'launchTemplateConfigs', 'modifySpotFleetRequest_launchTemplateConfigs' - The launch template and overrides. You can only use this parameter if
-- you specified a launch template (@LaunchTemplateConfigs@) in your Spot
-- Fleet request. If you specified @LaunchSpecifications@ in your Spot
-- Fleet request, then omit this parameter.
--
-- 'onDemandTargetCapacity', 'modifySpotFleetRequest_onDemandTargetCapacity' - The number of On-Demand Instances in the fleet.
--
-- 'spotFleetRequestId', 'modifySpotFleetRequest_spotFleetRequestId' - The ID of the Spot Fleet request.
newModifySpotFleetRequest ::
  -- | 'spotFleetRequestId'
  Prelude.Text ->
  ModifySpotFleetRequest
newModifySpotFleetRequest pSpotFleetRequestId_ =
  ModifySpotFleetRequest'
    { excessCapacityTerminationPolicy =
        Prelude.Nothing,
      targetCapacity = Prelude.Nothing,
      context = Prelude.Nothing,
      launchTemplateConfigs = Prelude.Nothing,
      onDemandTargetCapacity = Prelude.Nothing,
      spotFleetRequestId = pSpotFleetRequestId_
    }

-- | Indicates whether running Spot Instances should be terminated if the
-- target capacity of the Spot Fleet request is decreased below the current
-- size of the Spot Fleet.
modifySpotFleetRequest_excessCapacityTerminationPolicy :: Lens.Lens' ModifySpotFleetRequest (Prelude.Maybe ExcessCapacityTerminationPolicy)
modifySpotFleetRequest_excessCapacityTerminationPolicy = Lens.lens (\ModifySpotFleetRequest' {excessCapacityTerminationPolicy} -> excessCapacityTerminationPolicy) (\s@ModifySpotFleetRequest' {} a -> s {excessCapacityTerminationPolicy = a} :: ModifySpotFleetRequest)

-- | The size of the fleet.
modifySpotFleetRequest_targetCapacity :: Lens.Lens' ModifySpotFleetRequest (Prelude.Maybe Prelude.Int)
modifySpotFleetRequest_targetCapacity = Lens.lens (\ModifySpotFleetRequest' {targetCapacity} -> targetCapacity) (\s@ModifySpotFleetRequest' {} a -> s {targetCapacity = a} :: ModifySpotFleetRequest)

-- | Reserved.
modifySpotFleetRequest_context :: Lens.Lens' ModifySpotFleetRequest (Prelude.Maybe Prelude.Text)
modifySpotFleetRequest_context = Lens.lens (\ModifySpotFleetRequest' {context} -> context) (\s@ModifySpotFleetRequest' {} a -> s {context = a} :: ModifySpotFleetRequest)

-- | The launch template and overrides. You can only use this parameter if
-- you specified a launch template (@LaunchTemplateConfigs@) in your Spot
-- Fleet request. If you specified @LaunchSpecifications@ in your Spot
-- Fleet request, then omit this parameter.
modifySpotFleetRequest_launchTemplateConfigs :: Lens.Lens' ModifySpotFleetRequest (Prelude.Maybe [LaunchTemplateConfig])
modifySpotFleetRequest_launchTemplateConfigs = Lens.lens (\ModifySpotFleetRequest' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@ModifySpotFleetRequest' {} a -> s {launchTemplateConfigs = a} :: ModifySpotFleetRequest) Prelude.. Lens.mapping Lens.coerced

-- | The number of On-Demand Instances in the fleet.
modifySpotFleetRequest_onDemandTargetCapacity :: Lens.Lens' ModifySpotFleetRequest (Prelude.Maybe Prelude.Int)
modifySpotFleetRequest_onDemandTargetCapacity = Lens.lens (\ModifySpotFleetRequest' {onDemandTargetCapacity} -> onDemandTargetCapacity) (\s@ModifySpotFleetRequest' {} a -> s {onDemandTargetCapacity = a} :: ModifySpotFleetRequest)

-- | The ID of the Spot Fleet request.
modifySpotFleetRequest_spotFleetRequestId :: Lens.Lens' ModifySpotFleetRequest Prelude.Text
modifySpotFleetRequest_spotFleetRequestId = Lens.lens (\ModifySpotFleetRequest' {spotFleetRequestId} -> spotFleetRequestId) (\s@ModifySpotFleetRequest' {} a -> s {spotFleetRequestId = a} :: ModifySpotFleetRequest)

instance Core.AWSRequest ModifySpotFleetRequest where
  type
    AWSResponse ModifySpotFleetRequest =
      ModifySpotFleetRequestResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifySpotFleetRequestResponse'
            Prelude.<$> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifySpotFleetRequest where
  hashWithSalt _salt ModifySpotFleetRequest' {..} =
    _salt
      `Prelude.hashWithSalt` excessCapacityTerminationPolicy
      `Prelude.hashWithSalt` targetCapacity
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` launchTemplateConfigs
      `Prelude.hashWithSalt` onDemandTargetCapacity
      `Prelude.hashWithSalt` spotFleetRequestId

instance Prelude.NFData ModifySpotFleetRequest where
  rnf ModifySpotFleetRequest' {..} =
    Prelude.rnf excessCapacityTerminationPolicy
      `Prelude.seq` Prelude.rnf targetCapacity
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf launchTemplateConfigs
      `Prelude.seq` Prelude.rnf onDemandTargetCapacity
      `Prelude.seq` Prelude.rnf spotFleetRequestId

instance Core.ToHeaders ModifySpotFleetRequest where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifySpotFleetRequest where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifySpotFleetRequest where
  toQuery ModifySpotFleetRequest' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifySpotFleetRequest" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "ExcessCapacityTerminationPolicy"
          Core.=: excessCapacityTerminationPolicy,
        "TargetCapacity" Core.=: targetCapacity,
        "Context" Core.=: context,
        Core.toQuery
          ( Core.toQueryList "LaunchTemplateConfig"
              Prelude.<$> launchTemplateConfigs
          ),
        "OnDemandTargetCapacity"
          Core.=: onDemandTargetCapacity,
        "SpotFleetRequestId" Core.=: spotFleetRequestId
      ]

-- | Contains the output of ModifySpotFleetRequest.
--
-- /See:/ 'newModifySpotFleetRequestResponse' smart constructor.
data ModifySpotFleetRequestResponse = ModifySpotFleetRequestResponse'
  { -- | If the request succeeds, the response returns @true@. If the request
    -- fails, no response is returned, and instead an error message is
    -- returned.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifySpotFleetRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifySpotFleetRequestResponse_return' - If the request succeeds, the response returns @true@. If the request
-- fails, no response is returned, and instead an error message is
-- returned.
--
-- 'httpStatus', 'modifySpotFleetRequestResponse_httpStatus' - The response's http status code.
newModifySpotFleetRequestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifySpotFleetRequestResponse
newModifySpotFleetRequestResponse pHttpStatus_ =
  ModifySpotFleetRequestResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the request succeeds, the response returns @true@. If the request
-- fails, no response is returned, and instead an error message is
-- returned.
modifySpotFleetRequestResponse_return :: Lens.Lens' ModifySpotFleetRequestResponse (Prelude.Maybe Prelude.Bool)
modifySpotFleetRequestResponse_return = Lens.lens (\ModifySpotFleetRequestResponse' {return'} -> return') (\s@ModifySpotFleetRequestResponse' {} a -> s {return' = a} :: ModifySpotFleetRequestResponse)

-- | The response's http status code.
modifySpotFleetRequestResponse_httpStatus :: Lens.Lens' ModifySpotFleetRequestResponse Prelude.Int
modifySpotFleetRequestResponse_httpStatus = Lens.lens (\ModifySpotFleetRequestResponse' {httpStatus} -> httpStatus) (\s@ModifySpotFleetRequestResponse' {} a -> s {httpStatus = a} :: ModifySpotFleetRequestResponse)

instance
  Prelude.NFData
    ModifySpotFleetRequestResponse
  where
  rnf ModifySpotFleetRequestResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
