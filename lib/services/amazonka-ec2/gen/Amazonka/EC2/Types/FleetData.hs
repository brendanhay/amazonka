{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.FleetData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DescribeFleetError
import Amazonka.EC2.Types.DescribeFleetsInstances
import Amazonka.EC2.Types.FleetActivityStatus
import Amazonka.EC2.Types.FleetExcessCapacityTerminationPolicy
import Amazonka.EC2.Types.FleetLaunchTemplateConfig
import Amazonka.EC2.Types.FleetStateCode
import Amazonka.EC2.Types.FleetType
import Amazonka.EC2.Types.OnDemandOptions
import Amazonka.EC2.Types.SpotOptions
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TargetCapacitySpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes an EC2 Fleet.
--
-- /See:/ 'newFleetData' smart constructor.
data FleetData = FleetData'
  { -- | The progress of the EC2 Fleet. If there is an error, the status is
    -- @error@. After all requests are placed, the status is
    -- @pending_fulfillment@. If the size of the EC2 Fleet is equal to or
    -- greater than its target capacity, the status is @fulfilled@. If the size
    -- of the EC2 Fleet is decreased, the status is @pending_termination@ while
    -- instances are terminating.
    activityStatus :: Prelude.Maybe FleetActivityStatus,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
    --
    -- Constraints: Maximum 64 ASCII characters
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Reserved.
    context :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time of the EC2 Fleet.
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | Information about the instances that could not be launched by the fleet.
    -- Valid only when __Type__ is set to @instant@.
    errors :: Prelude.Maybe [DescribeFleetError],
    -- | Indicates whether running instances should be terminated if the target
    -- capacity of the EC2 Fleet is decreased below the current size of the EC2
    -- Fleet.
    excessCapacityTerminationPolicy :: Prelude.Maybe FleetExcessCapacityTerminationPolicy,
    -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The state of the EC2 Fleet.
    fleetState :: Prelude.Maybe FleetStateCode,
    -- | The number of units fulfilled by this request compared to the set target
    -- capacity.
    fulfilledCapacity :: Prelude.Maybe Prelude.Double,
    -- | The number of units fulfilled by this request compared to the set target
    -- On-Demand capacity.
    fulfilledOnDemandCapacity :: Prelude.Maybe Prelude.Double,
    -- | Information about the instances that were launched by the fleet. Valid
    -- only when __Type__ is set to @instant@.
    instances :: Prelude.Maybe [DescribeFleetsInstances],
    -- | The launch template and overrides.
    launchTemplateConfigs :: Prelude.Maybe [FleetLaunchTemplateConfig],
    -- | The allocation strategy of On-Demand Instances in an EC2 Fleet.
    onDemandOptions :: Prelude.Maybe OnDemandOptions,
    -- | Indicates whether EC2 Fleet should replace unhealthy Spot Instances.
    -- Supported only for fleets of type @maintain@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#ec2-fleet-health-checks EC2 Fleet health checks>
    -- in the /Amazon EC2 User Guide/.
    replaceUnhealthyInstances :: Prelude.Maybe Prelude.Bool,
    -- | The configuration of Spot Instances in an EC2 Fleet.
    spotOptions :: Prelude.Maybe SpotOptions,
    -- | The tags for an EC2 Fleet resource.
    tags :: Prelude.Maybe [Tag],
    -- | The number of units to request. You can choose to set the target
    -- capacity in terms of instances or a performance characteristic that is
    -- important to your application workload, such as vCPUs, memory, or I\/O.
    -- If the request type is @maintain@, you can specify a target capacity of
    -- 0 and add capacity later.
    targetCapacitySpecification :: Prelude.Maybe TargetCapacitySpecification,
    -- | Indicates whether running instances should be terminated when the EC2
    -- Fleet expires.
    terminateInstancesWithExpiration :: Prelude.Maybe Prelude.Bool,
    -- | The type of request. Indicates whether the EC2 Fleet only @requests@ the
    -- target capacity, or also attempts to @maintain@ it. If you request a
    -- certain target capacity, EC2 Fleet only places the required requests; it
    -- does not attempt to replenish instances if capacity is diminished, and
    -- it does not submit requests in alternative capacity pools if capacity is
    -- unavailable. To maintain a certain target capacity, EC2 Fleet places the
    -- required requests to meet this target capacity. It also automatically
    -- replenishes any interrupted Spot Instances. Default: @maintain@.
    type' :: Prelude.Maybe FleetType,
    -- | The start date and time of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
    -- the request immediately.
    validFrom :: Prelude.Maybe Data.ISO8601,
    -- | The end date and time of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new instance
    -- requests are placed or able to fulfill the request. The default end date
    -- is 7 days from the current date.
    validUntil :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activityStatus', 'fleetData_activityStatus' - The progress of the EC2 Fleet. If there is an error, the status is
-- @error@. After all requests are placed, the status is
-- @pending_fulfillment@. If the size of the EC2 Fleet is equal to or
-- greater than its target capacity, the status is @fulfilled@. If the size
-- of the EC2 Fleet is decreased, the status is @pending_termination@ while
-- instances are terminating.
--
-- 'clientToken', 'fleetData_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
--
-- 'context', 'fleetData_context' - Reserved.
--
-- 'createTime', 'fleetData_createTime' - The creation date and time of the EC2 Fleet.
--
-- 'errors', 'fleetData_errors' - Information about the instances that could not be launched by the fleet.
-- Valid only when __Type__ is set to @instant@.
--
-- 'excessCapacityTerminationPolicy', 'fleetData_excessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the target
-- capacity of the EC2 Fleet is decreased below the current size of the EC2
-- Fleet.
--
-- 'fleetId', 'fleetData_fleetId' - The ID of the EC2 Fleet.
--
-- 'fleetState', 'fleetData_fleetState' - The state of the EC2 Fleet.
--
-- 'fulfilledCapacity', 'fleetData_fulfilledCapacity' - The number of units fulfilled by this request compared to the set target
-- capacity.
--
-- 'fulfilledOnDemandCapacity', 'fleetData_fulfilledOnDemandCapacity' - The number of units fulfilled by this request compared to the set target
-- On-Demand capacity.
--
-- 'instances', 'fleetData_instances' - Information about the instances that were launched by the fleet. Valid
-- only when __Type__ is set to @instant@.
--
-- 'launchTemplateConfigs', 'fleetData_launchTemplateConfigs' - The launch template and overrides.
--
-- 'onDemandOptions', 'fleetData_onDemandOptions' - The allocation strategy of On-Demand Instances in an EC2 Fleet.
--
-- 'replaceUnhealthyInstances', 'fleetData_replaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy Spot Instances.
-- Supported only for fleets of type @maintain@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#ec2-fleet-health-checks EC2 Fleet health checks>
-- in the /Amazon EC2 User Guide/.
--
-- 'spotOptions', 'fleetData_spotOptions' - The configuration of Spot Instances in an EC2 Fleet.
--
-- 'tags', 'fleetData_tags' - The tags for an EC2 Fleet resource.
--
-- 'targetCapacitySpecification', 'fleetData_targetCapacitySpecification' - The number of units to request. You can choose to set the target
-- capacity in terms of instances or a performance characteristic that is
-- important to your application workload, such as vCPUs, memory, or I\/O.
-- If the request type is @maintain@, you can specify a target capacity of
-- 0 and add capacity later.
--
-- 'terminateInstancesWithExpiration', 'fleetData_terminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2
-- Fleet expires.
--
-- 'type'', 'fleetData_type' - The type of request. Indicates whether the EC2 Fleet only @requests@ the
-- target capacity, or also attempts to @maintain@ it. If you request a
-- certain target capacity, EC2 Fleet only places the required requests; it
-- does not attempt to replenish instances if capacity is diminished, and
-- it does not submit requests in alternative capacity pools if capacity is
-- unavailable. To maintain a certain target capacity, EC2 Fleet places the
-- required requests to meet this target capacity. It also automatically
-- replenishes any interrupted Spot Instances. Default: @maintain@.
--
-- 'validFrom', 'fleetData_validFrom' - The start date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
-- the request immediately.
--
-- 'validUntil', 'fleetData_validUntil' - The end date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new instance
-- requests are placed or able to fulfill the request. The default end date
-- is 7 days from the current date.
newFleetData ::
  FleetData
newFleetData =
  FleetData'
    { activityStatus = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      context = Prelude.Nothing,
      createTime = Prelude.Nothing,
      errors = Prelude.Nothing,
      excessCapacityTerminationPolicy = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      fleetState = Prelude.Nothing,
      fulfilledCapacity = Prelude.Nothing,
      fulfilledOnDemandCapacity = Prelude.Nothing,
      instances = Prelude.Nothing,
      launchTemplateConfigs = Prelude.Nothing,
      onDemandOptions = Prelude.Nothing,
      replaceUnhealthyInstances = Prelude.Nothing,
      spotOptions = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetCapacitySpecification = Prelude.Nothing,
      terminateInstancesWithExpiration = Prelude.Nothing,
      type' = Prelude.Nothing,
      validFrom = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | The progress of the EC2 Fleet. If there is an error, the status is
-- @error@. After all requests are placed, the status is
-- @pending_fulfillment@. If the size of the EC2 Fleet is equal to or
-- greater than its target capacity, the status is @fulfilled@. If the size
-- of the EC2 Fleet is decreased, the status is @pending_termination@ while
-- instances are terminating.
fleetData_activityStatus :: Lens.Lens' FleetData (Prelude.Maybe FleetActivityStatus)
fleetData_activityStatus = Lens.lens (\FleetData' {activityStatus} -> activityStatus) (\s@FleetData' {} a -> s {activityStatus = a} :: FleetData)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraints: Maximum 64 ASCII characters
fleetData_clientToken :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Text)
fleetData_clientToken = Lens.lens (\FleetData' {clientToken} -> clientToken) (\s@FleetData' {} a -> s {clientToken = a} :: FleetData)

-- | Reserved.
fleetData_context :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Text)
fleetData_context = Lens.lens (\FleetData' {context} -> context) (\s@FleetData' {} a -> s {context = a} :: FleetData)

-- | The creation date and time of the EC2 Fleet.
fleetData_createTime :: Lens.Lens' FleetData (Prelude.Maybe Prelude.UTCTime)
fleetData_createTime = Lens.lens (\FleetData' {createTime} -> createTime) (\s@FleetData' {} a -> s {createTime = a} :: FleetData) Prelude.. Lens.mapping Data._Time

-- | Information about the instances that could not be launched by the fleet.
-- Valid only when __Type__ is set to @instant@.
fleetData_errors :: Lens.Lens' FleetData (Prelude.Maybe [DescribeFleetError])
fleetData_errors = Lens.lens (\FleetData' {errors} -> errors) (\s@FleetData' {} a -> s {errors = a} :: FleetData) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether running instances should be terminated if the target
-- capacity of the EC2 Fleet is decreased below the current size of the EC2
-- Fleet.
fleetData_excessCapacityTerminationPolicy :: Lens.Lens' FleetData (Prelude.Maybe FleetExcessCapacityTerminationPolicy)
fleetData_excessCapacityTerminationPolicy = Lens.lens (\FleetData' {excessCapacityTerminationPolicy} -> excessCapacityTerminationPolicy) (\s@FleetData' {} a -> s {excessCapacityTerminationPolicy = a} :: FleetData)

-- | The ID of the EC2 Fleet.
fleetData_fleetId :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Text)
fleetData_fleetId = Lens.lens (\FleetData' {fleetId} -> fleetId) (\s@FleetData' {} a -> s {fleetId = a} :: FleetData)

-- | The state of the EC2 Fleet.
fleetData_fleetState :: Lens.Lens' FleetData (Prelude.Maybe FleetStateCode)
fleetData_fleetState = Lens.lens (\FleetData' {fleetState} -> fleetState) (\s@FleetData' {} a -> s {fleetState = a} :: FleetData)

-- | The number of units fulfilled by this request compared to the set target
-- capacity.
fleetData_fulfilledCapacity :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Double)
fleetData_fulfilledCapacity = Lens.lens (\FleetData' {fulfilledCapacity} -> fulfilledCapacity) (\s@FleetData' {} a -> s {fulfilledCapacity = a} :: FleetData)

-- | The number of units fulfilled by this request compared to the set target
-- On-Demand capacity.
fleetData_fulfilledOnDemandCapacity :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Double)
fleetData_fulfilledOnDemandCapacity = Lens.lens (\FleetData' {fulfilledOnDemandCapacity} -> fulfilledOnDemandCapacity) (\s@FleetData' {} a -> s {fulfilledOnDemandCapacity = a} :: FleetData)

-- | Information about the instances that were launched by the fleet. Valid
-- only when __Type__ is set to @instant@.
fleetData_instances :: Lens.Lens' FleetData (Prelude.Maybe [DescribeFleetsInstances])
fleetData_instances = Lens.lens (\FleetData' {instances} -> instances) (\s@FleetData' {} a -> s {instances = a} :: FleetData) Prelude.. Lens.mapping Lens.coerced

-- | The launch template and overrides.
fleetData_launchTemplateConfigs :: Lens.Lens' FleetData (Prelude.Maybe [FleetLaunchTemplateConfig])
fleetData_launchTemplateConfigs = Lens.lens (\FleetData' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@FleetData' {} a -> s {launchTemplateConfigs = a} :: FleetData) Prelude.. Lens.mapping Lens.coerced

-- | The allocation strategy of On-Demand Instances in an EC2 Fleet.
fleetData_onDemandOptions :: Lens.Lens' FleetData (Prelude.Maybe OnDemandOptions)
fleetData_onDemandOptions = Lens.lens (\FleetData' {onDemandOptions} -> onDemandOptions) (\s@FleetData' {} a -> s {onDemandOptions = a} :: FleetData)

-- | Indicates whether EC2 Fleet should replace unhealthy Spot Instances.
-- Supported only for fleets of type @maintain@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#ec2-fleet-health-checks EC2 Fleet health checks>
-- in the /Amazon EC2 User Guide/.
fleetData_replaceUnhealthyInstances :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Bool)
fleetData_replaceUnhealthyInstances = Lens.lens (\FleetData' {replaceUnhealthyInstances} -> replaceUnhealthyInstances) (\s@FleetData' {} a -> s {replaceUnhealthyInstances = a} :: FleetData)

-- | The configuration of Spot Instances in an EC2 Fleet.
fleetData_spotOptions :: Lens.Lens' FleetData (Prelude.Maybe SpotOptions)
fleetData_spotOptions = Lens.lens (\FleetData' {spotOptions} -> spotOptions) (\s@FleetData' {} a -> s {spotOptions = a} :: FleetData)

-- | The tags for an EC2 Fleet resource.
fleetData_tags :: Lens.Lens' FleetData (Prelude.Maybe [Tag])
fleetData_tags = Lens.lens (\FleetData' {tags} -> tags) (\s@FleetData' {} a -> s {tags = a} :: FleetData) Prelude.. Lens.mapping Lens.coerced

-- | The number of units to request. You can choose to set the target
-- capacity in terms of instances or a performance characteristic that is
-- important to your application workload, such as vCPUs, memory, or I\/O.
-- If the request type is @maintain@, you can specify a target capacity of
-- 0 and add capacity later.
fleetData_targetCapacitySpecification :: Lens.Lens' FleetData (Prelude.Maybe TargetCapacitySpecification)
fleetData_targetCapacitySpecification = Lens.lens (\FleetData' {targetCapacitySpecification} -> targetCapacitySpecification) (\s@FleetData' {} a -> s {targetCapacitySpecification = a} :: FleetData)

-- | Indicates whether running instances should be terminated when the EC2
-- Fleet expires.
fleetData_terminateInstancesWithExpiration :: Lens.Lens' FleetData (Prelude.Maybe Prelude.Bool)
fleetData_terminateInstancesWithExpiration = Lens.lens (\FleetData' {terminateInstancesWithExpiration} -> terminateInstancesWithExpiration) (\s@FleetData' {} a -> s {terminateInstancesWithExpiration = a} :: FleetData)

-- | The type of request. Indicates whether the EC2 Fleet only @requests@ the
-- target capacity, or also attempts to @maintain@ it. If you request a
-- certain target capacity, EC2 Fleet only places the required requests; it
-- does not attempt to replenish instances if capacity is diminished, and
-- it does not submit requests in alternative capacity pools if capacity is
-- unavailable. To maintain a certain target capacity, EC2 Fleet places the
-- required requests to meet this target capacity. It also automatically
-- replenishes any interrupted Spot Instances. Default: @maintain@.
fleetData_type :: Lens.Lens' FleetData (Prelude.Maybe FleetType)
fleetData_type = Lens.lens (\FleetData' {type'} -> type') (\s@FleetData' {} a -> s {type' = a} :: FleetData)

-- | The start date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
-- the request immediately.
fleetData_validFrom :: Lens.Lens' FleetData (Prelude.Maybe Prelude.UTCTime)
fleetData_validFrom = Lens.lens (\FleetData' {validFrom} -> validFrom) (\s@FleetData' {} a -> s {validFrom = a} :: FleetData) Prelude.. Lens.mapping Data._Time

-- | The end date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new instance
-- requests are placed or able to fulfill the request. The default end date
-- is 7 days from the current date.
fleetData_validUntil :: Lens.Lens' FleetData (Prelude.Maybe Prelude.UTCTime)
fleetData_validUntil = Lens.lens (\FleetData' {validUntil} -> validUntil) (\s@FleetData' {} a -> s {validUntil = a} :: FleetData) Prelude.. Lens.mapping Data._Time

instance Data.FromXML FleetData where
  parseXML x =
    FleetData'
      Prelude.<$> (x Data..@? "activityStatus")
      Prelude.<*> (x Data..@? "clientToken")
      Prelude.<*> (x Data..@? "context")
      Prelude.<*> (x Data..@? "createTime")
      Prelude.<*> ( x Data..@? "errorSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "excessCapacityTerminationPolicy")
      Prelude.<*> (x Data..@? "fleetId")
      Prelude.<*> (x Data..@? "fleetState")
      Prelude.<*> (x Data..@? "fulfilledCapacity")
      Prelude.<*> (x Data..@? "fulfilledOnDemandCapacity")
      Prelude.<*> ( x
                      Data..@? "fleetInstanceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "launchTemplateConfigs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "onDemandOptions")
      Prelude.<*> (x Data..@? "replaceUnhealthyInstances")
      Prelude.<*> (x Data..@? "spotOptions")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "targetCapacitySpecification")
      Prelude.<*> (x Data..@? "terminateInstancesWithExpiration")
      Prelude.<*> (x Data..@? "type")
      Prelude.<*> (x Data..@? "validFrom")
      Prelude.<*> (x Data..@? "validUntil")

instance Prelude.Hashable FleetData where
  hashWithSalt _salt FleetData' {..} =
    _salt
      `Prelude.hashWithSalt` activityStatus
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` excessCapacityTerminationPolicy
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` fleetState
      `Prelude.hashWithSalt` fulfilledCapacity
      `Prelude.hashWithSalt` fulfilledOnDemandCapacity
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` launchTemplateConfigs
      `Prelude.hashWithSalt` onDemandOptions
      `Prelude.hashWithSalt` replaceUnhealthyInstances
      `Prelude.hashWithSalt` spotOptions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetCapacitySpecification
      `Prelude.hashWithSalt` terminateInstancesWithExpiration
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` validFrom
      `Prelude.hashWithSalt` validUntil

instance Prelude.NFData FleetData where
  rnf FleetData' {..} =
    Prelude.rnf activityStatus
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf excessCapacityTerminationPolicy
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf fleetState
      `Prelude.seq` Prelude.rnf fulfilledCapacity
      `Prelude.seq` Prelude.rnf fulfilledOnDemandCapacity
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf launchTemplateConfigs
      `Prelude.seq` Prelude.rnf onDemandOptions
      `Prelude.seq` Prelude.rnf replaceUnhealthyInstances
      `Prelude.seq` Prelude.rnf spotOptions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        targetCapacitySpecification
      `Prelude.seq` Prelude.rnf
        terminateInstancesWithExpiration
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf validFrom
      `Prelude.seq` Prelude.rnf validUntil
