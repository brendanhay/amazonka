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
-- Module      : Network.AWS.EC2.CreateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an EC2 Fleet.
--
-- You can create a single EC2 Fleet that includes multiple launch
-- specifications that vary by instance type, AMI, Availability Zone, or
-- subnet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet.html Launching an EC2 Fleet>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_tagSpecifications,
    createFleet_dryRun,
    createFleet_onDemandOptions,
    createFleet_validFrom,
    createFleet_replaceUnhealthyInstances,
    createFleet_validUntil,
    createFleet_excessCapacityTerminationPolicy,
    createFleet_type,
    createFleet_spotOptions,
    createFleet_clientToken,
    createFleet_terminateInstancesWithExpiration,
    createFleet_launchTemplateConfigs,
    createFleet_targetCapacitySpecification,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_fleetId,
    createFleetResponse_instances,
    createFleetResponse_errors,
    createFleetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | The key-value pair for tagging the EC2 Fleet request on creation. The
    -- value for @ResourceType@ must be @fleet@, otherwise the fleet request
    -- fails. To tag instances at launch, specify the tags in the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>.
    -- For information about tagging after launch, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources>.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
    onDemandOptions :: Prelude.Maybe OnDemandOptionsRequest,
    -- | The start date and time of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
    -- the request immediately.
    validFrom :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether EC2 Fleet should replace unhealthy Spot Instances.
    -- Supported only for fleets of type @maintain@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#ec2-fleet-health-checks EC2 Fleet health checks>
    -- in the /Amazon EC2 User Guide/.
    replaceUnhealthyInstances :: Prelude.Maybe Prelude.Bool,
    -- | The end date and time of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new EC2 Fleet
    -- requests are placed or able to fulfill the request. If no value is
    -- specified, the request remains until you cancel it.
    validUntil :: Prelude.Maybe Core.ISO8601,
    -- | Indicates whether running instances should be terminated if the total
    -- target capacity of the EC2 Fleet is decreased below the current size of
    -- the EC2 Fleet.
    excessCapacityTerminationPolicy :: Prelude.Maybe FleetExcessCapacityTerminationPolicy,
    -- | The type of request. The default value is @maintain@.
    --
    -- -   @maintain@ - The EC2 Fleet places an asynchronous request for your
    --     desired capacity, and continues to maintain your desired Spot
    --     capacity by replenishing interrupted Spot Instances.
    --
    -- -   @request@ - The EC2 Fleet places an asynchronous one-time request
    --     for your desired capacity, but does submit Spot requests in
    --     alternative capacity pools if Spot capacity is unavailable, and does
    --     not maintain Spot capacity if Spot Instances are interrupted.
    --
    -- -   @instant@ - The EC2 Fleet places a synchronous one-time request for
    --     your desired capacity, and returns errors for any instances that
    --     could not be launched.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet request types>
    -- in the /Amazon EC2 User Guide/.
    type' :: Prelude.Maybe FleetType,
    -- | Describes the configuration of Spot Instances in an EC2 Fleet.
    spotOptions :: Prelude.Maybe SpotOptionsRequest,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether running instances should be terminated when the EC2
    -- Fleet expires.
    terminateInstancesWithExpiration :: Prelude.Maybe Prelude.Bool,
    -- | The configuration for the EC2 Fleet.
    launchTemplateConfigs :: [FleetLaunchTemplateConfigRequest],
    -- | The number of units to request.
    targetCapacitySpecification :: TargetCapacitySpecificationRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createFleet_tagSpecifications' - The key-value pair for tagging the EC2 Fleet request on creation. The
-- value for @ResourceType@ must be @fleet@, otherwise the fleet request
-- fails. To tag instances at launch, specify the tags in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>.
-- For information about tagging after launch, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources>.
--
-- 'dryRun', 'createFleet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'onDemandOptions', 'createFleet_onDemandOptions' - Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- 'validFrom', 'createFleet_validFrom' - The start date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
-- the request immediately.
--
-- 'replaceUnhealthyInstances', 'createFleet_replaceUnhealthyInstances' - Indicates whether EC2 Fleet should replace unhealthy Spot Instances.
-- Supported only for fleets of type @maintain@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#ec2-fleet-health-checks EC2 Fleet health checks>
-- in the /Amazon EC2 User Guide/.
--
-- 'validUntil', 'createFleet_validUntil' - The end date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new EC2 Fleet
-- requests are placed or able to fulfill the request. If no value is
-- specified, the request remains until you cancel it.
--
-- 'excessCapacityTerminationPolicy', 'createFleet_excessCapacityTerminationPolicy' - Indicates whether running instances should be terminated if the total
-- target capacity of the EC2 Fleet is decreased below the current size of
-- the EC2 Fleet.
--
-- 'type'', 'createFleet_type' - The type of request. The default value is @maintain@.
--
-- -   @maintain@ - The EC2 Fleet places an asynchronous request for your
--     desired capacity, and continues to maintain your desired Spot
--     capacity by replenishing interrupted Spot Instances.
--
-- -   @request@ - The EC2 Fleet places an asynchronous one-time request
--     for your desired capacity, but does submit Spot requests in
--     alternative capacity pools if Spot capacity is unavailable, and does
--     not maintain Spot capacity if Spot Instances are interrupted.
--
-- -   @instant@ - The EC2 Fleet places a synchronous one-time request for
--     your desired capacity, and returns errors for any instances that
--     could not be launched.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet request types>
-- in the /Amazon EC2 User Guide/.
--
-- 'spotOptions', 'createFleet_spotOptions' - Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- 'clientToken', 'createFleet_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'terminateInstancesWithExpiration', 'createFleet_terminateInstancesWithExpiration' - Indicates whether running instances should be terminated when the EC2
-- Fleet expires.
--
-- 'launchTemplateConfigs', 'createFleet_launchTemplateConfigs' - The configuration for the EC2 Fleet.
--
-- 'targetCapacitySpecification', 'createFleet_targetCapacitySpecification' - The number of units to request.
newCreateFleet ::
  -- | 'targetCapacitySpecification'
  TargetCapacitySpecificationRequest ->
  CreateFleet
newCreateFleet pTargetCapacitySpecification_ =
  CreateFleet'
    { tagSpecifications = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      onDemandOptions = Prelude.Nothing,
      validFrom = Prelude.Nothing,
      replaceUnhealthyInstances = Prelude.Nothing,
      validUntil = Prelude.Nothing,
      excessCapacityTerminationPolicy = Prelude.Nothing,
      type' = Prelude.Nothing,
      spotOptions = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      terminateInstancesWithExpiration = Prelude.Nothing,
      launchTemplateConfigs = Prelude.mempty,
      targetCapacitySpecification =
        pTargetCapacitySpecification_
    }

-- | The key-value pair for tagging the EC2 Fleet request on creation. The
-- value for @ResourceType@ must be @fleet@, otherwise the fleet request
-- fails. To tag instances at launch, specify the tags in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template launch template>.
-- For information about tagging after launch, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html#tag-resources Tagging your resources>.
createFleet_tagSpecifications :: Lens.Lens' CreateFleet (Prelude.Maybe [TagSpecification])
createFleet_tagSpecifications = Lens.lens (\CreateFleet' {tagSpecifications} -> tagSpecifications) (\s@CreateFleet' {} a -> s {tagSpecifications = a} :: CreateFleet) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createFleet_dryRun :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Bool)
createFleet_dryRun = Lens.lens (\CreateFleet' {dryRun} -> dryRun) (\s@CreateFleet' {} a -> s {dryRun = a} :: CreateFleet)

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
createFleet_onDemandOptions :: Lens.Lens' CreateFleet (Prelude.Maybe OnDemandOptionsRequest)
createFleet_onDemandOptions = Lens.lens (\CreateFleet' {onDemandOptions} -> onDemandOptions) (\s@CreateFleet' {} a -> s {onDemandOptions = a} :: CreateFleet)

-- | The start date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The default is to start fulfilling
-- the request immediately.
createFleet_validFrom :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.UTCTime)
createFleet_validFrom = Lens.lens (\CreateFleet' {validFrom} -> validFrom) (\s@CreateFleet' {} a -> s {validFrom = a} :: CreateFleet) Prelude.. Lens.mapping Core._Time

-- | Indicates whether EC2 Fleet should replace unhealthy Spot Instances.
-- Supported only for fleets of type @maintain@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/manage-ec2-fleet.html#ec2-fleet-health-checks EC2 Fleet health checks>
-- in the /Amazon EC2 User Guide/.
createFleet_replaceUnhealthyInstances :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Bool)
createFleet_replaceUnhealthyInstances = Lens.lens (\CreateFleet' {replaceUnhealthyInstances} -> replaceUnhealthyInstances) (\s@CreateFleet' {} a -> s {replaceUnhealthyInstances = a} :: CreateFleet)

-- | The end date and time of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). At this point, no new EC2 Fleet
-- requests are placed or able to fulfill the request. If no value is
-- specified, the request remains until you cancel it.
createFleet_validUntil :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.UTCTime)
createFleet_validUntil = Lens.lens (\CreateFleet' {validUntil} -> validUntil) (\s@CreateFleet' {} a -> s {validUntil = a} :: CreateFleet) Prelude.. Lens.mapping Core._Time

-- | Indicates whether running instances should be terminated if the total
-- target capacity of the EC2 Fleet is decreased below the current size of
-- the EC2 Fleet.
createFleet_excessCapacityTerminationPolicy :: Lens.Lens' CreateFleet (Prelude.Maybe FleetExcessCapacityTerminationPolicy)
createFleet_excessCapacityTerminationPolicy = Lens.lens (\CreateFleet' {excessCapacityTerminationPolicy} -> excessCapacityTerminationPolicy) (\s@CreateFleet' {} a -> s {excessCapacityTerminationPolicy = a} :: CreateFleet)

-- | The type of request. The default value is @maintain@.
--
-- -   @maintain@ - The EC2 Fleet places an asynchronous request for your
--     desired capacity, and continues to maintain your desired Spot
--     capacity by replenishing interrupted Spot Instances.
--
-- -   @request@ - The EC2 Fleet places an asynchronous one-time request
--     for your desired capacity, but does submit Spot requests in
--     alternative capacity pools if Spot capacity is unavailable, and does
--     not maintain Spot capacity if Spot Instances are interrupted.
--
-- -   @instant@ - The EC2 Fleet places a synchronous one-time request for
--     your desired capacity, and returns errors for any instances that
--     could not be launched.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-configuration-strategies.html#ec2-fleet-request-type EC2 Fleet request types>
-- in the /Amazon EC2 User Guide/.
createFleet_type :: Lens.Lens' CreateFleet (Prelude.Maybe FleetType)
createFleet_type = Lens.lens (\CreateFleet' {type'} -> type') (\s@CreateFleet' {} a -> s {type' = a} :: CreateFleet)

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
createFleet_spotOptions :: Lens.Lens' CreateFleet (Prelude.Maybe SpotOptionsRequest)
createFleet_spotOptions = Lens.lens (\CreateFleet' {spotOptions} -> spotOptions) (\s@CreateFleet' {} a -> s {spotOptions = a} :: CreateFleet)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
createFleet_clientToken :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_clientToken = Lens.lens (\CreateFleet' {clientToken} -> clientToken) (\s@CreateFleet' {} a -> s {clientToken = a} :: CreateFleet)

-- | Indicates whether running instances should be terminated when the EC2
-- Fleet expires.
createFleet_terminateInstancesWithExpiration :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Bool)
createFleet_terminateInstancesWithExpiration = Lens.lens (\CreateFleet' {terminateInstancesWithExpiration} -> terminateInstancesWithExpiration) (\s@CreateFleet' {} a -> s {terminateInstancesWithExpiration = a} :: CreateFleet)

-- | The configuration for the EC2 Fleet.
createFleet_launchTemplateConfigs :: Lens.Lens' CreateFleet [FleetLaunchTemplateConfigRequest]
createFleet_launchTemplateConfigs = Lens.lens (\CreateFleet' {launchTemplateConfigs} -> launchTemplateConfigs) (\s@CreateFleet' {} a -> s {launchTemplateConfigs = a} :: CreateFleet) Prelude.. Lens._Coerce

-- | The number of units to request.
createFleet_targetCapacitySpecification :: Lens.Lens' CreateFleet TargetCapacitySpecificationRequest
createFleet_targetCapacitySpecification = Lens.lens (\CreateFleet' {targetCapacitySpecification} -> targetCapacitySpecification) (\s@CreateFleet' {} a -> s {targetCapacitySpecification = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Core..@? "fleetId")
            Prelude.<*> ( x Core..@? "fleetInstanceSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "errorSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleet

instance Prelude.NFData CreateFleet

instance Core.ToHeaders CreateFleet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateFleet where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFleet where
  toQuery CreateFleet' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateFleet" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "OnDemandOptions" Core.=: onDemandOptions,
        "ValidFrom" Core.=: validFrom,
        "ReplaceUnhealthyInstances"
          Core.=: replaceUnhealthyInstances,
        "ValidUntil" Core.=: validUntil,
        "ExcessCapacityTerminationPolicy"
          Core.=: excessCapacityTerminationPolicy,
        "Type" Core.=: type',
        "SpotOptions" Core.=: spotOptions,
        "ClientToken" Core.=: clientToken,
        "TerminateInstancesWithExpiration"
          Core.=: terminateInstancesWithExpiration,
        Core.toQueryList
          "LaunchTemplateConfigs"
          launchTemplateConfigs,
        "TargetCapacitySpecification"
          Core.=: targetCapacitySpecification
      ]

-- | /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | The ID of the EC2 Fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the instances that were launched by the fleet. Valid
    -- only when __Type__ is set to @instant@.
    instances :: Prelude.Maybe [CreateFleetInstance],
    -- | Information about the instances that could not be launched by the fleet.
    -- Valid only when __Type__ is set to @instant@.
    errors :: Prelude.Maybe [CreateFleetError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'createFleetResponse_fleetId' - The ID of the EC2 Fleet.
--
-- 'instances', 'createFleetResponse_instances' - Information about the instances that were launched by the fleet. Valid
-- only when __Type__ is set to @instant@.
--
-- 'errors', 'createFleetResponse_errors' - Information about the instances that could not be launched by the fleet.
-- Valid only when __Type__ is set to @instant@.
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ =
  CreateFleetResponse'
    { fleetId = Prelude.Nothing,
      instances = Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the EC2 Fleet.
createFleetResponse_fleetId :: Lens.Lens' CreateFleetResponse (Prelude.Maybe Prelude.Text)
createFleetResponse_fleetId = Lens.lens (\CreateFleetResponse' {fleetId} -> fleetId) (\s@CreateFleetResponse' {} a -> s {fleetId = a} :: CreateFleetResponse)

-- | Information about the instances that were launched by the fleet. Valid
-- only when __Type__ is set to @instant@.
createFleetResponse_instances :: Lens.Lens' CreateFleetResponse (Prelude.Maybe [CreateFleetInstance])
createFleetResponse_instances = Lens.lens (\CreateFleetResponse' {instances} -> instances) (\s@CreateFleetResponse' {} a -> s {instances = a} :: CreateFleetResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the instances that could not be launched by the fleet.
-- Valid only when __Type__ is set to @instant@.
createFleetResponse_errors :: Lens.Lens' CreateFleetResponse (Prelude.Maybe [CreateFleetError])
createFleetResponse_errors = Lens.lens (\CreateFleetResponse' {errors} -> errors) (\s@CreateFleetResponse' {} a -> s {errors = a} :: CreateFleetResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse
