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
-- Module      : Amazonka.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Instance request.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance requests>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- We strongly discourage using the RequestSpotInstances API because it is
-- a legacy API with no planned investment. For options for requesting Spot
-- Instances, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-best-practices.html#which-spot-request-method-to-use Which is the best Spot request method to use?>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon EC2 User Guide for Linux Instances/.
module Amazonka.EC2.RequestSpotInstances
  ( -- * Creating a Request
    RequestSpotInstances (..),
    newRequestSpotInstances,

    -- * Request Lenses
    requestSpotInstances_availabilityZoneGroup,
    requestSpotInstances_blockDurationMinutes,
    requestSpotInstances_clientToken,
    requestSpotInstances_dryRun,
    requestSpotInstances_instanceCount,
    requestSpotInstances_instanceInterruptionBehavior,
    requestSpotInstances_launchGroup,
    requestSpotInstances_launchSpecification,
    requestSpotInstances_spotPrice,
    requestSpotInstances_tagSpecifications,
    requestSpotInstances_type,
    requestSpotInstances_validFrom,
    requestSpotInstances_validUntil,

    -- * Destructuring the Response
    RequestSpotInstancesResponse (..),
    newRequestSpotInstancesResponse,

    -- * Response Lenses
    requestSpotInstancesResponse_spotInstanceRequests,
    requestSpotInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for RequestSpotInstances.
--
-- /See:/ 'newRequestSpotInstances' smart constructor.
data RequestSpotInstances = RequestSpotInstances'
  { -- | The user-specified name for a logical grouping of requests.
    --
    -- When you specify an Availability Zone group in a Spot Instance request,
    -- all Spot Instances in the request are launched in the same Availability
    -- Zone. Instance proximity is maintained with this parameter, but the
    -- choice of Availability Zone is not. The group applies only to requests
    -- for Spot Instances of the same instance type. Any additional Spot
    -- Instance requests that are specified with the same Availability Zone
    -- group name are launched in that same Availability Zone, as long as at
    -- least one instance from the group is still active.
    --
    -- If there is no active instance running in the Availability Zone group
    -- that you specify for a new Spot Instance request (all instances are
    -- terminated, the request is expired, or the maximum price you specified
    -- falls below current Spot price), then Amazon EC2 launches the instance
    -- in any Availability Zone where the constraint can be met. Consequently,
    -- the subsequent set of Spot Instances could be placed in a different zone
    -- from the original request, even if you specified the same Availability
    -- Zone group.
    --
    -- Default: Instances are launched in any available Availability Zone.
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | Deprecated.
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of Spot Instances to launch.
    --
    -- Default: 1
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The instance launch group. Launch groups are Spot Instances that launch
    -- together and terminate together.
    --
    -- Default: Instances are launched and terminated individually
    launchGroup :: Prelude.Maybe Prelude.Text,
    -- | The launch specification.
    launchSpecification :: Prelude.Maybe RequestSpotLaunchSpecification,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair for tagging the Spot Instance request on creation.
    -- The value for @ResourceType@ must be @spot-instances-request@, otherwise
    -- the Spot Instance request fails. To tag the Spot Instance request after
    -- it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The Spot Instance request type.
    --
    -- Default: @one-time@
    type' :: Prelude.Maybe SpotInstanceType,
    -- | The start date of the request. If this is a one-time request, the
    -- request becomes active at this date and time and remains active until
    -- all instances launch, the request expires, or the request is canceled.
    -- If the request is persistent, the request becomes active at this date
    -- and time and remains active until it expires or is canceled.
    --
    -- The specified start date and time cannot be equal to the current date
    -- and time. You must specify a start date and time that occurs after the
    -- current date and time.
    validFrom :: Prelude.Maybe Data.ISO8601,
    -- | The end date of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    --
    -- -   For a persistent request, the request remains active until the
    --     @ValidUntil@ date and time is reached. Otherwise, the request
    --     remains active until you cancel it.
    --
    -- -   For a one-time request, the request remains active until all
    --     instances launch, the request is canceled, or the @ValidUntil@ date
    --     and time is reached. By default, the request is valid for 7 days
    --     from the date the request was created.
    validUntil :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestSpotInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneGroup', 'requestSpotInstances_availabilityZoneGroup' - The user-specified name for a logical grouping of requests.
--
-- When you specify an Availability Zone group in a Spot Instance request,
-- all Spot Instances in the request are launched in the same Availability
-- Zone. Instance proximity is maintained with this parameter, but the
-- choice of Availability Zone is not. The group applies only to requests
-- for Spot Instances of the same instance type. Any additional Spot
-- Instance requests that are specified with the same Availability Zone
-- group name are launched in that same Availability Zone, as long as at
-- least one instance from the group is still active.
--
-- If there is no active instance running in the Availability Zone group
-- that you specify for a new Spot Instance request (all instances are
-- terminated, the request is expired, or the maximum price you specified
-- falls below current Spot price), then Amazon EC2 launches the instance
-- in any Availability Zone where the constraint can be met. Consequently,
-- the subsequent set of Spot Instances could be placed in a different zone
-- from the original request, even if you specified the same Availability
-- Zone group.
--
-- Default: Instances are launched in any available Availability Zone.
--
-- 'blockDurationMinutes', 'requestSpotInstances_blockDurationMinutes' - Deprecated.
--
-- 'clientToken', 'requestSpotInstances_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'dryRun', 'requestSpotInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceCount', 'requestSpotInstances_instanceCount' - The maximum number of Spot Instances to launch.
--
-- Default: 1
--
-- 'instanceInterruptionBehavior', 'requestSpotInstances_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'launchGroup', 'requestSpotInstances_launchGroup' - The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
--
-- 'launchSpecification', 'requestSpotInstances_launchSpecification' - The launch specification.
--
-- 'spotPrice', 'requestSpotInstances_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- 'tagSpecifications', 'requestSpotInstances_tagSpecifications' - The key-value pair for tagging the Spot Instance request on creation.
-- The value for @ResourceType@ must be @spot-instances-request@, otherwise
-- the Spot Instance request fails. To tag the Spot Instance request after
-- it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'type'', 'requestSpotInstances_type' - The Spot Instance request type.
--
-- Default: @one-time@
--
-- 'validFrom', 'requestSpotInstances_validFrom' - The start date of the request. If this is a one-time request, the
-- request becomes active at this date and time and remains active until
-- all instances launch, the request expires, or the request is canceled.
-- If the request is persistent, the request becomes active at this date
-- and time and remains active until it expires or is canceled.
--
-- The specified start date and time cannot be equal to the current date
-- and time. You must specify a start date and time that occurs after the
-- current date and time.
--
-- 'validUntil', 'requestSpotInstances_validUntil' - The end date of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- -   For a persistent request, the request remains active until the
--     @ValidUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, the request remains active until all
--     instances launch, the request is canceled, or the @ValidUntil@ date
--     and time is reached. By default, the request is valid for 7 days
--     from the date the request was created.
newRequestSpotInstances ::
  RequestSpotInstances
newRequestSpotInstances =
  RequestSpotInstances'
    { availabilityZoneGroup =
        Prelude.Nothing,
      blockDurationMinutes = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      launchGroup = Prelude.Nothing,
      launchSpecification = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      type' = Prelude.Nothing,
      validFrom = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | The user-specified name for a logical grouping of requests.
--
-- When you specify an Availability Zone group in a Spot Instance request,
-- all Spot Instances in the request are launched in the same Availability
-- Zone. Instance proximity is maintained with this parameter, but the
-- choice of Availability Zone is not. The group applies only to requests
-- for Spot Instances of the same instance type. Any additional Spot
-- Instance requests that are specified with the same Availability Zone
-- group name are launched in that same Availability Zone, as long as at
-- least one instance from the group is still active.
--
-- If there is no active instance running in the Availability Zone group
-- that you specify for a new Spot Instance request (all instances are
-- terminated, the request is expired, or the maximum price you specified
-- falls below current Spot price), then Amazon EC2 launches the instance
-- in any Availability Zone where the constraint can be met. Consequently,
-- the subsequent set of Spot Instances could be placed in a different zone
-- from the original request, even if you specified the same Availability
-- Zone group.
--
-- Default: Instances are launched in any available Availability Zone.
requestSpotInstances_availabilityZoneGroup :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_availabilityZoneGroup = Lens.lens (\RequestSpotInstances' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@RequestSpotInstances' {} a -> s {availabilityZoneGroup = a} :: RequestSpotInstances)

-- | Deprecated.
requestSpotInstances_blockDurationMinutes :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Int)
requestSpotInstances_blockDurationMinutes = Lens.lens (\RequestSpotInstances' {blockDurationMinutes} -> blockDurationMinutes) (\s@RequestSpotInstances' {} a -> s {blockDurationMinutes = a} :: RequestSpotInstances)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon EC2 User Guide for Linux Instances/.
requestSpotInstances_clientToken :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_clientToken = Lens.lens (\RequestSpotInstances' {clientToken} -> clientToken) (\s@RequestSpotInstances' {} a -> s {clientToken = a} :: RequestSpotInstances)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
requestSpotInstances_dryRun :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Bool)
requestSpotInstances_dryRun = Lens.lens (\RequestSpotInstances' {dryRun} -> dryRun) (\s@RequestSpotInstances' {} a -> s {dryRun = a} :: RequestSpotInstances)

-- | The maximum number of Spot Instances to launch.
--
-- Default: 1
requestSpotInstances_instanceCount :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Int)
requestSpotInstances_instanceCount = Lens.lens (\RequestSpotInstances' {instanceCount} -> instanceCount) (\s@RequestSpotInstances' {} a -> s {instanceCount = a} :: RequestSpotInstances)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
requestSpotInstances_instanceInterruptionBehavior :: Lens.Lens' RequestSpotInstances (Prelude.Maybe InstanceInterruptionBehavior)
requestSpotInstances_instanceInterruptionBehavior = Lens.lens (\RequestSpotInstances' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@RequestSpotInstances' {} a -> s {instanceInterruptionBehavior = a} :: RequestSpotInstances)

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
requestSpotInstances_launchGroup :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_launchGroup = Lens.lens (\RequestSpotInstances' {launchGroup} -> launchGroup) (\s@RequestSpotInstances' {} a -> s {launchGroup = a} :: RequestSpotInstances)

-- | The launch specification.
requestSpotInstances_launchSpecification :: Lens.Lens' RequestSpotInstances (Prelude.Maybe RequestSpotLaunchSpecification)
requestSpotInstances_launchSpecification = Lens.lens (\RequestSpotInstances' {launchSpecification} -> launchSpecification) (\s@RequestSpotInstances' {} a -> s {launchSpecification = a} :: RequestSpotInstances)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
requestSpotInstances_spotPrice :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_spotPrice = Lens.lens (\RequestSpotInstances' {spotPrice} -> spotPrice) (\s@RequestSpotInstances' {} a -> s {spotPrice = a} :: RequestSpotInstances)

-- | The key-value pair for tagging the Spot Instance request on creation.
-- The value for @ResourceType@ must be @spot-instances-request@, otherwise
-- the Spot Instance request fails. To tag the Spot Instance request after
-- it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
requestSpotInstances_tagSpecifications :: Lens.Lens' RequestSpotInstances (Prelude.Maybe [TagSpecification])
requestSpotInstances_tagSpecifications = Lens.lens (\RequestSpotInstances' {tagSpecifications} -> tagSpecifications) (\s@RequestSpotInstances' {} a -> s {tagSpecifications = a} :: RequestSpotInstances) Prelude.. Lens.mapping Lens.coerced

-- | The Spot Instance request type.
--
-- Default: @one-time@
requestSpotInstances_type :: Lens.Lens' RequestSpotInstances (Prelude.Maybe SpotInstanceType)
requestSpotInstances_type = Lens.lens (\RequestSpotInstances' {type'} -> type') (\s@RequestSpotInstances' {} a -> s {type' = a} :: RequestSpotInstances)

-- | The start date of the request. If this is a one-time request, the
-- request becomes active at this date and time and remains active until
-- all instances launch, the request expires, or the request is canceled.
-- If the request is persistent, the request becomes active at this date
-- and time and remains active until it expires or is canceled.
--
-- The specified start date and time cannot be equal to the current date
-- and time. You must specify a start date and time that occurs after the
-- current date and time.
requestSpotInstances_validFrom :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.UTCTime)
requestSpotInstances_validFrom = Lens.lens (\RequestSpotInstances' {validFrom} -> validFrom) (\s@RequestSpotInstances' {} a -> s {validFrom = a} :: RequestSpotInstances) Prelude.. Lens.mapping Data._Time

-- | The end date of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- -   For a persistent request, the request remains active until the
--     @ValidUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, the request remains active until all
--     instances launch, the request is canceled, or the @ValidUntil@ date
--     and time is reached. By default, the request is valid for 7 days
--     from the date the request was created.
requestSpotInstances_validUntil :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.UTCTime)
requestSpotInstances_validUntil = Lens.lens (\RequestSpotInstances' {validUntil} -> validUntil) (\s@RequestSpotInstances' {} a -> s {validUntil = a} :: RequestSpotInstances) Prelude.. Lens.mapping Data._Time

instance Core.AWSRequest RequestSpotInstances where
  type
    AWSResponse RequestSpotInstances =
      RequestSpotInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RequestSpotInstancesResponse'
            Prelude.<$> ( x
                            Data..@? "spotInstanceRequestSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RequestSpotInstances where
  hashWithSalt _salt RequestSpotInstances' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZoneGroup
      `Prelude.hashWithSalt` blockDurationMinutes
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceInterruptionBehavior
      `Prelude.hashWithSalt` launchGroup
      `Prelude.hashWithSalt` launchSpecification
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` validFrom
      `Prelude.hashWithSalt` validUntil

instance Prelude.NFData RequestSpotInstances where
  rnf RequestSpotInstances' {..} =
    Prelude.rnf availabilityZoneGroup `Prelude.seq`
      Prelude.rnf blockDurationMinutes `Prelude.seq`
        Prelude.rnf clientToken `Prelude.seq`
          Prelude.rnf dryRun `Prelude.seq`
            Prelude.rnf instanceCount `Prelude.seq`
              Prelude.rnf instanceInterruptionBehavior `Prelude.seq`
                Prelude.rnf launchGroup `Prelude.seq`
                  Prelude.rnf launchSpecification `Prelude.seq`
                    Prelude.rnf spotPrice `Prelude.seq`
                      Prelude.rnf tagSpecifications `Prelude.seq`
                        Prelude.rnf type' `Prelude.seq`
                          Prelude.rnf validFrom `Prelude.seq`
                            Prelude.rnf validUntil

instance Data.ToHeaders RequestSpotInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RequestSpotInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery RequestSpotInstances where
  toQuery RequestSpotInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RequestSpotInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AvailabilityZoneGroup"
          Data.=: availabilityZoneGroup,
        "BlockDurationMinutes" Data.=: blockDurationMinutes,
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "InstanceCount" Data.=: instanceCount,
        "InstanceInterruptionBehavior"
          Data.=: instanceInterruptionBehavior,
        "LaunchGroup" Data.=: launchGroup,
        "LaunchSpecification" Data.=: launchSpecification,
        "SpotPrice" Data.=: spotPrice,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "Type" Data.=: type',
        "ValidFrom" Data.=: validFrom,
        "ValidUntil" Data.=: validUntil
      ]

-- | Contains the output of RequestSpotInstances.
--
-- /See:/ 'newRequestSpotInstancesResponse' smart constructor.
data RequestSpotInstancesResponse = RequestSpotInstancesResponse'
  { -- | One or more Spot Instance requests.
    spotInstanceRequests :: Prelude.Maybe [SpotInstanceRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestSpotInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotInstanceRequests', 'requestSpotInstancesResponse_spotInstanceRequests' - One or more Spot Instance requests.
--
-- 'httpStatus', 'requestSpotInstancesResponse_httpStatus' - The response's http status code.
newRequestSpotInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RequestSpotInstancesResponse
newRequestSpotInstancesResponse pHttpStatus_ =
  RequestSpotInstancesResponse'
    { spotInstanceRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One or more Spot Instance requests.
requestSpotInstancesResponse_spotInstanceRequests :: Lens.Lens' RequestSpotInstancesResponse (Prelude.Maybe [SpotInstanceRequest])
requestSpotInstancesResponse_spotInstanceRequests = Lens.lens (\RequestSpotInstancesResponse' {spotInstanceRequests} -> spotInstanceRequests) (\s@RequestSpotInstancesResponse' {} a -> s {spotInstanceRequests = a} :: RequestSpotInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
requestSpotInstancesResponse_httpStatus :: Lens.Lens' RequestSpotInstancesResponse Prelude.Int
requestSpotInstancesResponse_httpStatus = Lens.lens (\RequestSpotInstancesResponse' {httpStatus} -> httpStatus) (\s@RequestSpotInstancesResponse' {} a -> s {httpStatus = a} :: RequestSpotInstancesResponse)

instance Prelude.NFData RequestSpotInstancesResponse where
  rnf RequestSpotInstancesResponse' {..} =
    Prelude.rnf spotInstanceRequests `Prelude.seq`
      Prelude.rnf httpStatus
