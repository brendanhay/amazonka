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
-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Instance request.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance requests>
-- in the /Amazon EC2 User Guide for Linux Instances/.
module Network.AWS.EC2.RequestSpotInstances
  ( -- * Creating a Request
    RequestSpotInstances (..),
    newRequestSpotInstances,

    -- * Request Lenses
    requestSpotInstances_tagSpecifications,
    requestSpotInstances_dryRun,
    requestSpotInstances_validFrom,
    requestSpotInstances_spotPrice,
    requestSpotInstances_blockDurationMinutes,
    requestSpotInstances_launchGroup,
    requestSpotInstances_instanceInterruptionBehavior,
    requestSpotInstances_validUntil,
    requestSpotInstances_launchSpecification,
    requestSpotInstances_type,
    requestSpotInstances_availabilityZoneGroup,
    requestSpotInstances_clientToken,
    requestSpotInstances_instanceCount,

    -- * Destructuring the Response
    RequestSpotInstancesResponse (..),
    newRequestSpotInstancesResponse,

    -- * Response Lenses
    requestSpotInstancesResponse_spotInstanceRequests,
    requestSpotInstancesResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RequestSpotInstances.
--
-- /See:/ 'newRequestSpotInstances' smart constructor.
data RequestSpotInstances = RequestSpotInstances'
  { -- | The key-value pair for tagging the Spot Instance request on creation.
    -- The value for @ResourceType@ must be @spot-instances-request@, otherwise
    -- the Spot Instance request fails. To tag the Spot Instance request after
    -- it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The start date of the request. If this is a one-time request, the
    -- request becomes active at this date and time and remains active until
    -- all instances launch, the request expires, or the request is canceled.
    -- If the request is persistent, the request becomes active at this date
    -- and time and remains active until it expires or is canceled.
    --
    -- The specified start date and time cannot be equal to the current date
    -- and time. You must specify a start date and time that occurs after the
    -- current date and time.
    validFrom :: Prelude.Maybe Prelude.ISO8601,
    -- | The maximum price per hour that you are willing to pay for a Spot
    -- Instance. The default is the On-Demand price.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The required duration for the Spot Instances (also known as Spot
    -- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
    -- 240, 300, or 360).
    --
    -- The duration period starts as soon as your Spot Instance receives its
    -- instance ID. At the end of the duration period, Amazon EC2 marks the
    -- Spot Instance for termination and provides a Spot Instance termination
    -- notice, which gives the instance a two-minute warning before it
    -- terminates.
    --
    -- You can\'t specify an Availability Zone group or a launch group if you
    -- specify a duration.
    --
    -- New accounts or accounts with no previous billing history with AWS are
    -- not eligible for Spot Instances with a defined duration (also known as
    -- Spot blocks).
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The instance launch group. Launch groups are Spot Instances that launch
    -- together and terminate together.
    --
    -- Default: Instances are launched and terminated individually
    launchGroup :: Prelude.Maybe Prelude.Text,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
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
    validUntil :: Prelude.Maybe Prelude.ISO8601,
    -- | The launch specification.
    launchSpecification :: Prelude.Maybe RequestSpotLaunchSpecification,
    -- | The Spot Instance request type.
    --
    -- Default: @one-time@
    type' :: Prelude.Maybe SpotInstanceType,
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
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Spot Instances to launch.
    --
    -- Default: 1
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequestSpotInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'requestSpotInstances_tagSpecifications' - The key-value pair for tagging the Spot Instance request on creation.
-- The value for @ResourceType@ must be @spot-instances-request@, otherwise
-- the Spot Instance request fails. To tag the Spot Instance request after
-- it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'dryRun', 'requestSpotInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- 'spotPrice', 'requestSpotInstances_spotPrice' - The maximum price per hour that you are willing to pay for a Spot
-- Instance. The default is the On-Demand price.
--
-- 'blockDurationMinutes', 'requestSpotInstances_blockDurationMinutes' - The required duration for the Spot Instances (also known as Spot
-- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
-- 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its
-- instance ID. At the end of the duration period, Amazon EC2 marks the
-- Spot Instance for termination and provides a Spot Instance termination
-- notice, which gives the instance a two-minute warning before it
-- terminates.
--
-- You can\'t specify an Availability Zone group or a launch group if you
-- specify a duration.
--
-- New accounts or accounts with no previous billing history with AWS are
-- not eligible for Spot Instances with a defined duration (also known as
-- Spot blocks).
--
-- 'launchGroup', 'requestSpotInstances_launchGroup' - The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
--
-- 'instanceInterruptionBehavior', 'requestSpotInstances_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
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
--
-- 'launchSpecification', 'requestSpotInstances_launchSpecification' - The launch specification.
--
-- 'type'', 'requestSpotInstances_type' - The Spot Instance request type.
--
-- Default: @one-time@
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
-- 'clientToken', 'requestSpotInstances_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'instanceCount', 'requestSpotInstances_instanceCount' - The maximum number of Spot Instances to launch.
--
-- Default: 1
newRequestSpotInstances ::
  RequestSpotInstances
newRequestSpotInstances =
  RequestSpotInstances'
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      validFrom = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      blockDurationMinutes = Prelude.Nothing,
      launchGroup = Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      validUntil = Prelude.Nothing,
      launchSpecification = Prelude.Nothing,
      type' = Prelude.Nothing,
      availabilityZoneGroup = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | The key-value pair for tagging the Spot Instance request on creation.
-- The value for @ResourceType@ must be @spot-instances-request@, otherwise
-- the Spot Instance request fails. To tag the Spot Instance request after
-- it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
requestSpotInstances_tagSpecifications :: Lens.Lens' RequestSpotInstances (Prelude.Maybe [TagSpecification])
requestSpotInstances_tagSpecifications = Lens.lens (\RequestSpotInstances' {tagSpecifications} -> tagSpecifications) (\s@RequestSpotInstances' {} a -> s {tagSpecifications = a} :: RequestSpotInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
requestSpotInstances_dryRun :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Bool)
requestSpotInstances_dryRun = Lens.lens (\RequestSpotInstances' {dryRun} -> dryRun) (\s@RequestSpotInstances' {} a -> s {dryRun = a} :: RequestSpotInstances)

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
requestSpotInstances_validFrom = Lens.lens (\RequestSpotInstances' {validFrom} -> validFrom) (\s@RequestSpotInstances' {} a -> s {validFrom = a} :: RequestSpotInstances) Prelude.. Lens.mapping Prelude._Time

-- | The maximum price per hour that you are willing to pay for a Spot
-- Instance. The default is the On-Demand price.
requestSpotInstances_spotPrice :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_spotPrice = Lens.lens (\RequestSpotInstances' {spotPrice} -> spotPrice) (\s@RequestSpotInstances' {} a -> s {spotPrice = a} :: RequestSpotInstances)

-- | The required duration for the Spot Instances (also known as Spot
-- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
-- 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its
-- instance ID. At the end of the duration period, Amazon EC2 marks the
-- Spot Instance for termination and provides a Spot Instance termination
-- notice, which gives the instance a two-minute warning before it
-- terminates.
--
-- You can\'t specify an Availability Zone group or a launch group if you
-- specify a duration.
--
-- New accounts or accounts with no previous billing history with AWS are
-- not eligible for Spot Instances with a defined duration (also known as
-- Spot blocks).
requestSpotInstances_blockDurationMinutes :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Int)
requestSpotInstances_blockDurationMinutes = Lens.lens (\RequestSpotInstances' {blockDurationMinutes} -> blockDurationMinutes) (\s@RequestSpotInstances' {} a -> s {blockDurationMinutes = a} :: RequestSpotInstances)

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- Default: Instances are launched and terminated individually
requestSpotInstances_launchGroup :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_launchGroup = Lens.lens (\RequestSpotInstances' {launchGroup} -> launchGroup) (\s@RequestSpotInstances' {} a -> s {launchGroup = a} :: RequestSpotInstances)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
requestSpotInstances_instanceInterruptionBehavior :: Lens.Lens' RequestSpotInstances (Prelude.Maybe InstanceInterruptionBehavior)
requestSpotInstances_instanceInterruptionBehavior = Lens.lens (\RequestSpotInstances' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@RequestSpotInstances' {} a -> s {instanceInterruptionBehavior = a} :: RequestSpotInstances)

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
requestSpotInstances_validUntil = Lens.lens (\RequestSpotInstances' {validUntil} -> validUntil) (\s@RequestSpotInstances' {} a -> s {validUntil = a} :: RequestSpotInstances) Prelude.. Lens.mapping Prelude._Time

-- | The launch specification.
requestSpotInstances_launchSpecification :: Lens.Lens' RequestSpotInstances (Prelude.Maybe RequestSpotLaunchSpecification)
requestSpotInstances_launchSpecification = Lens.lens (\RequestSpotInstances' {launchSpecification} -> launchSpecification) (\s@RequestSpotInstances' {} a -> s {launchSpecification = a} :: RequestSpotInstances)

-- | The Spot Instance request type.
--
-- Default: @one-time@
requestSpotInstances_type :: Lens.Lens' RequestSpotInstances (Prelude.Maybe SpotInstanceType)
requestSpotInstances_type = Lens.lens (\RequestSpotInstances' {type'} -> type') (\s@RequestSpotInstances' {} a -> s {type' = a} :: RequestSpotInstances)

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

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon EC2 User Guide for Linux Instances/.
requestSpotInstances_clientToken :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Text)
requestSpotInstances_clientToken = Lens.lens (\RequestSpotInstances' {clientToken} -> clientToken) (\s@RequestSpotInstances' {} a -> s {clientToken = a} :: RequestSpotInstances)

-- | The maximum number of Spot Instances to launch.
--
-- Default: 1
requestSpotInstances_instanceCount :: Lens.Lens' RequestSpotInstances (Prelude.Maybe Prelude.Int)
requestSpotInstances_instanceCount = Lens.lens (\RequestSpotInstances' {instanceCount} -> instanceCount) (\s@RequestSpotInstances' {} a -> s {instanceCount = a} :: RequestSpotInstances)

instance Prelude.AWSRequest RequestSpotInstances where
  type
    Rs RequestSpotInstances =
      RequestSpotInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RequestSpotInstancesResponse'
            Prelude.<$> ( x Prelude..@? "spotInstanceRequestSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RequestSpotInstances

instance Prelude.NFData RequestSpotInstances

instance Prelude.ToHeaders RequestSpotInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RequestSpotInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RequestSpotInstances where
  toQuery RequestSpotInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RequestSpotInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "ValidFrom" Prelude.=: validFrom,
        "SpotPrice" Prelude.=: spotPrice,
        "BlockDurationMinutes"
          Prelude.=: blockDurationMinutes,
        "LaunchGroup" Prelude.=: launchGroup,
        "InstanceInterruptionBehavior"
          Prelude.=: instanceInterruptionBehavior,
        "ValidUntil" Prelude.=: validUntil,
        "LaunchSpecification" Prelude.=: launchSpecification,
        "Type" Prelude.=: type',
        "AvailabilityZoneGroup"
          Prelude.=: availabilityZoneGroup,
        "ClientToken" Prelude.=: clientToken,
        "InstanceCount" Prelude.=: instanceCount
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
requestSpotInstancesResponse_spotInstanceRequests = Lens.lens (\RequestSpotInstancesResponse' {spotInstanceRequests} -> spotInstanceRequests) (\s@RequestSpotInstancesResponse' {} a -> s {spotInstanceRequests = a} :: RequestSpotInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
requestSpotInstancesResponse_httpStatus :: Lens.Lens' RequestSpotInstancesResponse Prelude.Int
requestSpotInstancesResponse_httpStatus = Lens.lens (\RequestSpotInstancesResponse' {httpStatus} -> httpStatus) (\s@RequestSpotInstancesResponse' {} a -> s {httpStatus = a} :: RequestSpotInstancesResponse)

instance Prelude.NFData RequestSpotInstancesResponse
