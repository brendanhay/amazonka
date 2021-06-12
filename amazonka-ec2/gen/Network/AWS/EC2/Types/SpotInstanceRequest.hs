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
-- Module      : Network.AWS.EC2.Types.SpotInstanceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.LaunchSpecification
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.EC2.Types.SpotInstanceState
import Network.AWS.EC2.Types.SpotInstanceStateFault
import Network.AWS.EC2.Types.SpotInstanceStatus
import Network.AWS.EC2.Types.SpotInstanceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a Spot Instance request.
--
-- /See:/ 'newSpotInstanceRequest' smart constructor.
data SpotInstanceRequest = SpotInstanceRequest'
  { -- | If you specified a duration and your Spot Instance request was
    -- fulfilled, this is the fixed hourly price in effect for the Spot
    -- Instance while it runs.
    actualBlockHourlyPrice :: Core.Maybe Core.Text,
    -- | The status code and status message describing the Spot Instance request.
    status :: Core.Maybe SpotInstanceStatus,
    -- | The instance ID, if an instance has been launched to fulfill the Spot
    -- Instance request.
    instanceId :: Core.Maybe Core.Text,
    -- | The Availability Zone in which the request is launched.
    launchedAvailabilityZone :: Core.Maybe Core.Text,
    -- | The start date of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
    -- date and time.
    validFrom :: Core.Maybe Core.ISO8601,
    -- | The maximum price per hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Core.Maybe Core.Text,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Core.Maybe SpotInstanceStateFault,
    -- | The duration for the Spot Instance, in minutes.
    blockDurationMinutes :: Core.Maybe Core.Int,
    -- | The instance launch group. Launch groups are Spot Instances that launch
    -- together and terminate together.
    launchGroup :: Core.Maybe Core.Text,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Core.Maybe InstanceInterruptionBehavior,
    -- | The state of the Spot Instance request. Spot status information helps
    -- track your Spot Instance requests. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    state :: Core.Maybe SpotInstanceState,
    -- | The end date of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    --
    -- -   For a persistent request, the request remains active until the
    --     @validUntil@ date and time is reached. Otherwise, the request
    --     remains active until you cancel it.
    --
    -- -   For a one-time request, the request remains active until all
    --     instances launch, the request is canceled, or the @validUntil@ date
    --     and time is reached. By default, the request is valid for 7 days
    --     from the date the request was created.
    validUntil :: Core.Maybe Core.ISO8601,
    -- | Any tags assigned to the resource.
    tags :: Core.Maybe [Tag],
    -- | The date and time when the Spot Instance request was created, in UTC
    -- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    createTime :: Core.Maybe Core.ISO8601,
    -- | Additional information for launching instances.
    launchSpecification :: Core.Maybe LaunchSpecification,
    -- | The Spot Instance request type.
    type' :: Core.Maybe SpotInstanceType,
    -- | The Availability Zone group. If you specify the same Availability Zone
    -- group for all Spot Instance requests, all Spot Instances are launched in
    -- the same Availability Zone.
    availabilityZoneGroup :: Core.Maybe Core.Text,
    -- | The product description associated with the Spot Instance.
    productDescription :: Core.Maybe RIProductDescription,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpotInstanceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actualBlockHourlyPrice', 'spotInstanceRequest_actualBlockHourlyPrice' - If you specified a duration and your Spot Instance request was
-- fulfilled, this is the fixed hourly price in effect for the Spot
-- Instance while it runs.
--
-- 'status', 'spotInstanceRequest_status' - The status code and status message describing the Spot Instance request.
--
-- 'instanceId', 'spotInstanceRequest_instanceId' - The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
--
-- 'launchedAvailabilityZone', 'spotInstanceRequest_launchedAvailabilityZone' - The Availability Zone in which the request is launched.
--
-- 'validFrom', 'spotInstanceRequest_validFrom' - The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
-- date and time.
--
-- 'spotPrice', 'spotInstanceRequest_spotPrice' - The maximum price per hour that you are willing to pay for a Spot
-- Instance.
--
-- 'fault', 'spotInstanceRequest_fault' - The fault codes for the Spot Instance request, if any.
--
-- 'blockDurationMinutes', 'spotInstanceRequest_blockDurationMinutes' - The duration for the Spot Instance, in minutes.
--
-- 'launchGroup', 'spotInstanceRequest_launchGroup' - The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- 'instanceInterruptionBehavior', 'spotInstanceRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- 'state', 'spotInstanceRequest_state' - The state of the Spot Instance request. Spot status information helps
-- track your Spot Instance requests. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'validUntil', 'spotInstanceRequest_validUntil' - The end date of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- -   For a persistent request, the request remains active until the
--     @validUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, the request remains active until all
--     instances launch, the request is canceled, or the @validUntil@ date
--     and time is reached. By default, the request is valid for 7 days
--     from the date the request was created.
--
-- 'tags', 'spotInstanceRequest_tags' - Any tags assigned to the resource.
--
-- 'createTime', 'spotInstanceRequest_createTime' - The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'launchSpecification', 'spotInstanceRequest_launchSpecification' - Additional information for launching instances.
--
-- 'type'', 'spotInstanceRequest_type' - The Spot Instance request type.
--
-- 'availabilityZoneGroup', 'spotInstanceRequest_availabilityZoneGroup' - The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
--
-- 'productDescription', 'spotInstanceRequest_productDescription' - The product description associated with the Spot Instance.
--
-- 'spotInstanceRequestId', 'spotInstanceRequest_spotInstanceRequestId' - The ID of the Spot Instance request.
newSpotInstanceRequest ::
  SpotInstanceRequest
newSpotInstanceRequest =
  SpotInstanceRequest'
    { actualBlockHourlyPrice =
        Core.Nothing,
      status = Core.Nothing,
      instanceId = Core.Nothing,
      launchedAvailabilityZone = Core.Nothing,
      validFrom = Core.Nothing,
      spotPrice = Core.Nothing,
      fault = Core.Nothing,
      blockDurationMinutes = Core.Nothing,
      launchGroup = Core.Nothing,
      instanceInterruptionBehavior = Core.Nothing,
      state = Core.Nothing,
      validUntil = Core.Nothing,
      tags = Core.Nothing,
      createTime = Core.Nothing,
      launchSpecification = Core.Nothing,
      type' = Core.Nothing,
      availabilityZoneGroup = Core.Nothing,
      productDescription = Core.Nothing,
      spotInstanceRequestId = Core.Nothing
    }

-- | If you specified a duration and your Spot Instance request was
-- fulfilled, this is the fixed hourly price in effect for the Spot
-- Instance while it runs.
spotInstanceRequest_actualBlockHourlyPrice :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_actualBlockHourlyPrice = Lens.lens (\SpotInstanceRequest' {actualBlockHourlyPrice} -> actualBlockHourlyPrice) (\s@SpotInstanceRequest' {} a -> s {actualBlockHourlyPrice = a} :: SpotInstanceRequest)

-- | The status code and status message describing the Spot Instance request.
spotInstanceRequest_status :: Lens.Lens' SpotInstanceRequest (Core.Maybe SpotInstanceStatus)
spotInstanceRequest_status = Lens.lens (\SpotInstanceRequest' {status} -> status) (\s@SpotInstanceRequest' {} a -> s {status = a} :: SpotInstanceRequest)

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
spotInstanceRequest_instanceId :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_instanceId = Lens.lens (\SpotInstanceRequest' {instanceId} -> instanceId) (\s@SpotInstanceRequest' {} a -> s {instanceId = a} :: SpotInstanceRequest)

-- | The Availability Zone in which the request is launched.
spotInstanceRequest_launchedAvailabilityZone :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_launchedAvailabilityZone = Lens.lens (\SpotInstanceRequest' {launchedAvailabilityZone} -> launchedAvailabilityZone) (\s@SpotInstanceRequest' {} a -> s {launchedAvailabilityZone = a} :: SpotInstanceRequest)

-- | The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
-- date and time.
spotInstanceRequest_validFrom :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.UTCTime)
spotInstanceRequest_validFrom = Lens.lens (\SpotInstanceRequest' {validFrom} -> validFrom) (\s@SpotInstanceRequest' {} a -> s {validFrom = a} :: SpotInstanceRequest) Core.. Lens.mapping Core._Time

-- | The maximum price per hour that you are willing to pay for a Spot
-- Instance.
spotInstanceRequest_spotPrice :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_spotPrice = Lens.lens (\SpotInstanceRequest' {spotPrice} -> spotPrice) (\s@SpotInstanceRequest' {} a -> s {spotPrice = a} :: SpotInstanceRequest)

-- | The fault codes for the Spot Instance request, if any.
spotInstanceRequest_fault :: Lens.Lens' SpotInstanceRequest (Core.Maybe SpotInstanceStateFault)
spotInstanceRequest_fault = Lens.lens (\SpotInstanceRequest' {fault} -> fault) (\s@SpotInstanceRequest' {} a -> s {fault = a} :: SpotInstanceRequest)

-- | The duration for the Spot Instance, in minutes.
spotInstanceRequest_blockDurationMinutes :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Int)
spotInstanceRequest_blockDurationMinutes = Lens.lens (\SpotInstanceRequest' {blockDurationMinutes} -> blockDurationMinutes) (\s@SpotInstanceRequest' {} a -> s {blockDurationMinutes = a} :: SpotInstanceRequest)

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
spotInstanceRequest_launchGroup :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_launchGroup = Lens.lens (\SpotInstanceRequest' {launchGroup} -> launchGroup) (\s@SpotInstanceRequest' {} a -> s {launchGroup = a} :: SpotInstanceRequest)

-- | The behavior when a Spot Instance is interrupted.
spotInstanceRequest_instanceInterruptionBehavior :: Lens.Lens' SpotInstanceRequest (Core.Maybe InstanceInterruptionBehavior)
spotInstanceRequest_instanceInterruptionBehavior = Lens.lens (\SpotInstanceRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotInstanceRequest' {} a -> s {instanceInterruptionBehavior = a} :: SpotInstanceRequest)

-- | The state of the Spot Instance request. Spot status information helps
-- track your Spot Instance requests. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status>
-- in the /Amazon EC2 User Guide for Linux Instances/.
spotInstanceRequest_state :: Lens.Lens' SpotInstanceRequest (Core.Maybe SpotInstanceState)
spotInstanceRequest_state = Lens.lens (\SpotInstanceRequest' {state} -> state) (\s@SpotInstanceRequest' {} a -> s {state = a} :: SpotInstanceRequest)

-- | The end date of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- -   For a persistent request, the request remains active until the
--     @validUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, the request remains active until all
--     instances launch, the request is canceled, or the @validUntil@ date
--     and time is reached. By default, the request is valid for 7 days
--     from the date the request was created.
spotInstanceRequest_validUntil :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.UTCTime)
spotInstanceRequest_validUntil = Lens.lens (\SpotInstanceRequest' {validUntil} -> validUntil) (\s@SpotInstanceRequest' {} a -> s {validUntil = a} :: SpotInstanceRequest) Core.. Lens.mapping Core._Time

-- | Any tags assigned to the resource.
spotInstanceRequest_tags :: Lens.Lens' SpotInstanceRequest (Core.Maybe [Tag])
spotInstanceRequest_tags = Lens.lens (\SpotInstanceRequest' {tags} -> tags) (\s@SpotInstanceRequest' {} a -> s {tags = a} :: SpotInstanceRequest) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotInstanceRequest_createTime :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.UTCTime)
spotInstanceRequest_createTime = Lens.lens (\SpotInstanceRequest' {createTime} -> createTime) (\s@SpotInstanceRequest' {} a -> s {createTime = a} :: SpotInstanceRequest) Core.. Lens.mapping Core._Time

-- | Additional information for launching instances.
spotInstanceRequest_launchSpecification :: Lens.Lens' SpotInstanceRequest (Core.Maybe LaunchSpecification)
spotInstanceRequest_launchSpecification = Lens.lens (\SpotInstanceRequest' {launchSpecification} -> launchSpecification) (\s@SpotInstanceRequest' {} a -> s {launchSpecification = a} :: SpotInstanceRequest)

-- | The Spot Instance request type.
spotInstanceRequest_type :: Lens.Lens' SpotInstanceRequest (Core.Maybe SpotInstanceType)
spotInstanceRequest_type = Lens.lens (\SpotInstanceRequest' {type'} -> type') (\s@SpotInstanceRequest' {} a -> s {type' = a} :: SpotInstanceRequest)

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
spotInstanceRequest_availabilityZoneGroup :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_availabilityZoneGroup = Lens.lens (\SpotInstanceRequest' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@SpotInstanceRequest' {} a -> s {availabilityZoneGroup = a} :: SpotInstanceRequest)

-- | The product description associated with the Spot Instance.
spotInstanceRequest_productDescription :: Lens.Lens' SpotInstanceRequest (Core.Maybe RIProductDescription)
spotInstanceRequest_productDescription = Lens.lens (\SpotInstanceRequest' {productDescription} -> productDescription) (\s@SpotInstanceRequest' {} a -> s {productDescription = a} :: SpotInstanceRequest)

-- | The ID of the Spot Instance request.
spotInstanceRequest_spotInstanceRequestId :: Lens.Lens' SpotInstanceRequest (Core.Maybe Core.Text)
spotInstanceRequest_spotInstanceRequestId = Lens.lens (\SpotInstanceRequest' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@SpotInstanceRequest' {} a -> s {spotInstanceRequestId = a} :: SpotInstanceRequest)

instance Core.FromXML SpotInstanceRequest where
  parseXML x =
    SpotInstanceRequest'
      Core.<$> (x Core..@? "actualBlockHourlyPrice")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "launchedAvailabilityZone")
      Core.<*> (x Core..@? "validFrom")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "fault")
      Core.<*> (x Core..@? "blockDurationMinutes")
      Core.<*> (x Core..@? "launchGroup")
      Core.<*> (x Core..@? "instanceInterruptionBehavior")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "validUntil")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "createTime")
      Core.<*> (x Core..@? "launchSpecification")
      Core.<*> (x Core..@? "type")
      Core.<*> (x Core..@? "availabilityZoneGroup")
      Core.<*> (x Core..@? "productDescription")
      Core.<*> (x Core..@? "spotInstanceRequestId")

instance Core.Hashable SpotInstanceRequest

instance Core.NFData SpotInstanceRequest
