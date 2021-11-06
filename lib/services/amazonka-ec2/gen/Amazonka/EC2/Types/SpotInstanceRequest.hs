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
-- Module      : Amazonka.EC2.Types.SpotInstanceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotInstanceRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.LaunchSpecification
import Amazonka.EC2.Types.RIProductDescription
import Amazonka.EC2.Types.SpotInstanceState
import Amazonka.EC2.Types.SpotInstanceStateFault
import Amazonka.EC2.Types.SpotInstanceStatus
import Amazonka.EC2.Types.SpotInstanceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a Spot Instance request.
--
-- /See:/ 'newSpotInstanceRequest' smart constructor.
data SpotInstanceRequest = SpotInstanceRequest'
  { -- | The instance ID, if an instance has been launched to fulfill the Spot
    -- Instance request.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The status code and status message describing the Spot Instance request.
    status :: Prelude.Maybe SpotInstanceStatus,
    -- | The state of the Spot Instance request. Spot status information helps
    -- track your Spot Instance requests. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    state :: Prelude.Maybe SpotInstanceState,
    -- | Deprecated.
    actualBlockHourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | Deprecated.
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The product description associated with the Spot Instance.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The maximum price per hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | Additional information for launching instances.
    launchSpecification :: Prelude.Maybe LaunchSpecification,
    -- | The Availability Zone group. If you specify the same Availability Zone
    -- group for all Spot Instance requests, all Spot Instances are launched in
    -- the same Availability Zone.
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone in which the request is launched.
    launchedAvailabilityZone :: Prelude.Maybe Prelude.Text,
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
    validUntil :: Prelude.Maybe Core.ISO8601,
    -- | The instance launch group. Launch groups are Spot Instances that launch
    -- together and terminate together.
    launchGroup :: Prelude.Maybe Prelude.Text,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Prelude.Maybe SpotInstanceStateFault,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Prelude.Maybe Prelude.Text,
    -- | The Spot Instance request type.
    type' :: Prelude.Maybe SpotInstanceType,
    -- | The start date of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
    -- date and time.
    validFrom :: Prelude.Maybe Core.ISO8601,
    -- | The date and time when the Spot Instance request was created, in UTC
    -- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    createTime :: Prelude.Maybe Core.ISO8601,
    -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotInstanceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'spotInstanceRequest_instanceId' - The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
--
-- 'status', 'spotInstanceRequest_status' - The status code and status message describing the Spot Instance request.
--
-- 'state', 'spotInstanceRequest_state' - The state of the Spot Instance request. Spot status information helps
-- track your Spot Instance requests. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'actualBlockHourlyPrice', 'spotInstanceRequest_actualBlockHourlyPrice' - Deprecated.
--
-- 'blockDurationMinutes', 'spotInstanceRequest_blockDurationMinutes' - Deprecated.
--
-- 'instanceInterruptionBehavior', 'spotInstanceRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- 'productDescription', 'spotInstanceRequest_productDescription' - The product description associated with the Spot Instance.
--
-- 'spotPrice', 'spotInstanceRequest_spotPrice' - The maximum price per hour that you are willing to pay for a Spot
-- Instance.
--
-- 'launchSpecification', 'spotInstanceRequest_launchSpecification' - Additional information for launching instances.
--
-- 'availabilityZoneGroup', 'spotInstanceRequest_availabilityZoneGroup' - The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
--
-- 'launchedAvailabilityZone', 'spotInstanceRequest_launchedAvailabilityZone' - The Availability Zone in which the request is launched.
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
-- 'launchGroup', 'spotInstanceRequest_launchGroup' - The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- 'fault', 'spotInstanceRequest_fault' - The fault codes for the Spot Instance request, if any.
--
-- 'spotInstanceRequestId', 'spotInstanceRequest_spotInstanceRequestId' - The ID of the Spot Instance request.
--
-- 'type'', 'spotInstanceRequest_type' - The Spot Instance request type.
--
-- 'validFrom', 'spotInstanceRequest_validFrom' - The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
-- date and time.
--
-- 'createTime', 'spotInstanceRequest_createTime' - The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'tags', 'spotInstanceRequest_tags' - Any tags assigned to the resource.
newSpotInstanceRequest ::
  SpotInstanceRequest
newSpotInstanceRequest =
  SpotInstanceRequest'
    { instanceId = Prelude.Nothing,
      status = Prelude.Nothing,
      state = Prelude.Nothing,
      actualBlockHourlyPrice = Prelude.Nothing,
      blockDurationMinutes = Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      launchSpecification = Prelude.Nothing,
      availabilityZoneGroup = Prelude.Nothing,
      launchedAvailabilityZone = Prelude.Nothing,
      validUntil = Prelude.Nothing,
      launchGroup = Prelude.Nothing,
      fault = Prelude.Nothing,
      spotInstanceRequestId = Prelude.Nothing,
      type' = Prelude.Nothing,
      validFrom = Prelude.Nothing,
      createTime = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
spotInstanceRequest_instanceId :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_instanceId = Lens.lens (\SpotInstanceRequest' {instanceId} -> instanceId) (\s@SpotInstanceRequest' {} a -> s {instanceId = a} :: SpotInstanceRequest)

-- | The status code and status message describing the Spot Instance request.
spotInstanceRequest_status :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceStatus)
spotInstanceRequest_status = Lens.lens (\SpotInstanceRequest' {status} -> status) (\s@SpotInstanceRequest' {} a -> s {status = a} :: SpotInstanceRequest)

-- | The state of the Spot Instance request. Spot status information helps
-- track your Spot Instance requests. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-bid-status.html Spot status>
-- in the /Amazon EC2 User Guide for Linux Instances/.
spotInstanceRequest_state :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceState)
spotInstanceRequest_state = Lens.lens (\SpotInstanceRequest' {state} -> state) (\s@SpotInstanceRequest' {} a -> s {state = a} :: SpotInstanceRequest)

-- | Deprecated.
spotInstanceRequest_actualBlockHourlyPrice :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_actualBlockHourlyPrice = Lens.lens (\SpotInstanceRequest' {actualBlockHourlyPrice} -> actualBlockHourlyPrice) (\s@SpotInstanceRequest' {} a -> s {actualBlockHourlyPrice = a} :: SpotInstanceRequest)

-- | Deprecated.
spotInstanceRequest_blockDurationMinutes :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Int)
spotInstanceRequest_blockDurationMinutes = Lens.lens (\SpotInstanceRequest' {blockDurationMinutes} -> blockDurationMinutes) (\s@SpotInstanceRequest' {} a -> s {blockDurationMinutes = a} :: SpotInstanceRequest)

-- | The behavior when a Spot Instance is interrupted.
spotInstanceRequest_instanceInterruptionBehavior :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe InstanceInterruptionBehavior)
spotInstanceRequest_instanceInterruptionBehavior = Lens.lens (\SpotInstanceRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotInstanceRequest' {} a -> s {instanceInterruptionBehavior = a} :: SpotInstanceRequest)

-- | The product description associated with the Spot Instance.
spotInstanceRequest_productDescription :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe RIProductDescription)
spotInstanceRequest_productDescription = Lens.lens (\SpotInstanceRequest' {productDescription} -> productDescription) (\s@SpotInstanceRequest' {} a -> s {productDescription = a} :: SpotInstanceRequest)

-- | The maximum price per hour that you are willing to pay for a Spot
-- Instance.
spotInstanceRequest_spotPrice :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_spotPrice = Lens.lens (\SpotInstanceRequest' {spotPrice} -> spotPrice) (\s@SpotInstanceRequest' {} a -> s {spotPrice = a} :: SpotInstanceRequest)

-- | Additional information for launching instances.
spotInstanceRequest_launchSpecification :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe LaunchSpecification)
spotInstanceRequest_launchSpecification = Lens.lens (\SpotInstanceRequest' {launchSpecification} -> launchSpecification) (\s@SpotInstanceRequest' {} a -> s {launchSpecification = a} :: SpotInstanceRequest)

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
spotInstanceRequest_availabilityZoneGroup :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_availabilityZoneGroup = Lens.lens (\SpotInstanceRequest' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@SpotInstanceRequest' {} a -> s {availabilityZoneGroup = a} :: SpotInstanceRequest)

-- | The Availability Zone in which the request is launched.
spotInstanceRequest_launchedAvailabilityZone :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_launchedAvailabilityZone = Lens.lens (\SpotInstanceRequest' {launchedAvailabilityZone} -> launchedAvailabilityZone) (\s@SpotInstanceRequest' {} a -> s {launchedAvailabilityZone = a} :: SpotInstanceRequest)

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
spotInstanceRequest_validUntil :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.UTCTime)
spotInstanceRequest_validUntil = Lens.lens (\SpotInstanceRequest' {validUntil} -> validUntil) (\s@SpotInstanceRequest' {} a -> s {validUntil = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Core._Time

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
spotInstanceRequest_launchGroup :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_launchGroup = Lens.lens (\SpotInstanceRequest' {launchGroup} -> launchGroup) (\s@SpotInstanceRequest' {} a -> s {launchGroup = a} :: SpotInstanceRequest)

-- | The fault codes for the Spot Instance request, if any.
spotInstanceRequest_fault :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceStateFault)
spotInstanceRequest_fault = Lens.lens (\SpotInstanceRequest' {fault} -> fault) (\s@SpotInstanceRequest' {} a -> s {fault = a} :: SpotInstanceRequest)

-- | The ID of the Spot Instance request.
spotInstanceRequest_spotInstanceRequestId :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_spotInstanceRequestId = Lens.lens (\SpotInstanceRequest' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@SpotInstanceRequest' {} a -> s {spotInstanceRequestId = a} :: SpotInstanceRequest)

-- | The Spot Instance request type.
spotInstanceRequest_type :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceType)
spotInstanceRequest_type = Lens.lens (\SpotInstanceRequest' {type'} -> type') (\s@SpotInstanceRequest' {} a -> s {type' = a} :: SpotInstanceRequest)

-- | The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
-- date and time.
spotInstanceRequest_validFrom :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.UTCTime)
spotInstanceRequest_validFrom = Lens.lens (\SpotInstanceRequest' {validFrom} -> validFrom) (\s@SpotInstanceRequest' {} a -> s {validFrom = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Core._Time

-- | The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotInstanceRequest_createTime :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.UTCTime)
spotInstanceRequest_createTime = Lens.lens (\SpotInstanceRequest' {createTime} -> createTime) (\s@SpotInstanceRequest' {} a -> s {createTime = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Core._Time

-- | Any tags assigned to the resource.
spotInstanceRequest_tags :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe [Tag])
spotInstanceRequest_tags = Lens.lens (\SpotInstanceRequest' {tags} -> tags) (\s@SpotInstanceRequest' {} a -> s {tags = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML SpotInstanceRequest where
  parseXML x =
    SpotInstanceRequest'
      Prelude.<$> (x Core..@? "instanceId")
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "actualBlockHourlyPrice")
      Prelude.<*> (x Core..@? "blockDurationMinutes")
      Prelude.<*> (x Core..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Core..@? "productDescription")
      Prelude.<*> (x Core..@? "spotPrice")
      Prelude.<*> (x Core..@? "launchSpecification")
      Prelude.<*> (x Core..@? "availabilityZoneGroup")
      Prelude.<*> (x Core..@? "launchedAvailabilityZone")
      Prelude.<*> (x Core..@? "validUntil")
      Prelude.<*> (x Core..@? "launchGroup")
      Prelude.<*> (x Core..@? "fault")
      Prelude.<*> (x Core..@? "spotInstanceRequestId")
      Prelude.<*> (x Core..@? "type")
      Prelude.<*> (x Core..@? "validFrom")
      Prelude.<*> (x Core..@? "createTime")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable SpotInstanceRequest

instance Prelude.NFData SpotInstanceRequest
