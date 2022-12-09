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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotInstanceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.LaunchSpecification
import Amazonka.EC2.Types.RIProductDescription
import Amazonka.EC2.Types.SpotInstanceState
import Amazonka.EC2.Types.SpotInstanceStateFault
import Amazonka.EC2.Types.SpotInstanceStatus
import Amazonka.EC2.Types.SpotInstanceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a Spot Instance request.
--
-- /See:/ 'newSpotInstanceRequest' smart constructor.
data SpotInstanceRequest = SpotInstanceRequest'
  { -- | Deprecated.
    actualBlockHourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone group. If you specify the same Availability Zone
    -- group for all Spot Instance requests, all Spot Instances are launched in
    -- the same Availability Zone.
    availabilityZoneGroup :: Prelude.Maybe Prelude.Text,
    -- | Deprecated.
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The date and time when the Spot Instance request was created, in UTC
    -- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    createTime :: Prelude.Maybe Data.ISO8601,
    -- | The fault codes for the Spot Instance request, if any.
    fault :: Prelude.Maybe SpotInstanceStateFault,
    -- | The instance ID, if an instance has been launched to fulfill the Spot
    -- Instance request.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The instance launch group. Launch groups are Spot Instances that launch
    -- together and terminate together.
    launchGroup :: Prelude.Maybe Prelude.Text,
    -- | Additional information for launching instances.
    launchSpecification :: Prelude.Maybe LaunchSpecification,
    -- | The Availability Zone in which the request is launched.
    launchedAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The product description associated with the Spot Instance.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Prelude.Maybe Prelude.Text,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The state of the Spot Instance request. Spot request status information
    -- helps track your Spot Instance requests. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-request-status.html Spot request status>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    state :: Prelude.Maybe SpotInstanceState,
    -- | The status code and status message describing the Spot Instance request.
    status :: Prelude.Maybe SpotInstanceStatus,
    -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The Spot Instance request type.
    type' :: Prelude.Maybe SpotInstanceType,
    -- | The start date of the request, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
    -- date and time.
    validFrom :: Prelude.Maybe Data.ISO8601,
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
    validUntil :: Prelude.Maybe Data.ISO8601
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
-- 'actualBlockHourlyPrice', 'spotInstanceRequest_actualBlockHourlyPrice' - Deprecated.
--
-- 'availabilityZoneGroup', 'spotInstanceRequest_availabilityZoneGroup' - The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
--
-- 'blockDurationMinutes', 'spotInstanceRequest_blockDurationMinutes' - Deprecated.
--
-- 'createTime', 'spotInstanceRequest_createTime' - The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'fault', 'spotInstanceRequest_fault' - The fault codes for the Spot Instance request, if any.
--
-- 'instanceId', 'spotInstanceRequest_instanceId' - The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
--
-- 'instanceInterruptionBehavior', 'spotInstanceRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- 'launchGroup', 'spotInstanceRequest_launchGroup' - The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
--
-- 'launchSpecification', 'spotInstanceRequest_launchSpecification' - Additional information for launching instances.
--
-- 'launchedAvailabilityZone', 'spotInstanceRequest_launchedAvailabilityZone' - The Availability Zone in which the request is launched.
--
-- 'productDescription', 'spotInstanceRequest_productDescription' - The product description associated with the Spot Instance.
--
-- 'spotInstanceRequestId', 'spotInstanceRequest_spotInstanceRequestId' - The ID of the Spot Instance request.
--
-- 'spotPrice', 'spotInstanceRequest_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- 'state', 'spotInstanceRequest_state' - The state of the Spot Instance request. Spot request status information
-- helps track your Spot Instance requests. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-request-status.html Spot request status>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- 'status', 'spotInstanceRequest_status' - The status code and status message describing the Spot Instance request.
--
-- 'tags', 'spotInstanceRequest_tags' - Any tags assigned to the resource.
--
-- 'type'', 'spotInstanceRequest_type' - The Spot Instance request type.
--
-- 'validFrom', 'spotInstanceRequest_validFrom' - The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
-- date and time.
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
newSpotInstanceRequest ::
  SpotInstanceRequest
newSpotInstanceRequest =
  SpotInstanceRequest'
    { actualBlockHourlyPrice =
        Prelude.Nothing,
      availabilityZoneGroup = Prelude.Nothing,
      blockDurationMinutes = Prelude.Nothing,
      createTime = Prelude.Nothing,
      fault = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      launchGroup = Prelude.Nothing,
      launchSpecification = Prelude.Nothing,
      launchedAvailabilityZone = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      spotInstanceRequestId = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      state = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      validFrom = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | Deprecated.
spotInstanceRequest_actualBlockHourlyPrice :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_actualBlockHourlyPrice = Lens.lens (\SpotInstanceRequest' {actualBlockHourlyPrice} -> actualBlockHourlyPrice) (\s@SpotInstanceRequest' {} a -> s {actualBlockHourlyPrice = a} :: SpotInstanceRequest)

-- | The Availability Zone group. If you specify the same Availability Zone
-- group for all Spot Instance requests, all Spot Instances are launched in
-- the same Availability Zone.
spotInstanceRequest_availabilityZoneGroup :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_availabilityZoneGroup = Lens.lens (\SpotInstanceRequest' {availabilityZoneGroup} -> availabilityZoneGroup) (\s@SpotInstanceRequest' {} a -> s {availabilityZoneGroup = a} :: SpotInstanceRequest)

-- | Deprecated.
spotInstanceRequest_blockDurationMinutes :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Int)
spotInstanceRequest_blockDurationMinutes = Lens.lens (\SpotInstanceRequest' {blockDurationMinutes} -> blockDurationMinutes) (\s@SpotInstanceRequest' {} a -> s {blockDurationMinutes = a} :: SpotInstanceRequest)

-- | The date and time when the Spot Instance request was created, in UTC
-- format (for example, /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotInstanceRequest_createTime :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.UTCTime)
spotInstanceRequest_createTime = Lens.lens (\SpotInstanceRequest' {createTime} -> createTime) (\s@SpotInstanceRequest' {} a -> s {createTime = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Data._Time

-- | The fault codes for the Spot Instance request, if any.
spotInstanceRequest_fault :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceStateFault)
spotInstanceRequest_fault = Lens.lens (\SpotInstanceRequest' {fault} -> fault) (\s@SpotInstanceRequest' {} a -> s {fault = a} :: SpotInstanceRequest)

-- | The instance ID, if an instance has been launched to fulfill the Spot
-- Instance request.
spotInstanceRequest_instanceId :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_instanceId = Lens.lens (\SpotInstanceRequest' {instanceId} -> instanceId) (\s@SpotInstanceRequest' {} a -> s {instanceId = a} :: SpotInstanceRequest)

-- | The behavior when a Spot Instance is interrupted.
spotInstanceRequest_instanceInterruptionBehavior :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe InstanceInterruptionBehavior)
spotInstanceRequest_instanceInterruptionBehavior = Lens.lens (\SpotInstanceRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotInstanceRequest' {} a -> s {instanceInterruptionBehavior = a} :: SpotInstanceRequest)

-- | The instance launch group. Launch groups are Spot Instances that launch
-- together and terminate together.
spotInstanceRequest_launchGroup :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_launchGroup = Lens.lens (\SpotInstanceRequest' {launchGroup} -> launchGroup) (\s@SpotInstanceRequest' {} a -> s {launchGroup = a} :: SpotInstanceRequest)

-- | Additional information for launching instances.
spotInstanceRequest_launchSpecification :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe LaunchSpecification)
spotInstanceRequest_launchSpecification = Lens.lens (\SpotInstanceRequest' {launchSpecification} -> launchSpecification) (\s@SpotInstanceRequest' {} a -> s {launchSpecification = a} :: SpotInstanceRequest)

-- | The Availability Zone in which the request is launched.
spotInstanceRequest_launchedAvailabilityZone :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_launchedAvailabilityZone = Lens.lens (\SpotInstanceRequest' {launchedAvailabilityZone} -> launchedAvailabilityZone) (\s@SpotInstanceRequest' {} a -> s {launchedAvailabilityZone = a} :: SpotInstanceRequest)

-- | The product description associated with the Spot Instance.
spotInstanceRequest_productDescription :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe RIProductDescription)
spotInstanceRequest_productDescription = Lens.lens (\SpotInstanceRequest' {productDescription} -> productDescription) (\s@SpotInstanceRequest' {} a -> s {productDescription = a} :: SpotInstanceRequest)

-- | The ID of the Spot Instance request.
spotInstanceRequest_spotInstanceRequestId :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_spotInstanceRequestId = Lens.lens (\SpotInstanceRequest' {spotInstanceRequestId} -> spotInstanceRequestId) (\s@SpotInstanceRequest' {} a -> s {spotInstanceRequestId = a} :: SpotInstanceRequest)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
spotInstanceRequest_spotPrice :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.Text)
spotInstanceRequest_spotPrice = Lens.lens (\SpotInstanceRequest' {spotPrice} -> spotPrice) (\s@SpotInstanceRequest' {} a -> s {spotPrice = a} :: SpotInstanceRequest)

-- | The state of the Spot Instance request. Spot request status information
-- helps track your Spot Instance requests. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-request-status.html Spot request status>
-- in the /Amazon EC2 User Guide for Linux Instances/.
spotInstanceRequest_state :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceState)
spotInstanceRequest_state = Lens.lens (\SpotInstanceRequest' {state} -> state) (\s@SpotInstanceRequest' {} a -> s {state = a} :: SpotInstanceRequest)

-- | The status code and status message describing the Spot Instance request.
spotInstanceRequest_status :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceStatus)
spotInstanceRequest_status = Lens.lens (\SpotInstanceRequest' {status} -> status) (\s@SpotInstanceRequest' {} a -> s {status = a} :: SpotInstanceRequest)

-- | Any tags assigned to the resource.
spotInstanceRequest_tags :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe [Tag])
spotInstanceRequest_tags = Lens.lens (\SpotInstanceRequest' {tags} -> tags) (\s@SpotInstanceRequest' {} a -> s {tags = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Lens.coerced

-- | The Spot Instance request type.
spotInstanceRequest_type :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe SpotInstanceType)
spotInstanceRequest_type = Lens.lens (\SpotInstanceRequest' {type'} -> type') (\s@SpotInstanceRequest' {} a -> s {type' = a} :: SpotInstanceRequest)

-- | The start date of the request, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). The request becomes active at this
-- date and time.
spotInstanceRequest_validFrom :: Lens.Lens' SpotInstanceRequest (Prelude.Maybe Prelude.UTCTime)
spotInstanceRequest_validFrom = Lens.lens (\SpotInstanceRequest' {validFrom} -> validFrom) (\s@SpotInstanceRequest' {} a -> s {validFrom = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Data._Time

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
spotInstanceRequest_validUntil = Lens.lens (\SpotInstanceRequest' {validUntil} -> validUntil) (\s@SpotInstanceRequest' {} a -> s {validUntil = a} :: SpotInstanceRequest) Prelude.. Lens.mapping Data._Time

instance Data.FromXML SpotInstanceRequest where
  parseXML x =
    SpotInstanceRequest'
      Prelude.<$> (x Data..@? "actualBlockHourlyPrice")
      Prelude.<*> (x Data..@? "availabilityZoneGroup")
      Prelude.<*> (x Data..@? "blockDurationMinutes")
      Prelude.<*> (x Data..@? "createTime")
      Prelude.<*> (x Data..@? "fault")
      Prelude.<*> (x Data..@? "instanceId")
      Prelude.<*> (x Data..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Data..@? "launchGroup")
      Prelude.<*> (x Data..@? "launchSpecification")
      Prelude.<*> (x Data..@? "launchedAvailabilityZone")
      Prelude.<*> (x Data..@? "productDescription")
      Prelude.<*> (x Data..@? "spotInstanceRequestId")
      Prelude.<*> (x Data..@? "spotPrice")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "type")
      Prelude.<*> (x Data..@? "validFrom")
      Prelude.<*> (x Data..@? "validUntil")

instance Prelude.Hashable SpotInstanceRequest where
  hashWithSalt _salt SpotInstanceRequest' {..} =
    _salt `Prelude.hashWithSalt` actualBlockHourlyPrice
      `Prelude.hashWithSalt` availabilityZoneGroup
      `Prelude.hashWithSalt` blockDurationMinutes
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` fault
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` instanceInterruptionBehavior
      `Prelude.hashWithSalt` launchGroup
      `Prelude.hashWithSalt` launchSpecification
      `Prelude.hashWithSalt` launchedAvailabilityZone
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` spotInstanceRequestId
      `Prelude.hashWithSalt` spotPrice
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` validFrom
      `Prelude.hashWithSalt` validUntil

instance Prelude.NFData SpotInstanceRequest where
  rnf SpotInstanceRequest' {..} =
    Prelude.rnf actualBlockHourlyPrice
      `Prelude.seq` Prelude.rnf availabilityZoneGroup
      `Prelude.seq` Prelude.rnf blockDurationMinutes
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf fault
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf launchGroup
      `Prelude.seq` Prelude.rnf launchSpecification
      `Prelude.seq` Prelude.rnf launchedAvailabilityZone
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf spotInstanceRequestId
      `Prelude.seq` Prelude.rnf spotPrice
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf validFrom
      `Prelude.seq` Prelude.rnf validUntil
