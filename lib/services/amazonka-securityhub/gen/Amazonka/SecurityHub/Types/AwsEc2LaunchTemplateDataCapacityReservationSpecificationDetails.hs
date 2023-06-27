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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails

-- | Specifies the Capacity Reservation targeting option of an Amazon EC2
-- instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' smart constructor.
data AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails = AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails'
  { -- | Indicates the instance\'s Capacity Reservation preferences. If equal to
    -- @open@, the instance can run in any open Capacity Reservation that has
    -- matching attributes (instance type, platform, Availability Zone). If
    -- equal to @none@, the instance avoids running in a Capacity Reservation
    -- even if one is available. The instance runs in On-Demand capacity.
    capacityReservationPreference :: Prelude.Maybe Prelude.Text,
    -- | Specifies a target Capacity Reservation.
    capacityReservationTarget :: Prelude.Maybe AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationPreference', 'awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationPreference' - Indicates the instance\'s Capacity Reservation preferences. If equal to
-- @open@, the instance can run in any open Capacity Reservation that has
-- matching attributes (instance type, platform, Availability Zone). If
-- equal to @none@, the instance avoids running in a Capacity Reservation
-- even if one is available. The instance runs in On-Demand capacity.
--
-- 'capacityReservationTarget', 'awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationTarget' - Specifies a target Capacity Reservation.
newAwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails ::
  AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
newAwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails =
  AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails'
    { capacityReservationPreference =
        Prelude.Nothing,
      capacityReservationTarget =
        Prelude.Nothing
    }

-- | Indicates the instance\'s Capacity Reservation preferences. If equal to
-- @open@, the instance can run in any open Capacity Reservation that has
-- matching attributes (instance type, platform, Availability Zone). If
-- equal to @none@, the instance avoids running in a Capacity Reservation
-- even if one is available. The instance runs in On-Demand capacity.
awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationPreference :: Lens.Lens' AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationPreference = Lens.lens (\AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {capacityReservationPreference} -> capacityReservationPreference) (\s@AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {} a -> s {capacityReservationPreference = a} :: AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails)

-- | Specifies a target Capacity Reservation.
awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationTarget :: Lens.Lens' AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails (Prelude.Maybe AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails)
awsEc2LaunchTemplateDataCapacityReservationSpecificationDetails_capacityReservationTarget = Lens.lens (\AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {capacityReservationTarget} -> capacityReservationTarget) (\s@AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {} a -> s {capacityReservationTarget = a} :: AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails'
            Prelude.<$> (x Data..:? "CapacityReservationPreference")
            Prelude.<*> (x Data..:? "CapacityReservationTarget")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationPreference
        `Prelude.hashWithSalt` capacityReservationTarget

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
  where
  rnf
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {..} =
      Prelude.rnf capacityReservationPreference
        `Prelude.seq` Prelude.rnf capacityReservationTarget

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CapacityReservationPreference" Data..=)
                Prelude.<$> capacityReservationPreference,
              ("CapacityReservationTarget" Data..=)
                Prelude.<$> capacityReservationTarget
            ]
        )
