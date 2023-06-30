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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the target Capacity Reservation or Capacity
-- Reservation group in which to run an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' smart constructor.
data AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails = AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails'
  { -- | The ID of the Capacity Reservation in which to run the instance.
    capacityReservationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Capacity Reservation resource
    -- group in which to run the instance.
    capacityReservationResourceGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationId', 'awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationId' - The ID of the Capacity Reservation in which to run the instance.
--
-- 'capacityReservationResourceGroupArn', 'awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationResourceGroupArn' - The Amazon Resource Name (ARN) of the Capacity Reservation resource
-- group in which to run the instance.
newAwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails ::
  AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
newAwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails =
  AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails'
    { capacityReservationId =
        Prelude.Nothing,
      capacityReservationResourceGroupArn =
        Prelude.Nothing
    }

-- | The ID of the Capacity Reservation in which to run the instance.
awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationId :: Lens.Lens' AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationId = Lens.lens (\AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {capacityReservationId} -> capacityReservationId) (\s@AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {} a -> s {capacityReservationId = a} :: AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails)

-- | The Amazon Resource Name (ARN) of the Capacity Reservation resource
-- group in which to run the instance.
awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationResourceGroupArn :: Lens.Lens' AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails_capacityReservationResourceGroupArn = Lens.lens (\AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {capacityReservationResourceGroupArn} -> capacityReservationResourceGroupArn) (\s@AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {} a -> s {capacityReservationResourceGroupArn = a} :: AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails'
            Prelude.<$> (x Data..:? "CapacityReservationId")
            Prelude.<*> (x Data..:? "CapacityReservationResourceGroupArn")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {..} =
      _salt
        `Prelude.hashWithSalt` capacityReservationId
        `Prelude.hashWithSalt` capacityReservationResourceGroupArn

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
  where
  rnf
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {..} =
      Prelude.rnf capacityReservationId
        `Prelude.seq` Prelude.rnf capacityReservationResourceGroupArn

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataCapacityReservationSpecificationCapacityReservationTargetDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("CapacityReservationId" Data..=)
                Prelude.<$> capacityReservationId,
              ("CapacityReservationResourceGroupArn" Data..=)
                Prelude.<$> capacityReservationResourceGroupArn
            ]
        )
