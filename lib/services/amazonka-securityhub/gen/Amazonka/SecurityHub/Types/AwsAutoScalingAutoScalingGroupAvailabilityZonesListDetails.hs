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
-- Module      : Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An Availability Zone for the automatic scaling group.
--
-- /See:/ 'newAwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' smart constructor.
data AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails = AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails'
  { -- | The name of the Availability Zone.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value' - The name of the Availability Zone.
newAwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails ::
  AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
newAwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails =
  AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails'
    { value =
        Prelude.Nothing
    }

-- | The name of the Availability Zone.
awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value :: Lens.Lens' AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails (Prelude.Maybe Prelude.Text)
awsAutoScalingAutoScalingGroupAvailabilityZonesListDetails_value = Lens.lens (\AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' {value} -> value) (\s@AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' {} a -> s {value = a} :: AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails)

instance
  Core.FromJSON
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
  where
  parseJSON =
    Core.withObject
      "AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails"
      ( \x ->
          AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails'
            Prelude.<$> (x Core..:? "Value")
      )

instance
  Prelude.Hashable
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
  where
  hashWithSalt
    _salt
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' {..} =
      _salt `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
  where
  rnf
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' {..} =
      Prelude.rnf value

instance
  Core.ToJSON
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails
  where
  toJSON
    AwsAutoScalingAutoScalingGroupAvailabilityZonesListDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [("Value" Core..=) Prelude.<$> value]
        )
