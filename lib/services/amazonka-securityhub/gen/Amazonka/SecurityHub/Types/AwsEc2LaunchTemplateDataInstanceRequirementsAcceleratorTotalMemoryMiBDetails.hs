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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory, in MiB, for the accelerators
-- on an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails = AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails'
  { -- | The maximum amount of memory, in MiB. If this parameter isn\'t
    -- specified, there\'s no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of memory, in MiB. If @0@ is specified, there\'s no
    -- maximum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_max' - The maximum amount of memory, in MiB. If this parameter isn\'t
-- specified, there\'s no maximum limit.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_min' - The minimum amount of memory, in MiB. If @0@ is specified, there\'s no
-- maximum limit.
newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum amount of memory, in MiB. If this parameter isn\'t
-- specified, there\'s no maximum limit.
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails)

-- | The minimum amount of memory, in MiB. If @0@ is specified, there\'s no
-- maximum limit.
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorTotalMemoryMiBDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
