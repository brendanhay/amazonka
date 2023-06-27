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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory, in MiB, for an Amazon EC2
-- instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails = AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails'
  { -- | The maximum amount of memory, in MiB.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of memory, in MiB.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_max' - The maximum amount of memory, in MiB.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_min' - The minimum amount of memory, in MiB.
newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum amount of memory, in MiB.
awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails)

-- | The minimum amount of memory, in MiB.
awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails'
            Prelude.<$> (x Data..:? "Max")
            Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {..} =
      _salt
        `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryMiBDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
