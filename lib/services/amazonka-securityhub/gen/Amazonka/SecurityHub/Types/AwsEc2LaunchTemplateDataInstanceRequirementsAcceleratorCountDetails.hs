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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails = AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails'
  { -- | The maximum number of accelerators. If this parameter isn\'t specified,
    -- there\'s no maximum limit. To exclude accelerator-enabled instance
    -- types, set @Max@ to @0@.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of accelerators. If this parameter isn\'t specified,
    -- there\'s no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_max' - The maximum number of accelerators. If this parameter isn\'t specified,
-- there\'s no maximum limit. To exclude accelerator-enabled instance
-- types, set @Max@ to @0@.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_min' - The minimum number of accelerators. If this parameter isn\'t specified,
-- there\'s no minimum limit.
newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum number of accelerators. If this parameter isn\'t specified,
-- there\'s no maximum limit. To exclude accelerator-enabled instance
-- types, set @Max@ to @0@.
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails)

-- | The minimum number of accelerators. If this parameter isn\'t specified,
-- there\'s no minimum limit.
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails'
            Prelude.<$> (x Data..:? "Max")
            Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {..} =
      _salt
        `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsAcceleratorCountDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
