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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory per vCPU, in GiB.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails = AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails'
  { -- | The maximum amount of memory per vCPU, in GiB. If this parameter is
    -- omitted, there\'s no maximum limit.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of memory per vCPU, in GiB. If this parameter is
    -- omitted, there\'s no maximum limit.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_max' - The maximum amount of memory per vCPU, in GiB. If this parameter is
-- omitted, there\'s no maximum limit.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_min' - The minimum amount of memory per vCPU, in GiB. If this parameter is
-- omitted, there\'s no maximum limit.
newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum amount of memory per vCPU, in GiB. If this parameter is
-- omitted, there\'s no maximum limit.
awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails (Prelude.Maybe Prelude.Double)
awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails)

-- | The minimum amount of memory per vCPU, in GiB. If this parameter is
-- omitted, there\'s no maximum limit.
awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails (Prelude.Maybe Prelude.Double)
awsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsMemoryGiBPerVCpuDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
