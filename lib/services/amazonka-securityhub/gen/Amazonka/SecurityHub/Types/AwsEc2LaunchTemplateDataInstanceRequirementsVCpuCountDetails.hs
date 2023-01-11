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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of vCPUs for an Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails = AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails'
  { -- | The maximum number of vCPUs.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of vCPUs.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_max' - The maximum number of vCPUs.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_min' - The minimum number of vCPUs.
newAwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum number of vCPUs.
awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails)

-- | The minimum number of vCPUs.
awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsVCpuCountDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
