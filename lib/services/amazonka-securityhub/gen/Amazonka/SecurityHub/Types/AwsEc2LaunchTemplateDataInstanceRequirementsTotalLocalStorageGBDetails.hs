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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of total local storage, in GB, that an
-- Amazon EC2 instance uses.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails = AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails'
  { -- | The maximum amount of total local storage, in GB.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of total local storage, in GB.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_max' - The maximum amount of total local storage, in GB.
--
-- 'min', 'awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_min' - The minimum amount of total local storage, in GB.
newAwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails ::
  AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
newAwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails =
  AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails'
    { max =
        Prelude.Nothing,
      min =
        Prelude.Nothing
    }

-- | The maximum amount of total local storage, in GB.
awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_max :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails (Prelude.Maybe Prelude.Double)
awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_max = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {max} -> max) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {} a -> s {max = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails)

-- | The minimum amount of total local storage, in GB.
awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_min :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails (Prelude.Maybe Prelude.Double)
awsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails_min = Lens.lens (\AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {min} -> min) (\s@AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {} a -> s {min = a} :: AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails'
            Prelude.<$> (x Data..:? "Max") Prelude.<*> (x Data..:? "Min")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {..} =
      Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceRequirementsTotalLocalStorageGBDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Max" Data..=) Prelude.<$> max,
              ("Min" Data..=) Prelude.<$> min
            ]
        )
