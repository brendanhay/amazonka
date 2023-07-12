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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCpuOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataCpuOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the CPU options for an Amazon EC2 instance. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-optimize-cpu.html Optimize CPU options>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataCpuOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataCpuOptionsDetails = AwsEc2LaunchTemplateDataCpuOptionsDetails'
  { -- | The number of CPU cores for the instance.
    coreCount :: Prelude.Maybe Prelude.Int,
    -- | The number of threads per CPU core. A value of @1@ disables
    -- multithreading for the instance, The default value is @2@.
    threadsPerCore :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataCpuOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreCount', 'awsEc2LaunchTemplateDataCpuOptionsDetails_coreCount' - The number of CPU cores for the instance.
--
-- 'threadsPerCore', 'awsEc2LaunchTemplateDataCpuOptionsDetails_threadsPerCore' - The number of threads per CPU core. A value of @1@ disables
-- multithreading for the instance, The default value is @2@.
newAwsEc2LaunchTemplateDataCpuOptionsDetails ::
  AwsEc2LaunchTemplateDataCpuOptionsDetails
newAwsEc2LaunchTemplateDataCpuOptionsDetails =
  AwsEc2LaunchTemplateDataCpuOptionsDetails'
    { coreCount =
        Prelude.Nothing,
      threadsPerCore = Prelude.Nothing
    }

-- | The number of CPU cores for the instance.
awsEc2LaunchTemplateDataCpuOptionsDetails_coreCount :: Lens.Lens' AwsEc2LaunchTemplateDataCpuOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataCpuOptionsDetails_coreCount = Lens.lens (\AwsEc2LaunchTemplateDataCpuOptionsDetails' {coreCount} -> coreCount) (\s@AwsEc2LaunchTemplateDataCpuOptionsDetails' {} a -> s {coreCount = a} :: AwsEc2LaunchTemplateDataCpuOptionsDetails)

-- | The number of threads per CPU core. A value of @1@ disables
-- multithreading for the instance, The default value is @2@.
awsEc2LaunchTemplateDataCpuOptionsDetails_threadsPerCore :: Lens.Lens' AwsEc2LaunchTemplateDataCpuOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataCpuOptionsDetails_threadsPerCore = Lens.lens (\AwsEc2LaunchTemplateDataCpuOptionsDetails' {threadsPerCore} -> threadsPerCore) (\s@AwsEc2LaunchTemplateDataCpuOptionsDetails' {} a -> s {threadsPerCore = a} :: AwsEc2LaunchTemplateDataCpuOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataCpuOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataCpuOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataCpuOptionsDetails'
            Prelude.<$> (x Data..:? "CoreCount")
            Prelude.<*> (x Data..:? "ThreadsPerCore")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataCpuOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataCpuOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` coreCount
        `Prelude.hashWithSalt` threadsPerCore

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataCpuOptionsDetails
  where
  rnf AwsEc2LaunchTemplateDataCpuOptionsDetails' {..} =
    Prelude.rnf coreCount
      `Prelude.seq` Prelude.rnf threadsPerCore

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataCpuOptionsDetails
  where
  toJSON AwsEc2LaunchTemplateDataCpuOptionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CoreCount" Data..=) Prelude.<$> coreCount,
            ("ThreadsPerCore" Data..=)
              Prelude.<$> threadsPerCore
          ]
      )
