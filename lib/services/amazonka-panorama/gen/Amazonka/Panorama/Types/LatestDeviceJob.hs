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
-- Module      : Amazonka.Panorama.Types.LatestDeviceJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.LatestDeviceJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.JobType
import Amazonka.Panorama.Types.UpdateProgress
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the latest device job.
--
-- /See:/ 'newLatestDeviceJob' smart constructor.
data LatestDeviceJob = LatestDeviceJob'
  { -- | The target version of the device software.
    imageVersion :: Prelude.Maybe Prelude.Text,
    -- | The job\'s type.
    jobType :: Prelude.Maybe JobType,
    -- | Status of the latest device job.
    status :: Prelude.Maybe UpdateProgress
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LatestDeviceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageVersion', 'latestDeviceJob_imageVersion' - The target version of the device software.
--
-- 'jobType', 'latestDeviceJob_jobType' - The job\'s type.
--
-- 'status', 'latestDeviceJob_status' - Status of the latest device job.
newLatestDeviceJob ::
  LatestDeviceJob
newLatestDeviceJob =
  LatestDeviceJob'
    { imageVersion = Prelude.Nothing,
      jobType = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The target version of the device software.
latestDeviceJob_imageVersion :: Lens.Lens' LatestDeviceJob (Prelude.Maybe Prelude.Text)
latestDeviceJob_imageVersion = Lens.lens (\LatestDeviceJob' {imageVersion} -> imageVersion) (\s@LatestDeviceJob' {} a -> s {imageVersion = a} :: LatestDeviceJob)

-- | The job\'s type.
latestDeviceJob_jobType :: Lens.Lens' LatestDeviceJob (Prelude.Maybe JobType)
latestDeviceJob_jobType = Lens.lens (\LatestDeviceJob' {jobType} -> jobType) (\s@LatestDeviceJob' {} a -> s {jobType = a} :: LatestDeviceJob)

-- | Status of the latest device job.
latestDeviceJob_status :: Lens.Lens' LatestDeviceJob (Prelude.Maybe UpdateProgress)
latestDeviceJob_status = Lens.lens (\LatestDeviceJob' {status} -> status) (\s@LatestDeviceJob' {} a -> s {status = a} :: LatestDeviceJob)

instance Data.FromJSON LatestDeviceJob where
  parseJSON =
    Data.withObject
      "LatestDeviceJob"
      ( \x ->
          LatestDeviceJob'
            Prelude.<$> (x Data..:? "ImageVersion")
            Prelude.<*> (x Data..:? "JobType")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable LatestDeviceJob where
  hashWithSalt _salt LatestDeviceJob' {..} =
    _salt `Prelude.hashWithSalt` imageVersion
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` status

instance Prelude.NFData LatestDeviceJob where
  rnf LatestDeviceJob' {..} =
    Prelude.rnf imageVersion
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf status
