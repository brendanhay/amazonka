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
-- Module      : Amazonka.Panorama.Types.Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A job for a device.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The target device\'s ID.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The job\'s ID.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceId', 'job_deviceId' - The target device\'s ID.
--
-- 'jobId', 'job_jobId' - The job\'s ID.
newJob ::
  Job
newJob =
  Job'
    { deviceId = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The target device\'s ID.
job_deviceId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_deviceId = Lens.lens (\Job' {deviceId} -> deviceId) (\s@Job' {} a -> s {deviceId = a} :: Job)

-- | The job\'s ID.
job_jobId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_jobId = Lens.lens (\Job' {jobId} -> jobId) (\s@Job' {} a -> s {jobId = a} :: Job)

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "DeviceId")
            Prelude.<*> (x Data..:? "JobId")
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf jobId
