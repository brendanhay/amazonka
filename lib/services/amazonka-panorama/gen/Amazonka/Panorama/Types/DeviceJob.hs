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
-- Module      : Amazonka.Panorama.Types.DeviceJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.DeviceJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A job that runs on a device.
--
-- /See:/ 'newDeviceJob' smart constructor.
data DeviceJob = DeviceJob'
  { -- | The job\'s ID.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | When the job was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the target device
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target device.
    deviceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'deviceJob_jobId' - The job\'s ID.
--
-- 'createdTime', 'deviceJob_createdTime' - When the job was created.
--
-- 'deviceName', 'deviceJob_deviceName' - The name of the target device
--
-- 'deviceId', 'deviceJob_deviceId' - The ID of the target device.
newDeviceJob ::
  DeviceJob
newDeviceJob =
  DeviceJob'
    { jobId = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      deviceId = Prelude.Nothing
    }

-- | The job\'s ID.
deviceJob_jobId :: Lens.Lens' DeviceJob (Prelude.Maybe Prelude.Text)
deviceJob_jobId = Lens.lens (\DeviceJob' {jobId} -> jobId) (\s@DeviceJob' {} a -> s {jobId = a} :: DeviceJob)

-- | When the job was created.
deviceJob_createdTime :: Lens.Lens' DeviceJob (Prelude.Maybe Prelude.UTCTime)
deviceJob_createdTime = Lens.lens (\DeviceJob' {createdTime} -> createdTime) (\s@DeviceJob' {} a -> s {createdTime = a} :: DeviceJob) Prelude.. Lens.mapping Core._Time

-- | The name of the target device
deviceJob_deviceName :: Lens.Lens' DeviceJob (Prelude.Maybe Prelude.Text)
deviceJob_deviceName = Lens.lens (\DeviceJob' {deviceName} -> deviceName) (\s@DeviceJob' {} a -> s {deviceName = a} :: DeviceJob)

-- | The ID of the target device.
deviceJob_deviceId :: Lens.Lens' DeviceJob (Prelude.Maybe Prelude.Text)
deviceJob_deviceId = Lens.lens (\DeviceJob' {deviceId} -> deviceId) (\s@DeviceJob' {} a -> s {deviceId = a} :: DeviceJob)

instance Core.FromJSON DeviceJob where
  parseJSON =
    Core.withObject
      "DeviceJob"
      ( \x ->
          DeviceJob'
            Prelude.<$> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "DeviceName")
            Prelude.<*> (x Core..:? "DeviceId")
      )

instance Prelude.Hashable DeviceJob where
  hashWithSalt salt' DeviceJob' {..} =
    salt' `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData DeviceJob where
  rnf DeviceJob' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf createdTime
