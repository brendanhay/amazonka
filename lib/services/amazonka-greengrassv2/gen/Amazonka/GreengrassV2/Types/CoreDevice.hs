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
-- Module      : Amazonka.GreengrassV2.Types.CoreDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.CoreDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.CoreDeviceStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a Greengrass core device, which is an IoT
-- thing that runs the IoT Greengrass Core software.
--
-- /See:/ 'newCoreDevice' smart constructor.
data CoreDevice = CoreDevice'
  { -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Maybe Prelude.Text,
    -- | The status of the core device. Core devices can have the following
    -- statuses:
    --
    -- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
    --     on the core device without issue.
    --
    -- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
    --     a failed state on the core device.
    status :: Prelude.Maybe CoreDeviceStatus,
    -- | The time at which the core device\'s status last updated, expressed in
    -- ISO 8601 format.
    lastStatusUpdateTimestamp :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoreDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDeviceThingName', 'coreDevice_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
--
-- 'status', 'coreDevice_status' - The status of the core device. Core devices can have the following
-- statuses:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
--
-- 'lastStatusUpdateTimestamp', 'coreDevice_lastStatusUpdateTimestamp' - The time at which the core device\'s status last updated, expressed in
-- ISO 8601 format.
newCoreDevice ::
  CoreDevice
newCoreDevice =
  CoreDevice'
    { coreDeviceThingName = Prelude.Nothing,
      status = Prelude.Nothing,
      lastStatusUpdateTimestamp = Prelude.Nothing
    }

-- | The name of the core device. This is also the name of the IoT thing.
coreDevice_coreDeviceThingName :: Lens.Lens' CoreDevice (Prelude.Maybe Prelude.Text)
coreDevice_coreDeviceThingName = Lens.lens (\CoreDevice' {coreDeviceThingName} -> coreDeviceThingName) (\s@CoreDevice' {} a -> s {coreDeviceThingName = a} :: CoreDevice)

-- | The status of the core device. Core devices can have the following
-- statuses:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
coreDevice_status :: Lens.Lens' CoreDevice (Prelude.Maybe CoreDeviceStatus)
coreDevice_status = Lens.lens (\CoreDevice' {status} -> status) (\s@CoreDevice' {} a -> s {status = a} :: CoreDevice)

-- | The time at which the core device\'s status last updated, expressed in
-- ISO 8601 format.
coreDevice_lastStatusUpdateTimestamp :: Lens.Lens' CoreDevice (Prelude.Maybe Prelude.UTCTime)
coreDevice_lastStatusUpdateTimestamp = Lens.lens (\CoreDevice' {lastStatusUpdateTimestamp} -> lastStatusUpdateTimestamp) (\s@CoreDevice' {} a -> s {lastStatusUpdateTimestamp = a} :: CoreDevice) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CoreDevice where
  parseJSON =
    Data.withObject
      "CoreDevice"
      ( \x ->
          CoreDevice'
            Prelude.<$> (x Data..:? "coreDeviceThingName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "lastStatusUpdateTimestamp")
      )

instance Prelude.Hashable CoreDevice where
  hashWithSalt _salt CoreDevice' {..} =
    _salt `Prelude.hashWithSalt` coreDeviceThingName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastStatusUpdateTimestamp

instance Prelude.NFData CoreDevice where
  rnf CoreDevice' {..} =
    Prelude.rnf coreDeviceThingName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastStatusUpdateTimestamp
