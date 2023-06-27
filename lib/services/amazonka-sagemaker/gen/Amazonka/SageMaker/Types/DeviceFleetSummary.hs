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
-- Module      : Amazonka.SageMaker.Types.DeviceFleetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeviceFleetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the device fleet.
--
-- /See:/ 'newDeviceFleetSummary' smart constructor.
data DeviceFleetSummary = DeviceFleetSummary'
  { -- | Timestamp of when the device fleet was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Timestamp of when the device fleet was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | Amazon Resource Name (ARN) of the device fleet.
    deviceFleetArn :: Prelude.Text,
    -- | Name of the device fleet.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceFleetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'deviceFleetSummary_creationTime' - Timestamp of when the device fleet was created.
--
-- 'lastModifiedTime', 'deviceFleetSummary_lastModifiedTime' - Timestamp of when the device fleet was last updated.
--
-- 'deviceFleetArn', 'deviceFleetSummary_deviceFleetArn' - Amazon Resource Name (ARN) of the device fleet.
--
-- 'deviceFleetName', 'deviceFleetSummary_deviceFleetName' - Name of the device fleet.
newDeviceFleetSummary ::
  -- | 'deviceFleetArn'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  DeviceFleetSummary
newDeviceFleetSummary
  pDeviceFleetArn_
  pDeviceFleetName_ =
    DeviceFleetSummary'
      { creationTime = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        deviceFleetArn = pDeviceFleetArn_,
        deviceFleetName = pDeviceFleetName_
      }

-- | Timestamp of when the device fleet was created.
deviceFleetSummary_creationTime :: Lens.Lens' DeviceFleetSummary (Prelude.Maybe Prelude.UTCTime)
deviceFleetSummary_creationTime = Lens.lens (\DeviceFleetSummary' {creationTime} -> creationTime) (\s@DeviceFleetSummary' {} a -> s {creationTime = a} :: DeviceFleetSummary) Prelude.. Lens.mapping Data._Time

-- | Timestamp of when the device fleet was last updated.
deviceFleetSummary_lastModifiedTime :: Lens.Lens' DeviceFleetSummary (Prelude.Maybe Prelude.UTCTime)
deviceFleetSummary_lastModifiedTime = Lens.lens (\DeviceFleetSummary' {lastModifiedTime} -> lastModifiedTime) (\s@DeviceFleetSummary' {} a -> s {lastModifiedTime = a} :: DeviceFleetSummary) Prelude.. Lens.mapping Data._Time

-- | Amazon Resource Name (ARN) of the device fleet.
deviceFleetSummary_deviceFleetArn :: Lens.Lens' DeviceFleetSummary Prelude.Text
deviceFleetSummary_deviceFleetArn = Lens.lens (\DeviceFleetSummary' {deviceFleetArn} -> deviceFleetArn) (\s@DeviceFleetSummary' {} a -> s {deviceFleetArn = a} :: DeviceFleetSummary)

-- | Name of the device fleet.
deviceFleetSummary_deviceFleetName :: Lens.Lens' DeviceFleetSummary Prelude.Text
deviceFleetSummary_deviceFleetName = Lens.lens (\DeviceFleetSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceFleetSummary' {} a -> s {deviceFleetName = a} :: DeviceFleetSummary)

instance Data.FromJSON DeviceFleetSummary where
  parseJSON =
    Data.withObject
      "DeviceFleetSummary"
      ( \x ->
          DeviceFleetSummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..: "DeviceFleetArn")
            Prelude.<*> (x Data..: "DeviceFleetName")
      )

instance Prelude.Hashable DeviceFleetSummary where
  hashWithSalt _salt DeviceFleetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` deviceFleetArn
      `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData DeviceFleetSummary where
  rnf DeviceFleetSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf deviceFleetArn
      `Prelude.seq` Prelude.rnf deviceFleetName
