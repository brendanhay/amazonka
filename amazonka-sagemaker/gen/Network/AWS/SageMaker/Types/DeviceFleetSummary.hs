{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.DeviceFleetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeviceFleetSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary of the device fleet.
--
-- /See:/ 'newDeviceFleetSummary' smart constructor.
data DeviceFleetSummary = DeviceFleetSummary'
  { -- | Timestamp of when the device fleet was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Timestamp of when the device fleet was last updated.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | Amazon Resource Name (ARN) of the device fleet.
    deviceFleetArn :: Prelude.Text,
    -- | Name of the device fleet.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
deviceFleetSummary_creationTime = Lens.lens (\DeviceFleetSummary' {creationTime} -> creationTime) (\s@DeviceFleetSummary' {} a -> s {creationTime = a} :: DeviceFleetSummary) Prelude.. Lens.mapping Prelude._Time

-- | Timestamp of when the device fleet was last updated.
deviceFleetSummary_lastModifiedTime :: Lens.Lens' DeviceFleetSummary (Prelude.Maybe Prelude.UTCTime)
deviceFleetSummary_lastModifiedTime = Lens.lens (\DeviceFleetSummary' {lastModifiedTime} -> lastModifiedTime) (\s@DeviceFleetSummary' {} a -> s {lastModifiedTime = a} :: DeviceFleetSummary) Prelude.. Lens.mapping Prelude._Time

-- | Amazon Resource Name (ARN) of the device fleet.
deviceFleetSummary_deviceFleetArn :: Lens.Lens' DeviceFleetSummary Prelude.Text
deviceFleetSummary_deviceFleetArn = Lens.lens (\DeviceFleetSummary' {deviceFleetArn} -> deviceFleetArn) (\s@DeviceFleetSummary' {} a -> s {deviceFleetArn = a} :: DeviceFleetSummary)

-- | Name of the device fleet.
deviceFleetSummary_deviceFleetName :: Lens.Lens' DeviceFleetSummary Prelude.Text
deviceFleetSummary_deviceFleetName = Lens.lens (\DeviceFleetSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceFleetSummary' {} a -> s {deviceFleetName = a} :: DeviceFleetSummary)

instance Prelude.FromJSON DeviceFleetSummary where
  parseJSON =
    Prelude.withObject
      "DeviceFleetSummary"
      ( \x ->
          DeviceFleetSummary'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..: "DeviceFleetArn")
            Prelude.<*> (x Prelude..: "DeviceFleetName")
      )

instance Prelude.Hashable DeviceFleetSummary

instance Prelude.NFData DeviceFleetSummary
