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
-- Module      : Amazonka.IoTWireless.Types.ImportedSidewalkDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.ImportedSidewalkDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.OnboardStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a Sidewalk device that has been added to an import
-- task.
--
-- /See:/ 'newImportedSidewalkDevice' smart constructor.
data ImportedSidewalkDevice = ImportedSidewalkDevice'
  { -- | The time at which the status information was last updated.
    lastUpdateTime :: Prelude.Maybe Data.ISO8601,
    -- | The onboarding status of the Sidewalk device in the import task.
    onboardingStatus :: Prelude.Maybe OnboardStatus,
    -- | The reason for the onboarding status information for the Sidewalk
    -- device.
    onboardingStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk manufacturing serial number (SMSN) of the Sidewalk device.
    sidewalkManufacturingSn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportedSidewalkDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTime', 'importedSidewalkDevice_lastUpdateTime' - The time at which the status information was last updated.
--
-- 'onboardingStatus', 'importedSidewalkDevice_onboardingStatus' - The onboarding status of the Sidewalk device in the import task.
--
-- 'onboardingStatusReason', 'importedSidewalkDevice_onboardingStatusReason' - The reason for the onboarding status information for the Sidewalk
-- device.
--
-- 'sidewalkManufacturingSn', 'importedSidewalkDevice_sidewalkManufacturingSn' - The Sidewalk manufacturing serial number (SMSN) of the Sidewalk device.
newImportedSidewalkDevice ::
  ImportedSidewalkDevice
newImportedSidewalkDevice =
  ImportedSidewalkDevice'
    { lastUpdateTime =
        Prelude.Nothing,
      onboardingStatus = Prelude.Nothing,
      onboardingStatusReason = Prelude.Nothing,
      sidewalkManufacturingSn = Prelude.Nothing
    }

-- | The time at which the status information was last updated.
importedSidewalkDevice_lastUpdateTime :: Lens.Lens' ImportedSidewalkDevice (Prelude.Maybe Prelude.UTCTime)
importedSidewalkDevice_lastUpdateTime = Lens.lens (\ImportedSidewalkDevice' {lastUpdateTime} -> lastUpdateTime) (\s@ImportedSidewalkDevice' {} a -> s {lastUpdateTime = a} :: ImportedSidewalkDevice) Prelude.. Lens.mapping Data._Time

-- | The onboarding status of the Sidewalk device in the import task.
importedSidewalkDevice_onboardingStatus :: Lens.Lens' ImportedSidewalkDevice (Prelude.Maybe OnboardStatus)
importedSidewalkDevice_onboardingStatus = Lens.lens (\ImportedSidewalkDevice' {onboardingStatus} -> onboardingStatus) (\s@ImportedSidewalkDevice' {} a -> s {onboardingStatus = a} :: ImportedSidewalkDevice)

-- | The reason for the onboarding status information for the Sidewalk
-- device.
importedSidewalkDevice_onboardingStatusReason :: Lens.Lens' ImportedSidewalkDevice (Prelude.Maybe Prelude.Text)
importedSidewalkDevice_onboardingStatusReason = Lens.lens (\ImportedSidewalkDevice' {onboardingStatusReason} -> onboardingStatusReason) (\s@ImportedSidewalkDevice' {} a -> s {onboardingStatusReason = a} :: ImportedSidewalkDevice)

-- | The Sidewalk manufacturing serial number (SMSN) of the Sidewalk device.
importedSidewalkDevice_sidewalkManufacturingSn :: Lens.Lens' ImportedSidewalkDevice (Prelude.Maybe Prelude.Text)
importedSidewalkDevice_sidewalkManufacturingSn = Lens.lens (\ImportedSidewalkDevice' {sidewalkManufacturingSn} -> sidewalkManufacturingSn) (\s@ImportedSidewalkDevice' {} a -> s {sidewalkManufacturingSn = a} :: ImportedSidewalkDevice)

instance Data.FromJSON ImportedSidewalkDevice where
  parseJSON =
    Data.withObject
      "ImportedSidewalkDevice"
      ( \x ->
          ImportedSidewalkDevice'
            Prelude.<$> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "OnboardingStatus")
            Prelude.<*> (x Data..:? "OnboardingStatusReason")
            Prelude.<*> (x Data..:? "SidewalkManufacturingSn")
      )

instance Prelude.Hashable ImportedSidewalkDevice where
  hashWithSalt _salt ImportedSidewalkDevice' {..} =
    _salt
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` onboardingStatus
      `Prelude.hashWithSalt` onboardingStatusReason
      `Prelude.hashWithSalt` sidewalkManufacturingSn

instance Prelude.NFData ImportedSidewalkDevice where
  rnf ImportedSidewalkDevice' {..} =
    Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf onboardingStatus
      `Prelude.seq` Prelude.rnf onboardingStatusReason
      `Prelude.seq` Prelude.rnf sidewalkManufacturingSn
