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
-- Module      : Amazonka.SnowDeviceManagement.Types.SoftwareInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.SoftwareInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the software on the device.
--
-- /See:/ 'newSoftwareInformation' smart constructor.
data SoftwareInformation = SoftwareInformation'
  { -- | The version of the software being installed on the device.
    installingVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the software currently installed on the device.
    installedVersion :: Prelude.Maybe Prelude.Text,
    -- | The state of the software that is installed or that is being installed
    -- on the device.
    installState :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SoftwareInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'installingVersion', 'softwareInformation_installingVersion' - The version of the software being installed on the device.
--
-- 'installedVersion', 'softwareInformation_installedVersion' - The version of the software currently installed on the device.
--
-- 'installState', 'softwareInformation_installState' - The state of the software that is installed or that is being installed
-- on the device.
newSoftwareInformation ::
  SoftwareInformation
newSoftwareInformation =
  SoftwareInformation'
    { installingVersion =
        Prelude.Nothing,
      installedVersion = Prelude.Nothing,
      installState = Prelude.Nothing
    }

-- | The version of the software being installed on the device.
softwareInformation_installingVersion :: Lens.Lens' SoftwareInformation (Prelude.Maybe Prelude.Text)
softwareInformation_installingVersion = Lens.lens (\SoftwareInformation' {installingVersion} -> installingVersion) (\s@SoftwareInformation' {} a -> s {installingVersion = a} :: SoftwareInformation)

-- | The version of the software currently installed on the device.
softwareInformation_installedVersion :: Lens.Lens' SoftwareInformation (Prelude.Maybe Prelude.Text)
softwareInformation_installedVersion = Lens.lens (\SoftwareInformation' {installedVersion} -> installedVersion) (\s@SoftwareInformation' {} a -> s {installedVersion = a} :: SoftwareInformation)

-- | The state of the software that is installed or that is being installed
-- on the device.
softwareInformation_installState :: Lens.Lens' SoftwareInformation (Prelude.Maybe Prelude.Text)
softwareInformation_installState = Lens.lens (\SoftwareInformation' {installState} -> installState) (\s@SoftwareInformation' {} a -> s {installState = a} :: SoftwareInformation)

instance Data.FromJSON SoftwareInformation where
  parseJSON =
    Data.withObject
      "SoftwareInformation"
      ( \x ->
          SoftwareInformation'
            Prelude.<$> (x Data..:? "installingVersion")
            Prelude.<*> (x Data..:? "installedVersion")
            Prelude.<*> (x Data..:? "installState")
      )

instance Prelude.Hashable SoftwareInformation where
  hashWithSalt _salt SoftwareInformation' {..} =
    _salt `Prelude.hashWithSalt` installingVersion
      `Prelude.hashWithSalt` installedVersion
      `Prelude.hashWithSalt` installState

instance Prelude.NFData SoftwareInformation where
  rnf SoftwareInformation' {..} =
    Prelude.rnf installingVersion
      `Prelude.seq` Prelude.rnf installedVersion
      `Prelude.seq` Prelude.rnf installState
