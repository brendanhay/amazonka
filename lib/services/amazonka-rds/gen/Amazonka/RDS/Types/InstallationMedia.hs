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
-- Module      : Amazonka.RDS.Types.InstallationMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.InstallationMedia where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.InstallationMediaFailureCause

-- | Contains the installation media for a DB engine that requires an
-- on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'newInstallationMedia' smart constructor.
data InstallationMedia = InstallationMedia'
  { -- | The engine version of the DB engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The status of the installation medium.
    status :: Prelude.Maybe Prelude.Text,
    -- | The installation medium ID.
    installationMediaId :: Prelude.Maybe Prelude.Text,
    -- | The path to the installation medium for the DB engine.
    engineInstallationMediaPath :: Prelude.Maybe Prelude.Text,
    -- | The DB engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The path to the installation medium for the operating system associated
    -- with the DB engine.
    oSInstallationMediaPath :: Prelude.Maybe Prelude.Text,
    -- | The custom Availability Zone (AZ) that contains the installation media.
    customAvailabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | If an installation media failure occurred, the cause of the failure.
    failureCause :: Prelude.Maybe InstallationMediaFailureCause
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstallationMedia' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'installationMedia_engineVersion' - The engine version of the DB engine.
--
-- 'status', 'installationMedia_status' - The status of the installation medium.
--
-- 'installationMediaId', 'installationMedia_installationMediaId' - The installation medium ID.
--
-- 'engineInstallationMediaPath', 'installationMedia_engineInstallationMediaPath' - The path to the installation medium for the DB engine.
--
-- 'engine', 'installationMedia_engine' - The DB engine.
--
-- 'oSInstallationMediaPath', 'installationMedia_oSInstallationMediaPath' - The path to the installation medium for the operating system associated
-- with the DB engine.
--
-- 'customAvailabilityZoneId', 'installationMedia_customAvailabilityZoneId' - The custom Availability Zone (AZ) that contains the installation media.
--
-- 'failureCause', 'installationMedia_failureCause' - If an installation media failure occurred, the cause of the failure.
newInstallationMedia ::
  InstallationMedia
newInstallationMedia =
  InstallationMedia'
    { engineVersion = Prelude.Nothing,
      status = Prelude.Nothing,
      installationMediaId = Prelude.Nothing,
      engineInstallationMediaPath = Prelude.Nothing,
      engine = Prelude.Nothing,
      oSInstallationMediaPath = Prelude.Nothing,
      customAvailabilityZoneId = Prelude.Nothing,
      failureCause = Prelude.Nothing
    }

-- | The engine version of the DB engine.
installationMedia_engineVersion :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_engineVersion = Lens.lens (\InstallationMedia' {engineVersion} -> engineVersion) (\s@InstallationMedia' {} a -> s {engineVersion = a} :: InstallationMedia)

-- | The status of the installation medium.
installationMedia_status :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_status = Lens.lens (\InstallationMedia' {status} -> status) (\s@InstallationMedia' {} a -> s {status = a} :: InstallationMedia)

-- | The installation medium ID.
installationMedia_installationMediaId :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_installationMediaId = Lens.lens (\InstallationMedia' {installationMediaId} -> installationMediaId) (\s@InstallationMedia' {} a -> s {installationMediaId = a} :: InstallationMedia)

-- | The path to the installation medium for the DB engine.
installationMedia_engineInstallationMediaPath :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_engineInstallationMediaPath = Lens.lens (\InstallationMedia' {engineInstallationMediaPath} -> engineInstallationMediaPath) (\s@InstallationMedia' {} a -> s {engineInstallationMediaPath = a} :: InstallationMedia)

-- | The DB engine.
installationMedia_engine :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_engine = Lens.lens (\InstallationMedia' {engine} -> engine) (\s@InstallationMedia' {} a -> s {engine = a} :: InstallationMedia)

-- | The path to the installation medium for the operating system associated
-- with the DB engine.
installationMedia_oSInstallationMediaPath :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_oSInstallationMediaPath = Lens.lens (\InstallationMedia' {oSInstallationMediaPath} -> oSInstallationMediaPath) (\s@InstallationMedia' {} a -> s {oSInstallationMediaPath = a} :: InstallationMedia)

-- | The custom Availability Zone (AZ) that contains the installation media.
installationMedia_customAvailabilityZoneId :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_customAvailabilityZoneId = Lens.lens (\InstallationMedia' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@InstallationMedia' {} a -> s {customAvailabilityZoneId = a} :: InstallationMedia)

-- | If an installation media failure occurred, the cause of the failure.
installationMedia_failureCause :: Lens.Lens' InstallationMedia (Prelude.Maybe InstallationMediaFailureCause)
installationMedia_failureCause = Lens.lens (\InstallationMedia' {failureCause} -> failureCause) (\s@InstallationMedia' {} a -> s {failureCause = a} :: InstallationMedia)

instance Core.FromXML InstallationMedia where
  parseXML x =
    InstallationMedia'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "InstallationMediaId")
      Prelude.<*> (x Core..@? "EngineInstallationMediaPath")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "OSInstallationMediaPath")
      Prelude.<*> (x Core..@? "CustomAvailabilityZoneId")
      Prelude.<*> (x Core..@? "FailureCause")

instance Prelude.Hashable InstallationMedia where
  hashWithSalt _salt InstallationMedia' {..} =
    _salt `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` installationMediaId
      `Prelude.hashWithSalt` engineInstallationMediaPath
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` oSInstallationMediaPath
      `Prelude.hashWithSalt` customAvailabilityZoneId
      `Prelude.hashWithSalt` failureCause

instance Prelude.NFData InstallationMedia where
  rnf InstallationMedia' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf installationMediaId
      `Prelude.seq` Prelude.rnf engineInstallationMediaPath
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf oSInstallationMediaPath
      `Prelude.seq` Prelude.rnf customAvailabilityZoneId
      `Prelude.seq` Prelude.rnf failureCause
