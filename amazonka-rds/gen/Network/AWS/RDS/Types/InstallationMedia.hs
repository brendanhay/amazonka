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
-- Module      : Network.AWS.RDS.Types.InstallationMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.InstallationMedia where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.InstallationMediaFailureCause

-- | Contains the installation media for a DB engine that requires an
-- on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'newInstallationMedia' smart constructor.
data InstallationMedia = InstallationMedia'
  { -- | The status of the installation medium.
    status :: Prelude.Maybe Prelude.Text,
    -- | The custom Availability Zone (AZ) that contains the installation media.
    customAvailabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The installation medium ID.
    installationMediaId :: Prelude.Maybe Prelude.Text,
    -- | The engine version of the DB engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The path to the installation medium for the operating system associated
    -- with the DB engine.
    oSInstallationMediaPath :: Prelude.Maybe Prelude.Text,
    -- | If an installation media failure occurred, the cause of the failure.
    failureCause :: Prelude.Maybe InstallationMediaFailureCause,
    -- | The DB engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The path to the installation medium for the DB engine.
    engineInstallationMediaPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstallationMedia' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'installationMedia_status' - The status of the installation medium.
--
-- 'customAvailabilityZoneId', 'installationMedia_customAvailabilityZoneId' - The custom Availability Zone (AZ) that contains the installation media.
--
-- 'installationMediaId', 'installationMedia_installationMediaId' - The installation medium ID.
--
-- 'engineVersion', 'installationMedia_engineVersion' - The engine version of the DB engine.
--
-- 'oSInstallationMediaPath', 'installationMedia_oSInstallationMediaPath' - The path to the installation medium for the operating system associated
-- with the DB engine.
--
-- 'failureCause', 'installationMedia_failureCause' - If an installation media failure occurred, the cause of the failure.
--
-- 'engine', 'installationMedia_engine' - The DB engine.
--
-- 'engineInstallationMediaPath', 'installationMedia_engineInstallationMediaPath' - The path to the installation medium for the DB engine.
newInstallationMedia ::
  InstallationMedia
newInstallationMedia =
  InstallationMedia'
    { status = Prelude.Nothing,
      customAvailabilityZoneId = Prelude.Nothing,
      installationMediaId = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      oSInstallationMediaPath = Prelude.Nothing,
      failureCause = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineInstallationMediaPath = Prelude.Nothing
    }

-- | The status of the installation medium.
installationMedia_status :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_status = Lens.lens (\InstallationMedia' {status} -> status) (\s@InstallationMedia' {} a -> s {status = a} :: InstallationMedia)

-- | The custom Availability Zone (AZ) that contains the installation media.
installationMedia_customAvailabilityZoneId :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_customAvailabilityZoneId = Lens.lens (\InstallationMedia' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@InstallationMedia' {} a -> s {customAvailabilityZoneId = a} :: InstallationMedia)

-- | The installation medium ID.
installationMedia_installationMediaId :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_installationMediaId = Lens.lens (\InstallationMedia' {installationMediaId} -> installationMediaId) (\s@InstallationMedia' {} a -> s {installationMediaId = a} :: InstallationMedia)

-- | The engine version of the DB engine.
installationMedia_engineVersion :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_engineVersion = Lens.lens (\InstallationMedia' {engineVersion} -> engineVersion) (\s@InstallationMedia' {} a -> s {engineVersion = a} :: InstallationMedia)

-- | The path to the installation medium for the operating system associated
-- with the DB engine.
installationMedia_oSInstallationMediaPath :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_oSInstallationMediaPath = Lens.lens (\InstallationMedia' {oSInstallationMediaPath} -> oSInstallationMediaPath) (\s@InstallationMedia' {} a -> s {oSInstallationMediaPath = a} :: InstallationMedia)

-- | If an installation media failure occurred, the cause of the failure.
installationMedia_failureCause :: Lens.Lens' InstallationMedia (Prelude.Maybe InstallationMediaFailureCause)
installationMedia_failureCause = Lens.lens (\InstallationMedia' {failureCause} -> failureCause) (\s@InstallationMedia' {} a -> s {failureCause = a} :: InstallationMedia)

-- | The DB engine.
installationMedia_engine :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_engine = Lens.lens (\InstallationMedia' {engine} -> engine) (\s@InstallationMedia' {} a -> s {engine = a} :: InstallationMedia)

-- | The path to the installation medium for the DB engine.
installationMedia_engineInstallationMediaPath :: Lens.Lens' InstallationMedia (Prelude.Maybe Prelude.Text)
installationMedia_engineInstallationMediaPath = Lens.lens (\InstallationMedia' {engineInstallationMediaPath} -> engineInstallationMediaPath) (\s@InstallationMedia' {} a -> s {engineInstallationMediaPath = a} :: InstallationMedia)

instance Prelude.FromXML InstallationMedia where
  parseXML x =
    InstallationMedia'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "CustomAvailabilityZoneId")
      Prelude.<*> (x Prelude..@? "InstallationMediaId")
      Prelude.<*> (x Prelude..@? "EngineVersion")
      Prelude.<*> (x Prelude..@? "OSInstallationMediaPath")
      Prelude.<*> (x Prelude..@? "FailureCause")
      Prelude.<*> (x Prelude..@? "Engine")
      Prelude.<*> (x Prelude..@? "EngineInstallationMediaPath")

instance Prelude.Hashable InstallationMedia

instance Prelude.NFData InstallationMedia
