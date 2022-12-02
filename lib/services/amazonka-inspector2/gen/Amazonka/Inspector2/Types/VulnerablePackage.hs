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
-- Module      : Amazonka.Inspector2.Types.VulnerablePackage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.VulnerablePackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.PackageManager
import qualified Amazonka.Prelude as Prelude

-- | Information on the vulnerable package identified by a finding.
--
-- /See:/ 'newVulnerablePackage' smart constructor.
data VulnerablePackage = VulnerablePackage'
  { -- | The file path of the vulnerable package.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The version of the package that contains the vulnerability fix.
    fixedInVersion :: Prelude.Maybe Prelude.Text,
    -- | The code to run in your environment to update packages with a fix
    -- available.
    remediation :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the vulnerable package.
    arch :: Prelude.Maybe Prelude.Text,
    -- | The epoch of the vulnerable package.
    epoch :: Prelude.Maybe Prelude.Int,
    -- | The package manager of the vulnerable package.
    packageManager :: Prelude.Maybe PackageManager,
    -- | The release of the vulnerable package.
    release :: Prelude.Maybe Prelude.Text,
    -- | The source layer hash of the vulnerable package.
    sourceLayerHash :: Prelude.Maybe Prelude.Text,
    -- | The name of the vulnerable package.
    name :: Prelude.Text,
    -- | The version of the vulnerable package.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VulnerablePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filePath', 'vulnerablePackage_filePath' - The file path of the vulnerable package.
--
-- 'fixedInVersion', 'vulnerablePackage_fixedInVersion' - The version of the package that contains the vulnerability fix.
--
-- 'remediation', 'vulnerablePackage_remediation' - The code to run in your environment to update packages with a fix
-- available.
--
-- 'arch', 'vulnerablePackage_arch' - The architecture of the vulnerable package.
--
-- 'epoch', 'vulnerablePackage_epoch' - The epoch of the vulnerable package.
--
-- 'packageManager', 'vulnerablePackage_packageManager' - The package manager of the vulnerable package.
--
-- 'release', 'vulnerablePackage_release' - The release of the vulnerable package.
--
-- 'sourceLayerHash', 'vulnerablePackage_sourceLayerHash' - The source layer hash of the vulnerable package.
--
-- 'name', 'vulnerablePackage_name' - The name of the vulnerable package.
--
-- 'version', 'vulnerablePackage_version' - The version of the vulnerable package.
newVulnerablePackage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  VulnerablePackage
newVulnerablePackage pName_ pVersion_ =
  VulnerablePackage'
    { filePath = Prelude.Nothing,
      fixedInVersion = Prelude.Nothing,
      remediation = Prelude.Nothing,
      arch = Prelude.Nothing,
      epoch = Prelude.Nothing,
      packageManager = Prelude.Nothing,
      release = Prelude.Nothing,
      sourceLayerHash = Prelude.Nothing,
      name = pName_,
      version = pVersion_
    }

-- | The file path of the vulnerable package.
vulnerablePackage_filePath :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Text)
vulnerablePackage_filePath = Lens.lens (\VulnerablePackage' {filePath} -> filePath) (\s@VulnerablePackage' {} a -> s {filePath = a} :: VulnerablePackage)

-- | The version of the package that contains the vulnerability fix.
vulnerablePackage_fixedInVersion :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Text)
vulnerablePackage_fixedInVersion = Lens.lens (\VulnerablePackage' {fixedInVersion} -> fixedInVersion) (\s@VulnerablePackage' {} a -> s {fixedInVersion = a} :: VulnerablePackage)

-- | The code to run in your environment to update packages with a fix
-- available.
vulnerablePackage_remediation :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Text)
vulnerablePackage_remediation = Lens.lens (\VulnerablePackage' {remediation} -> remediation) (\s@VulnerablePackage' {} a -> s {remediation = a} :: VulnerablePackage)

-- | The architecture of the vulnerable package.
vulnerablePackage_arch :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Text)
vulnerablePackage_arch = Lens.lens (\VulnerablePackage' {arch} -> arch) (\s@VulnerablePackage' {} a -> s {arch = a} :: VulnerablePackage)

-- | The epoch of the vulnerable package.
vulnerablePackage_epoch :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Int)
vulnerablePackage_epoch = Lens.lens (\VulnerablePackage' {epoch} -> epoch) (\s@VulnerablePackage' {} a -> s {epoch = a} :: VulnerablePackage)

-- | The package manager of the vulnerable package.
vulnerablePackage_packageManager :: Lens.Lens' VulnerablePackage (Prelude.Maybe PackageManager)
vulnerablePackage_packageManager = Lens.lens (\VulnerablePackage' {packageManager} -> packageManager) (\s@VulnerablePackage' {} a -> s {packageManager = a} :: VulnerablePackage)

-- | The release of the vulnerable package.
vulnerablePackage_release :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Text)
vulnerablePackage_release = Lens.lens (\VulnerablePackage' {release} -> release) (\s@VulnerablePackage' {} a -> s {release = a} :: VulnerablePackage)

-- | The source layer hash of the vulnerable package.
vulnerablePackage_sourceLayerHash :: Lens.Lens' VulnerablePackage (Prelude.Maybe Prelude.Text)
vulnerablePackage_sourceLayerHash = Lens.lens (\VulnerablePackage' {sourceLayerHash} -> sourceLayerHash) (\s@VulnerablePackage' {} a -> s {sourceLayerHash = a} :: VulnerablePackage)

-- | The name of the vulnerable package.
vulnerablePackage_name :: Lens.Lens' VulnerablePackage Prelude.Text
vulnerablePackage_name = Lens.lens (\VulnerablePackage' {name} -> name) (\s@VulnerablePackage' {} a -> s {name = a} :: VulnerablePackage)

-- | The version of the vulnerable package.
vulnerablePackage_version :: Lens.Lens' VulnerablePackage Prelude.Text
vulnerablePackage_version = Lens.lens (\VulnerablePackage' {version} -> version) (\s@VulnerablePackage' {} a -> s {version = a} :: VulnerablePackage)

instance Data.FromJSON VulnerablePackage where
  parseJSON =
    Data.withObject
      "VulnerablePackage"
      ( \x ->
          VulnerablePackage'
            Prelude.<$> (x Data..:? "filePath")
            Prelude.<*> (x Data..:? "fixedInVersion")
            Prelude.<*> (x Data..:? "remediation")
            Prelude.<*> (x Data..:? "arch")
            Prelude.<*> (x Data..:? "epoch")
            Prelude.<*> (x Data..:? "packageManager")
            Prelude.<*> (x Data..:? "release")
            Prelude.<*> (x Data..:? "sourceLayerHash")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "version")
      )

instance Prelude.Hashable VulnerablePackage where
  hashWithSalt _salt VulnerablePackage' {..} =
    _salt `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` fixedInVersion
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` arch
      `Prelude.hashWithSalt` epoch
      `Prelude.hashWithSalt` packageManager
      `Prelude.hashWithSalt` release
      `Prelude.hashWithSalt` sourceLayerHash
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData VulnerablePackage where
  rnf VulnerablePackage' {..} =
    Prelude.rnf filePath
      `Prelude.seq` Prelude.rnf fixedInVersion
      `Prelude.seq` Prelude.rnf remediation
      `Prelude.seq` Prelude.rnf arch
      `Prelude.seq` Prelude.rnf epoch
      `Prelude.seq` Prelude.rnf packageManager
      `Prelude.seq` Prelude.rnf release
      `Prelude.seq` Prelude.rnf sourceLayerHash
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
