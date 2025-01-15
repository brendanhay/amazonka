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
-- Module      : Amazonka.SecurityHub.Types.SoftwarePackage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.SoftwarePackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a software package.
--
-- /See:/ 'newSoftwarePackage' smart constructor.
data SoftwarePackage = SoftwarePackage'
  { -- | The architecture used for the software package.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The epoch of the software package.
    epoch :: Prelude.Maybe Prelude.Text,
    -- | The file system path to the package manager inventory file.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The version of the software package in which the vulnerability has been
    -- resolved.
    fixedInVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the software package.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source of the package.
    packageManager :: Prelude.Maybe Prelude.Text,
    -- | The release of the software package.
    release :: Prelude.Maybe Prelude.Text,
    -- | Describes the actions a customer can take to resolve the vulnerability
    -- in the software package.
    remediation :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source layer.
    sourceLayerArn :: Prelude.Maybe Prelude.Text,
    -- | The source layer hash of the vulnerable package.
    sourceLayerHash :: Prelude.Maybe Prelude.Text,
    -- | The version of the software package.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SoftwarePackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'softwarePackage_architecture' - The architecture used for the software package.
--
-- 'epoch', 'softwarePackage_epoch' - The epoch of the software package.
--
-- 'filePath', 'softwarePackage_filePath' - The file system path to the package manager inventory file.
--
-- 'fixedInVersion', 'softwarePackage_fixedInVersion' - The version of the software package in which the vulnerability has been
-- resolved.
--
-- 'name', 'softwarePackage_name' - The name of the software package.
--
-- 'packageManager', 'softwarePackage_packageManager' - The source of the package.
--
-- 'release', 'softwarePackage_release' - The release of the software package.
--
-- 'remediation', 'softwarePackage_remediation' - Describes the actions a customer can take to resolve the vulnerability
-- in the software package.
--
-- 'sourceLayerArn', 'softwarePackage_sourceLayerArn' - The Amazon Resource Name (ARN) of the source layer.
--
-- 'sourceLayerHash', 'softwarePackage_sourceLayerHash' - The source layer hash of the vulnerable package.
--
-- 'version', 'softwarePackage_version' - The version of the software package.
newSoftwarePackage ::
  SoftwarePackage
newSoftwarePackage =
  SoftwarePackage'
    { architecture = Prelude.Nothing,
      epoch = Prelude.Nothing,
      filePath = Prelude.Nothing,
      fixedInVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      packageManager = Prelude.Nothing,
      release = Prelude.Nothing,
      remediation = Prelude.Nothing,
      sourceLayerArn = Prelude.Nothing,
      sourceLayerHash = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The architecture used for the software package.
softwarePackage_architecture :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_architecture = Lens.lens (\SoftwarePackage' {architecture} -> architecture) (\s@SoftwarePackage' {} a -> s {architecture = a} :: SoftwarePackage)

-- | The epoch of the software package.
softwarePackage_epoch :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_epoch = Lens.lens (\SoftwarePackage' {epoch} -> epoch) (\s@SoftwarePackage' {} a -> s {epoch = a} :: SoftwarePackage)

-- | The file system path to the package manager inventory file.
softwarePackage_filePath :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_filePath = Lens.lens (\SoftwarePackage' {filePath} -> filePath) (\s@SoftwarePackage' {} a -> s {filePath = a} :: SoftwarePackage)

-- | The version of the software package in which the vulnerability has been
-- resolved.
softwarePackage_fixedInVersion :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_fixedInVersion = Lens.lens (\SoftwarePackage' {fixedInVersion} -> fixedInVersion) (\s@SoftwarePackage' {} a -> s {fixedInVersion = a} :: SoftwarePackage)

-- | The name of the software package.
softwarePackage_name :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_name = Lens.lens (\SoftwarePackage' {name} -> name) (\s@SoftwarePackage' {} a -> s {name = a} :: SoftwarePackage)

-- | The source of the package.
softwarePackage_packageManager :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_packageManager = Lens.lens (\SoftwarePackage' {packageManager} -> packageManager) (\s@SoftwarePackage' {} a -> s {packageManager = a} :: SoftwarePackage)

-- | The release of the software package.
softwarePackage_release :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_release = Lens.lens (\SoftwarePackage' {release} -> release) (\s@SoftwarePackage' {} a -> s {release = a} :: SoftwarePackage)

-- | Describes the actions a customer can take to resolve the vulnerability
-- in the software package.
softwarePackage_remediation :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_remediation = Lens.lens (\SoftwarePackage' {remediation} -> remediation) (\s@SoftwarePackage' {} a -> s {remediation = a} :: SoftwarePackage)

-- | The Amazon Resource Name (ARN) of the source layer.
softwarePackage_sourceLayerArn :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_sourceLayerArn = Lens.lens (\SoftwarePackage' {sourceLayerArn} -> sourceLayerArn) (\s@SoftwarePackage' {} a -> s {sourceLayerArn = a} :: SoftwarePackage)

-- | The source layer hash of the vulnerable package.
softwarePackage_sourceLayerHash :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_sourceLayerHash = Lens.lens (\SoftwarePackage' {sourceLayerHash} -> sourceLayerHash) (\s@SoftwarePackage' {} a -> s {sourceLayerHash = a} :: SoftwarePackage)

-- | The version of the software package.
softwarePackage_version :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_version = Lens.lens (\SoftwarePackage' {version} -> version) (\s@SoftwarePackage' {} a -> s {version = a} :: SoftwarePackage)

instance Data.FromJSON SoftwarePackage where
  parseJSON =
    Data.withObject
      "SoftwarePackage"
      ( \x ->
          SoftwarePackage'
            Prelude.<$> (x Data..:? "Architecture")
            Prelude.<*> (x Data..:? "Epoch")
            Prelude.<*> (x Data..:? "FilePath")
            Prelude.<*> (x Data..:? "FixedInVersion")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PackageManager")
            Prelude.<*> (x Data..:? "Release")
            Prelude.<*> (x Data..:? "Remediation")
            Prelude.<*> (x Data..:? "SourceLayerArn")
            Prelude.<*> (x Data..:? "SourceLayerHash")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable SoftwarePackage where
  hashWithSalt _salt SoftwarePackage' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` epoch
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` fixedInVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` packageManager
      `Prelude.hashWithSalt` release
      `Prelude.hashWithSalt` remediation
      `Prelude.hashWithSalt` sourceLayerArn
      `Prelude.hashWithSalt` sourceLayerHash
      `Prelude.hashWithSalt` version

instance Prelude.NFData SoftwarePackage where
  rnf SoftwarePackage' {..} =
    Prelude.rnf architecture `Prelude.seq`
      Prelude.rnf epoch `Prelude.seq`
        Prelude.rnf filePath `Prelude.seq`
          Prelude.rnf fixedInVersion `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf packageManager `Prelude.seq`
                Prelude.rnf release `Prelude.seq`
                  Prelude.rnf remediation `Prelude.seq`
                    Prelude.rnf sourceLayerArn `Prelude.seq`
                      Prelude.rnf sourceLayerHash `Prelude.seq`
                        Prelude.rnf version

instance Data.ToJSON SoftwarePackage where
  toJSON SoftwarePackage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Architecture" Data..=) Prelude.<$> architecture,
            ("Epoch" Data..=) Prelude.<$> epoch,
            ("FilePath" Data..=) Prelude.<$> filePath,
            ("FixedInVersion" Data..=)
              Prelude.<$> fixedInVersion,
            ("Name" Data..=) Prelude.<$> name,
            ("PackageManager" Data..=)
              Prelude.<$> packageManager,
            ("Release" Data..=) Prelude.<$> release,
            ("Remediation" Data..=) Prelude.<$> remediation,
            ("SourceLayerArn" Data..=)
              Prelude.<$> sourceLayerArn,
            ("SourceLayerHash" Data..=)
              Prelude.<$> sourceLayerHash,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
