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
-- Module      : Network.AWS.SecurityHub.Types.SoftwarePackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.SoftwarePackage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a software package.
--
-- /See:/ 'newSoftwarePackage' smart constructor.
data SoftwarePackage = SoftwarePackage'
  { -- | The file system path to the package manager inventory file.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The release of the software package.
    release :: Prelude.Maybe Prelude.Text,
    -- | The name of the software package.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the software package.
    version :: Prelude.Maybe Prelude.Text,
    -- | The architecture used for the software package.
    architecture :: Prelude.Maybe Prelude.Text,
    -- | The source of the package.
    packageManager :: Prelude.Maybe Prelude.Text,
    -- | The epoch of the software package.
    epoch :: Prelude.Maybe Prelude.Text
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
-- 'filePath', 'softwarePackage_filePath' - The file system path to the package manager inventory file.
--
-- 'release', 'softwarePackage_release' - The release of the software package.
--
-- 'name', 'softwarePackage_name' - The name of the software package.
--
-- 'version', 'softwarePackage_version' - The version of the software package.
--
-- 'architecture', 'softwarePackage_architecture' - The architecture used for the software package.
--
-- 'packageManager', 'softwarePackage_packageManager' - The source of the package.
--
-- 'epoch', 'softwarePackage_epoch' - The epoch of the software package.
newSoftwarePackage ::
  SoftwarePackage
newSoftwarePackage =
  SoftwarePackage'
    { filePath = Prelude.Nothing,
      release = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      architecture = Prelude.Nothing,
      packageManager = Prelude.Nothing,
      epoch = Prelude.Nothing
    }

-- | The file system path to the package manager inventory file.
softwarePackage_filePath :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_filePath = Lens.lens (\SoftwarePackage' {filePath} -> filePath) (\s@SoftwarePackage' {} a -> s {filePath = a} :: SoftwarePackage)

-- | The release of the software package.
softwarePackage_release :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_release = Lens.lens (\SoftwarePackage' {release} -> release) (\s@SoftwarePackage' {} a -> s {release = a} :: SoftwarePackage)

-- | The name of the software package.
softwarePackage_name :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_name = Lens.lens (\SoftwarePackage' {name} -> name) (\s@SoftwarePackage' {} a -> s {name = a} :: SoftwarePackage)

-- | The version of the software package.
softwarePackage_version :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_version = Lens.lens (\SoftwarePackage' {version} -> version) (\s@SoftwarePackage' {} a -> s {version = a} :: SoftwarePackage)

-- | The architecture used for the software package.
softwarePackage_architecture :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_architecture = Lens.lens (\SoftwarePackage' {architecture} -> architecture) (\s@SoftwarePackage' {} a -> s {architecture = a} :: SoftwarePackage)

-- | The source of the package.
softwarePackage_packageManager :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_packageManager = Lens.lens (\SoftwarePackage' {packageManager} -> packageManager) (\s@SoftwarePackage' {} a -> s {packageManager = a} :: SoftwarePackage)

-- | The epoch of the software package.
softwarePackage_epoch :: Lens.Lens' SoftwarePackage (Prelude.Maybe Prelude.Text)
softwarePackage_epoch = Lens.lens (\SoftwarePackage' {epoch} -> epoch) (\s@SoftwarePackage' {} a -> s {epoch = a} :: SoftwarePackage)

instance Core.FromJSON SoftwarePackage where
  parseJSON =
    Core.withObject
      "SoftwarePackage"
      ( \x ->
          SoftwarePackage'
            Prelude.<$> (x Core..:? "FilePath")
            Prelude.<*> (x Core..:? "Release")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Architecture")
            Prelude.<*> (x Core..:? "PackageManager")
            Prelude.<*> (x Core..:? "Epoch")
      )

instance Prelude.Hashable SoftwarePackage

instance Prelude.NFData SoftwarePackage

instance Core.ToJSON SoftwarePackage where
  toJSON SoftwarePackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FilePath" Core..=) Prelude.<$> filePath,
            ("Release" Core..=) Prelude.<$> release,
            ("Name" Core..=) Prelude.<$> name,
            ("Version" Core..=) Prelude.<$> version,
            ("Architecture" Core..=) Prelude.<$> architecture,
            ("PackageManager" Core..=)
              Prelude.<$> packageManager,
            ("Epoch" Core..=) Prelude.<$> epoch
          ]
      )
