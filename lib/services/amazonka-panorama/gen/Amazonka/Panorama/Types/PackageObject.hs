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
-- Module      : Amazonka.Panorama.Types.PackageObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A package object.
--
-- /See:/ 'newPackageObject' smart constructor.
data PackageObject = PackageObject'
  { -- | The object\'s name.
    name :: Prelude.Text,
    -- | The object\'s package version.
    packageVersion :: Prelude.Text,
    -- | The object\'s patch version.
    patchVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'packageObject_name' - The object\'s name.
--
-- 'packageVersion', 'packageObject_packageVersion' - The object\'s package version.
--
-- 'patchVersion', 'packageObject_patchVersion' - The object\'s patch version.
newPackageObject ::
  -- | 'name'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  -- | 'patchVersion'
  Prelude.Text ->
  PackageObject
newPackageObject
  pName_
  pPackageVersion_
  pPatchVersion_ =
    PackageObject'
      { name = pName_,
        packageVersion = pPackageVersion_,
        patchVersion = pPatchVersion_
      }

-- | The object\'s name.
packageObject_name :: Lens.Lens' PackageObject Prelude.Text
packageObject_name = Lens.lens (\PackageObject' {name} -> name) (\s@PackageObject' {} a -> s {name = a} :: PackageObject)

-- | The object\'s package version.
packageObject_packageVersion :: Lens.Lens' PackageObject Prelude.Text
packageObject_packageVersion = Lens.lens (\PackageObject' {packageVersion} -> packageVersion) (\s@PackageObject' {} a -> s {packageVersion = a} :: PackageObject)

-- | The object\'s patch version.
packageObject_patchVersion :: Lens.Lens' PackageObject Prelude.Text
packageObject_patchVersion = Lens.lens (\PackageObject' {patchVersion} -> patchVersion) (\s@PackageObject' {} a -> s {patchVersion = a} :: PackageObject)

instance Data.FromJSON PackageObject where
  parseJSON =
    Data.withObject
      "PackageObject"
      ( \x ->
          PackageObject'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "PackageVersion")
            Prelude.<*> (x Data..: "PatchVersion")
      )

instance Prelude.Hashable PackageObject where
  hashWithSalt _salt PackageObject' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` patchVersion

instance Prelude.NFData PackageObject where
  rnf PackageObject' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf patchVersion
