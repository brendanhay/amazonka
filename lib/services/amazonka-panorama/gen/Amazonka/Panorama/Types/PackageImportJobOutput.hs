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
-- Module      : Amazonka.Panorama.Types.PackageImportJobOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageImportJobOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.OutPutS3Location
import qualified Amazonka.Prelude as Prelude

-- | Results of a package import job.
--
-- /See:/ 'newPackageImportJobOutput' smart constructor.
data PackageImportJobOutput = PackageImportJobOutput'
  { -- | The package\'s output location.
    outputS3Location :: OutPutS3Location,
    -- | The package\'s ID.
    packageId :: Prelude.Text,
    -- | The package\'s version.
    packageVersion :: Prelude.Text,
    -- | The package\'s patch version.
    patchVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageImportJobOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputS3Location', 'packageImportJobOutput_outputS3Location' - The package\'s output location.
--
-- 'packageId', 'packageImportJobOutput_packageId' - The package\'s ID.
--
-- 'packageVersion', 'packageImportJobOutput_packageVersion' - The package\'s version.
--
-- 'patchVersion', 'packageImportJobOutput_patchVersion' - The package\'s patch version.
newPackageImportJobOutput ::
  -- | 'outputS3Location'
  OutPutS3Location ->
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  -- | 'patchVersion'
  Prelude.Text ->
  PackageImportJobOutput
newPackageImportJobOutput
  pOutputS3Location_
  pPackageId_
  pPackageVersion_
  pPatchVersion_ =
    PackageImportJobOutput'
      { outputS3Location =
          pOutputS3Location_,
        packageId = pPackageId_,
        packageVersion = pPackageVersion_,
        patchVersion = pPatchVersion_
      }

-- | The package\'s output location.
packageImportJobOutput_outputS3Location :: Lens.Lens' PackageImportJobOutput OutPutS3Location
packageImportJobOutput_outputS3Location = Lens.lens (\PackageImportJobOutput' {outputS3Location} -> outputS3Location) (\s@PackageImportJobOutput' {} a -> s {outputS3Location = a} :: PackageImportJobOutput)

-- | The package\'s ID.
packageImportJobOutput_packageId :: Lens.Lens' PackageImportJobOutput Prelude.Text
packageImportJobOutput_packageId = Lens.lens (\PackageImportJobOutput' {packageId} -> packageId) (\s@PackageImportJobOutput' {} a -> s {packageId = a} :: PackageImportJobOutput)

-- | The package\'s version.
packageImportJobOutput_packageVersion :: Lens.Lens' PackageImportJobOutput Prelude.Text
packageImportJobOutput_packageVersion = Lens.lens (\PackageImportJobOutput' {packageVersion} -> packageVersion) (\s@PackageImportJobOutput' {} a -> s {packageVersion = a} :: PackageImportJobOutput)

-- | The package\'s patch version.
packageImportJobOutput_patchVersion :: Lens.Lens' PackageImportJobOutput Prelude.Text
packageImportJobOutput_patchVersion = Lens.lens (\PackageImportJobOutput' {patchVersion} -> patchVersion) (\s@PackageImportJobOutput' {} a -> s {patchVersion = a} :: PackageImportJobOutput)

instance Data.FromJSON PackageImportJobOutput where
  parseJSON =
    Data.withObject
      "PackageImportJobOutput"
      ( \x ->
          PackageImportJobOutput'
            Prelude.<$> (x Data..: "OutputS3Location")
            Prelude.<*> (x Data..: "PackageId")
            Prelude.<*> (x Data..: "PackageVersion")
            Prelude.<*> (x Data..: "PatchVersion")
      )

instance Prelude.Hashable PackageImportJobOutput where
  hashWithSalt _salt PackageImportJobOutput' {..} =
    _salt
      `Prelude.hashWithSalt` outputS3Location
      `Prelude.hashWithSalt` packageId
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` patchVersion

instance Prelude.NFData PackageImportJobOutput where
  rnf PackageImportJobOutput' {..} =
    Prelude.rnf outputS3Location
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf patchVersion
