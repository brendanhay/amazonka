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
-- Module      : Amazonka.CodeArtifact.Types.RepositoryExternalConnectionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.RepositoryExternalConnectionInfo where

import Amazonka.CodeArtifact.Types.ExternalConnectionStatus
import Amazonka.CodeArtifact.Types.PackageFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the external connection of a repository.
--
-- /See:/ 'newRepositoryExternalConnectionInfo' smart constructor.
data RepositoryExternalConnectionInfo = RepositoryExternalConnectionInfo'
  { -- | The name of the external connection associated with a repository.
    externalConnectionName :: Prelude.Maybe Prelude.Text,
    -- | The package format associated with a repository\'s external connection.
    -- The valid package formats are:
    --
    -- -   @npm@: A Node Package Manager (npm) package.
    --
    -- -   @pypi@: A Python Package Index (PyPI) package.
    --
    -- -   @maven@: A Maven package that contains compiled code in a
    --     distributable format, such as a JAR file.
    --
    -- -   @nuget@: A NuGet package.
    packageFormat :: Prelude.Maybe PackageFormat,
    -- | The status of the external connection of a repository. There is one
    -- valid value, @Available@.
    status :: Prelude.Maybe ExternalConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryExternalConnectionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalConnectionName', 'repositoryExternalConnectionInfo_externalConnectionName' - The name of the external connection associated with a repository.
--
-- 'packageFormat', 'repositoryExternalConnectionInfo_packageFormat' - The package format associated with a repository\'s external connection.
-- The valid package formats are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
--
-- -   @nuget@: A NuGet package.
--
-- 'status', 'repositoryExternalConnectionInfo_status' - The status of the external connection of a repository. There is one
-- valid value, @Available@.
newRepositoryExternalConnectionInfo ::
  RepositoryExternalConnectionInfo
newRepositoryExternalConnectionInfo =
  RepositoryExternalConnectionInfo'
    { externalConnectionName =
        Prelude.Nothing,
      packageFormat = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the external connection associated with a repository.
repositoryExternalConnectionInfo_externalConnectionName :: Lens.Lens' RepositoryExternalConnectionInfo (Prelude.Maybe Prelude.Text)
repositoryExternalConnectionInfo_externalConnectionName = Lens.lens (\RepositoryExternalConnectionInfo' {externalConnectionName} -> externalConnectionName) (\s@RepositoryExternalConnectionInfo' {} a -> s {externalConnectionName = a} :: RepositoryExternalConnectionInfo)

-- | The package format associated with a repository\'s external connection.
-- The valid package formats are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
--
-- -   @nuget@: A NuGet package.
repositoryExternalConnectionInfo_packageFormat :: Lens.Lens' RepositoryExternalConnectionInfo (Prelude.Maybe PackageFormat)
repositoryExternalConnectionInfo_packageFormat = Lens.lens (\RepositoryExternalConnectionInfo' {packageFormat} -> packageFormat) (\s@RepositoryExternalConnectionInfo' {} a -> s {packageFormat = a} :: RepositoryExternalConnectionInfo)

-- | The status of the external connection of a repository. There is one
-- valid value, @Available@.
repositoryExternalConnectionInfo_status :: Lens.Lens' RepositoryExternalConnectionInfo (Prelude.Maybe ExternalConnectionStatus)
repositoryExternalConnectionInfo_status = Lens.lens (\RepositoryExternalConnectionInfo' {status} -> status) (\s@RepositoryExternalConnectionInfo' {} a -> s {status = a} :: RepositoryExternalConnectionInfo)

instance
  Data.FromJSON
    RepositoryExternalConnectionInfo
  where
  parseJSON =
    Data.withObject
      "RepositoryExternalConnectionInfo"
      ( \x ->
          RepositoryExternalConnectionInfo'
            Prelude.<$> (x Data..:? "externalConnectionName")
            Prelude.<*> (x Data..:? "packageFormat")
            Prelude.<*> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    RepositoryExternalConnectionInfo
  where
  hashWithSalt
    _salt
    RepositoryExternalConnectionInfo' {..} =
      _salt
        `Prelude.hashWithSalt` externalConnectionName
        `Prelude.hashWithSalt` packageFormat
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    RepositoryExternalConnectionInfo
  where
  rnf RepositoryExternalConnectionInfo' {..} =
    Prelude.rnf externalConnectionName
      `Prelude.seq` Prelude.rnf packageFormat
      `Prelude.seq` Prelude.rnf status
