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
-- Module      : Amazonka.OpenSearch.Types.PackageDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.PackageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.ErrorDetails
import Amazonka.OpenSearch.Types.PackageStatus
import Amazonka.OpenSearch.Types.PackageType
import qualified Amazonka.Prelude as Prelude

-- | Basic information about a package.
--
-- /See:/ 'newPackageDetails' smart constructor.
data PackageDetails = PackageDetails'
  { -- | The package version.
    availablePackageVersion :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the package was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Additional information if the package is in an error state. Null
    -- otherwise.
    errorDetails :: Prelude.Maybe ErrorDetails,
    -- | Date and time when the package was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | User-specified description of the package.
    packageDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the package.
    packageID :: Prelude.Maybe Prelude.Text,
    -- | The user-specified name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the package. The available options are
    -- @AVAILABLE@, @COPYING@, @COPY_FAILED@, @VALIDATNG@, @VALIDATION_FAILED@,
    -- @DELETING@, and @DELETE_FAILED@.
    packageStatus :: Prelude.Maybe PackageStatus,
    -- | The type of package.
    packageType :: Prelude.Maybe PackageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availablePackageVersion', 'packageDetails_availablePackageVersion' - The package version.
--
-- 'createdAt', 'packageDetails_createdAt' - The timestamp when the package was created.
--
-- 'errorDetails', 'packageDetails_errorDetails' - Additional information if the package is in an error state. Null
-- otherwise.
--
-- 'lastUpdatedAt', 'packageDetails_lastUpdatedAt' - Date and time when the package was last updated.
--
-- 'packageDescription', 'packageDetails_packageDescription' - User-specified description of the package.
--
-- 'packageID', 'packageDetails_packageID' - The unique identifier of the package.
--
-- 'packageName', 'packageDetails_packageName' - The user-specified name of the package.
--
-- 'packageStatus', 'packageDetails_packageStatus' - The current status of the package. The available options are
-- @AVAILABLE@, @COPYING@, @COPY_FAILED@, @VALIDATNG@, @VALIDATION_FAILED@,
-- @DELETING@, and @DELETE_FAILED@.
--
-- 'packageType', 'packageDetails_packageType' - The type of package.
newPackageDetails ::
  PackageDetails
newPackageDetails =
  PackageDetails'
    { availablePackageVersion =
        Prelude.Nothing,
      createdAt = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      packageDescription = Prelude.Nothing,
      packageID = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageStatus = Prelude.Nothing,
      packageType = Prelude.Nothing
    }

-- | The package version.
packageDetails_availablePackageVersion :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_availablePackageVersion = Lens.lens (\PackageDetails' {availablePackageVersion} -> availablePackageVersion) (\s@PackageDetails' {} a -> s {availablePackageVersion = a} :: PackageDetails)

-- | The timestamp when the package was created.
packageDetails_createdAt :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.UTCTime)
packageDetails_createdAt = Lens.lens (\PackageDetails' {createdAt} -> createdAt) (\s@PackageDetails' {} a -> s {createdAt = a} :: PackageDetails) Prelude.. Lens.mapping Data._Time

-- | Additional information if the package is in an error state. Null
-- otherwise.
packageDetails_errorDetails :: Lens.Lens' PackageDetails (Prelude.Maybe ErrorDetails)
packageDetails_errorDetails = Lens.lens (\PackageDetails' {errorDetails} -> errorDetails) (\s@PackageDetails' {} a -> s {errorDetails = a} :: PackageDetails)

-- | Date and time when the package was last updated.
packageDetails_lastUpdatedAt :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.UTCTime)
packageDetails_lastUpdatedAt = Lens.lens (\PackageDetails' {lastUpdatedAt} -> lastUpdatedAt) (\s@PackageDetails' {} a -> s {lastUpdatedAt = a} :: PackageDetails) Prelude.. Lens.mapping Data._Time

-- | User-specified description of the package.
packageDetails_packageDescription :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageDescription = Lens.lens (\PackageDetails' {packageDescription} -> packageDescription) (\s@PackageDetails' {} a -> s {packageDescription = a} :: PackageDetails)

-- | The unique identifier of the package.
packageDetails_packageID :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageID = Lens.lens (\PackageDetails' {packageID} -> packageID) (\s@PackageDetails' {} a -> s {packageID = a} :: PackageDetails)

-- | The user-specified name of the package.
packageDetails_packageName :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageName = Lens.lens (\PackageDetails' {packageName} -> packageName) (\s@PackageDetails' {} a -> s {packageName = a} :: PackageDetails)

-- | The current status of the package. The available options are
-- @AVAILABLE@, @COPYING@, @COPY_FAILED@, @VALIDATNG@, @VALIDATION_FAILED@,
-- @DELETING@, and @DELETE_FAILED@.
packageDetails_packageStatus :: Lens.Lens' PackageDetails (Prelude.Maybe PackageStatus)
packageDetails_packageStatus = Lens.lens (\PackageDetails' {packageStatus} -> packageStatus) (\s@PackageDetails' {} a -> s {packageStatus = a} :: PackageDetails)

-- | The type of package.
packageDetails_packageType :: Lens.Lens' PackageDetails (Prelude.Maybe PackageType)
packageDetails_packageType = Lens.lens (\PackageDetails' {packageType} -> packageType) (\s@PackageDetails' {} a -> s {packageType = a} :: PackageDetails)

instance Data.FromJSON PackageDetails where
  parseJSON =
    Data.withObject
      "PackageDetails"
      ( \x ->
          PackageDetails'
            Prelude.<$> (x Data..:? "AvailablePackageVersion")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "ErrorDetails")
            Prelude.<*> (x Data..:? "LastUpdatedAt")
            Prelude.<*> (x Data..:? "PackageDescription")
            Prelude.<*> (x Data..:? "PackageID")
            Prelude.<*> (x Data..:? "PackageName")
            Prelude.<*> (x Data..:? "PackageStatus")
            Prelude.<*> (x Data..:? "PackageType")
      )

instance Prelude.Hashable PackageDetails where
  hashWithSalt _salt PackageDetails' {..} =
    _salt
      `Prelude.hashWithSalt` availablePackageVersion
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` packageDescription
      `Prelude.hashWithSalt` packageID
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` packageStatus
      `Prelude.hashWithSalt` packageType

instance Prelude.NFData PackageDetails where
  rnf PackageDetails' {..} =
    Prelude.rnf availablePackageVersion
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf packageDescription
      `Prelude.seq` Prelude.rnf packageID
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageStatus
      `Prelude.seq` Prelude.rnf packageType
