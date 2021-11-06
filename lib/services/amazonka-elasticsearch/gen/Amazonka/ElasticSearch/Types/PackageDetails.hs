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
-- Module      : Amazonka.ElasticSearch.Types.PackageDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.PackageDetails where

import qualified Amazonka.Core as Core
import Amazonka.ElasticSearch.Types.ErrorDetails
import Amazonka.ElasticSearch.Types.PackageStatus
import Amazonka.ElasticSearch.Types.PackageType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Basic information about a package.
--
-- /See:/ 'newPackageDetails' smart constructor.
data PackageDetails = PackageDetails'
  { -- | Internal ID of the package.
    packageID :: Prelude.Maybe Prelude.Text,
    -- | Currently supports only TXT-DICTIONARY.
    packageType :: Prelude.Maybe PackageType,
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | Timestamp which tells creation date of the package.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | User specified name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | Current state of the package. Values are
    -- COPYING\/COPY_FAILED\/AVAILABLE\/DELETING\/DELETE_FAILED
    packageStatus :: Prelude.Maybe PackageStatus,
    -- | User-specified description of the package.
    packageDescription :: Prelude.Maybe Prelude.Text,
    -- | Additional information if the package is in an error state. Null
    -- otherwise.
    errorDetails :: Prelude.Maybe ErrorDetails,
    availablePackageVersion :: Prelude.Maybe Prelude.Text
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
-- 'packageID', 'packageDetails_packageID' - Internal ID of the package.
--
-- 'packageType', 'packageDetails_packageType' - Currently supports only TXT-DICTIONARY.
--
-- 'lastUpdatedAt', 'packageDetails_lastUpdatedAt' - Undocumented member.
--
-- 'createdAt', 'packageDetails_createdAt' - Timestamp which tells creation date of the package.
--
-- 'packageName', 'packageDetails_packageName' - User specified name of the package.
--
-- 'packageStatus', 'packageDetails_packageStatus' - Current state of the package. Values are
-- COPYING\/COPY_FAILED\/AVAILABLE\/DELETING\/DELETE_FAILED
--
-- 'packageDescription', 'packageDetails_packageDescription' - User-specified description of the package.
--
-- 'errorDetails', 'packageDetails_errorDetails' - Additional information if the package is in an error state. Null
-- otherwise.
--
-- 'availablePackageVersion', 'packageDetails_availablePackageVersion' - Undocumented member.
newPackageDetails ::
  PackageDetails
newPackageDetails =
  PackageDetails'
    { packageID = Prelude.Nothing,
      packageType = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageStatus = Prelude.Nothing,
      packageDescription = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      availablePackageVersion = Prelude.Nothing
    }

-- | Internal ID of the package.
packageDetails_packageID :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageID = Lens.lens (\PackageDetails' {packageID} -> packageID) (\s@PackageDetails' {} a -> s {packageID = a} :: PackageDetails)

-- | Currently supports only TXT-DICTIONARY.
packageDetails_packageType :: Lens.Lens' PackageDetails (Prelude.Maybe PackageType)
packageDetails_packageType = Lens.lens (\PackageDetails' {packageType} -> packageType) (\s@PackageDetails' {} a -> s {packageType = a} :: PackageDetails)

-- | Undocumented member.
packageDetails_lastUpdatedAt :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.UTCTime)
packageDetails_lastUpdatedAt = Lens.lens (\PackageDetails' {lastUpdatedAt} -> lastUpdatedAt) (\s@PackageDetails' {} a -> s {lastUpdatedAt = a} :: PackageDetails) Prelude.. Lens.mapping Core._Time

-- | Timestamp which tells creation date of the package.
packageDetails_createdAt :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.UTCTime)
packageDetails_createdAt = Lens.lens (\PackageDetails' {createdAt} -> createdAt) (\s@PackageDetails' {} a -> s {createdAt = a} :: PackageDetails) Prelude.. Lens.mapping Core._Time

-- | User specified name of the package.
packageDetails_packageName :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageName = Lens.lens (\PackageDetails' {packageName} -> packageName) (\s@PackageDetails' {} a -> s {packageName = a} :: PackageDetails)

-- | Current state of the package. Values are
-- COPYING\/COPY_FAILED\/AVAILABLE\/DELETING\/DELETE_FAILED
packageDetails_packageStatus :: Lens.Lens' PackageDetails (Prelude.Maybe PackageStatus)
packageDetails_packageStatus = Lens.lens (\PackageDetails' {packageStatus} -> packageStatus) (\s@PackageDetails' {} a -> s {packageStatus = a} :: PackageDetails)

-- | User-specified description of the package.
packageDetails_packageDescription :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageDescription = Lens.lens (\PackageDetails' {packageDescription} -> packageDescription) (\s@PackageDetails' {} a -> s {packageDescription = a} :: PackageDetails)

-- | Additional information if the package is in an error state. Null
-- otherwise.
packageDetails_errorDetails :: Lens.Lens' PackageDetails (Prelude.Maybe ErrorDetails)
packageDetails_errorDetails = Lens.lens (\PackageDetails' {errorDetails} -> errorDetails) (\s@PackageDetails' {} a -> s {errorDetails = a} :: PackageDetails)

-- | Undocumented member.
packageDetails_availablePackageVersion :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_availablePackageVersion = Lens.lens (\PackageDetails' {availablePackageVersion} -> availablePackageVersion) (\s@PackageDetails' {} a -> s {availablePackageVersion = a} :: PackageDetails)

instance Core.FromJSON PackageDetails where
  parseJSON =
    Core.withObject
      "PackageDetails"
      ( \x ->
          PackageDetails'
            Prelude.<$> (x Core..:? "PackageID")
            Prelude.<*> (x Core..:? "PackageType")
            Prelude.<*> (x Core..:? "LastUpdatedAt")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "PackageName")
            Prelude.<*> (x Core..:? "PackageStatus")
            Prelude.<*> (x Core..:? "PackageDescription")
            Prelude.<*> (x Core..:? "ErrorDetails")
            Prelude.<*> (x Core..:? "AvailablePackageVersion")
      )

instance Prelude.Hashable PackageDetails

instance Prelude.NFData PackageDetails
