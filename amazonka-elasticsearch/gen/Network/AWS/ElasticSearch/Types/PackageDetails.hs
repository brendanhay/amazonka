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
-- Module      : Network.AWS.ElasticSearch.Types.PackageDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageDetails where

import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.PackageStatus
import Network.AWS.ElasticSearch.Types.PackageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Basic information about a package.
--
-- /See:/ 'newPackageDetails' smart constructor.
data PackageDetails = PackageDetails'
  { availablePackageVersion :: Prelude.Maybe Prelude.Text,
    -- | Current state of the package. Values are
    -- COPYING\/COPY_FAILED\/AVAILABLE\/DELETING\/DELETE_FAILED
    packageStatus :: Prelude.Maybe PackageStatus,
    -- | User specified name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | Timestamp which tells creation date of the package.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | Internal ID of the package.
    packageID :: Prelude.Maybe Prelude.Text,
    -- | User-specified description of the package.
    packageDescription :: Prelude.Maybe Prelude.Text,
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | Currently supports only TXT-DICTIONARY.
    packageType :: Prelude.Maybe PackageType,
    -- | Additional information if the package is in an error state. Null
    -- otherwise.
    errorDetails :: Prelude.Maybe ErrorDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PackageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availablePackageVersion', 'packageDetails_availablePackageVersion' - Undocumented member.
--
-- 'packageStatus', 'packageDetails_packageStatus' - Current state of the package. Values are
-- COPYING\/COPY_FAILED\/AVAILABLE\/DELETING\/DELETE_FAILED
--
-- 'packageName', 'packageDetails_packageName' - User specified name of the package.
--
-- 'createdAt', 'packageDetails_createdAt' - Timestamp which tells creation date of the package.
--
-- 'packageID', 'packageDetails_packageID' - Internal ID of the package.
--
-- 'packageDescription', 'packageDetails_packageDescription' - User-specified description of the package.
--
-- 'lastUpdatedAt', 'packageDetails_lastUpdatedAt' - Undocumented member.
--
-- 'packageType', 'packageDetails_packageType' - Currently supports only TXT-DICTIONARY.
--
-- 'errorDetails', 'packageDetails_errorDetails' - Additional information if the package is in an error state. Null
-- otherwise.
newPackageDetails ::
  PackageDetails
newPackageDetails =
  PackageDetails'
    { availablePackageVersion =
        Prelude.Nothing,
      packageStatus = Prelude.Nothing,
      packageName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      packageID = Prelude.Nothing,
      packageDescription = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      packageType = Prelude.Nothing,
      errorDetails = Prelude.Nothing
    }

-- | Undocumented member.
packageDetails_availablePackageVersion :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_availablePackageVersion = Lens.lens (\PackageDetails' {availablePackageVersion} -> availablePackageVersion) (\s@PackageDetails' {} a -> s {availablePackageVersion = a} :: PackageDetails)

-- | Current state of the package. Values are
-- COPYING\/COPY_FAILED\/AVAILABLE\/DELETING\/DELETE_FAILED
packageDetails_packageStatus :: Lens.Lens' PackageDetails (Prelude.Maybe PackageStatus)
packageDetails_packageStatus = Lens.lens (\PackageDetails' {packageStatus} -> packageStatus) (\s@PackageDetails' {} a -> s {packageStatus = a} :: PackageDetails)

-- | User specified name of the package.
packageDetails_packageName :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageName = Lens.lens (\PackageDetails' {packageName} -> packageName) (\s@PackageDetails' {} a -> s {packageName = a} :: PackageDetails)

-- | Timestamp which tells creation date of the package.
packageDetails_createdAt :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.UTCTime)
packageDetails_createdAt = Lens.lens (\PackageDetails' {createdAt} -> createdAt) (\s@PackageDetails' {} a -> s {createdAt = a} :: PackageDetails) Prelude.. Lens.mapping Prelude._Time

-- | Internal ID of the package.
packageDetails_packageID :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageID = Lens.lens (\PackageDetails' {packageID} -> packageID) (\s@PackageDetails' {} a -> s {packageID = a} :: PackageDetails)

-- | User-specified description of the package.
packageDetails_packageDescription :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.Text)
packageDetails_packageDescription = Lens.lens (\PackageDetails' {packageDescription} -> packageDescription) (\s@PackageDetails' {} a -> s {packageDescription = a} :: PackageDetails)

-- | Undocumented member.
packageDetails_lastUpdatedAt :: Lens.Lens' PackageDetails (Prelude.Maybe Prelude.UTCTime)
packageDetails_lastUpdatedAt = Lens.lens (\PackageDetails' {lastUpdatedAt} -> lastUpdatedAt) (\s@PackageDetails' {} a -> s {lastUpdatedAt = a} :: PackageDetails) Prelude.. Lens.mapping Prelude._Time

-- | Currently supports only TXT-DICTIONARY.
packageDetails_packageType :: Lens.Lens' PackageDetails (Prelude.Maybe PackageType)
packageDetails_packageType = Lens.lens (\PackageDetails' {packageType} -> packageType) (\s@PackageDetails' {} a -> s {packageType = a} :: PackageDetails)

-- | Additional information if the package is in an error state. Null
-- otherwise.
packageDetails_errorDetails :: Lens.Lens' PackageDetails (Prelude.Maybe ErrorDetails)
packageDetails_errorDetails = Lens.lens (\PackageDetails' {errorDetails} -> errorDetails) (\s@PackageDetails' {} a -> s {errorDetails = a} :: PackageDetails)

instance Prelude.FromJSON PackageDetails where
  parseJSON =
    Prelude.withObject
      "PackageDetails"
      ( \x ->
          PackageDetails'
            Prelude.<$> (x Prelude..:? "AvailablePackageVersion")
            Prelude.<*> (x Prelude..:? "PackageStatus")
            Prelude.<*> (x Prelude..:? "PackageName")
            Prelude.<*> (x Prelude..:? "CreatedAt")
            Prelude.<*> (x Prelude..:? "PackageID")
            Prelude.<*> (x Prelude..:? "PackageDescription")
            Prelude.<*> (x Prelude..:? "LastUpdatedAt")
            Prelude.<*> (x Prelude..:? "PackageType")
            Prelude.<*> (x Prelude..:? "ErrorDetails")
      )

instance Prelude.Hashable PackageDetails

instance Prelude.NFData PackageDetails
