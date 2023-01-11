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
-- Module      : Amazonka.OpenSearch.Types.DomainPackageDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainPackageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.DomainPackageStatus
import Amazonka.OpenSearch.Types.ErrorDetails
import Amazonka.OpenSearch.Types.PackageType
import qualified Amazonka.Prelude as Prelude

-- | Information about a package that is associated with a domain. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/custom-packages.html Custom packages for Amazon OpenSearch Service>.
--
-- /See:/ 'newDomainPackageDetails' smart constructor.
data DomainPackageDetails = DomainPackageDetails'
  { -- | Name of the domain that the package is associated with.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | State of the association.
    domainPackageStatus :: Prelude.Maybe DomainPackageStatus,
    -- | Additional information if the package is in an error state. Null
    -- otherwise.
    errorDetails :: Prelude.Maybe ErrorDetails,
    -- | Timestamp of the most recent update to the package association status.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | Internal ID of the package.
    packageID :: Prelude.Maybe Prelude.Text,
    -- | User-specified name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | The type of package.
    packageType :: Prelude.Maybe PackageType,
    -- | The current version of the package.
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | Denotes the location of the package on the OpenSearch Service cluster
    -- nodes. It\'s the same as @synonym_path@ for dictionary files.
    referencePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainPackageDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'domainPackageDetails_domainName' - Name of the domain that the package is associated with.
--
-- 'domainPackageStatus', 'domainPackageDetails_domainPackageStatus' - State of the association.
--
-- 'errorDetails', 'domainPackageDetails_errorDetails' - Additional information if the package is in an error state. Null
-- otherwise.
--
-- 'lastUpdated', 'domainPackageDetails_lastUpdated' - Timestamp of the most recent update to the package association status.
--
-- 'packageID', 'domainPackageDetails_packageID' - Internal ID of the package.
--
-- 'packageName', 'domainPackageDetails_packageName' - User-specified name of the package.
--
-- 'packageType', 'domainPackageDetails_packageType' - The type of package.
--
-- 'packageVersion', 'domainPackageDetails_packageVersion' - The current version of the package.
--
-- 'referencePath', 'domainPackageDetails_referencePath' - Denotes the location of the package on the OpenSearch Service cluster
-- nodes. It\'s the same as @synonym_path@ for dictionary files.
newDomainPackageDetails ::
  DomainPackageDetails
newDomainPackageDetails =
  DomainPackageDetails'
    { domainName = Prelude.Nothing,
      domainPackageStatus = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      packageID = Prelude.Nothing,
      packageName = Prelude.Nothing,
      packageType = Prelude.Nothing,
      packageVersion = Prelude.Nothing,
      referencePath = Prelude.Nothing
    }

-- | Name of the domain that the package is associated with.
domainPackageDetails_domainName :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_domainName = Lens.lens (\DomainPackageDetails' {domainName} -> domainName) (\s@DomainPackageDetails' {} a -> s {domainName = a} :: DomainPackageDetails)

-- | State of the association.
domainPackageDetails_domainPackageStatus :: Lens.Lens' DomainPackageDetails (Prelude.Maybe DomainPackageStatus)
domainPackageDetails_domainPackageStatus = Lens.lens (\DomainPackageDetails' {domainPackageStatus} -> domainPackageStatus) (\s@DomainPackageDetails' {} a -> s {domainPackageStatus = a} :: DomainPackageDetails)

-- | Additional information if the package is in an error state. Null
-- otherwise.
domainPackageDetails_errorDetails :: Lens.Lens' DomainPackageDetails (Prelude.Maybe ErrorDetails)
domainPackageDetails_errorDetails = Lens.lens (\DomainPackageDetails' {errorDetails} -> errorDetails) (\s@DomainPackageDetails' {} a -> s {errorDetails = a} :: DomainPackageDetails)

-- | Timestamp of the most recent update to the package association status.
domainPackageDetails_lastUpdated :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.UTCTime)
domainPackageDetails_lastUpdated = Lens.lens (\DomainPackageDetails' {lastUpdated} -> lastUpdated) (\s@DomainPackageDetails' {} a -> s {lastUpdated = a} :: DomainPackageDetails) Prelude.. Lens.mapping Data._Time

-- | Internal ID of the package.
domainPackageDetails_packageID :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_packageID = Lens.lens (\DomainPackageDetails' {packageID} -> packageID) (\s@DomainPackageDetails' {} a -> s {packageID = a} :: DomainPackageDetails)

-- | User-specified name of the package.
domainPackageDetails_packageName :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_packageName = Lens.lens (\DomainPackageDetails' {packageName} -> packageName) (\s@DomainPackageDetails' {} a -> s {packageName = a} :: DomainPackageDetails)

-- | The type of package.
domainPackageDetails_packageType :: Lens.Lens' DomainPackageDetails (Prelude.Maybe PackageType)
domainPackageDetails_packageType = Lens.lens (\DomainPackageDetails' {packageType} -> packageType) (\s@DomainPackageDetails' {} a -> s {packageType = a} :: DomainPackageDetails)

-- | The current version of the package.
domainPackageDetails_packageVersion :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_packageVersion = Lens.lens (\DomainPackageDetails' {packageVersion} -> packageVersion) (\s@DomainPackageDetails' {} a -> s {packageVersion = a} :: DomainPackageDetails)

-- | Denotes the location of the package on the OpenSearch Service cluster
-- nodes. It\'s the same as @synonym_path@ for dictionary files.
domainPackageDetails_referencePath :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_referencePath = Lens.lens (\DomainPackageDetails' {referencePath} -> referencePath) (\s@DomainPackageDetails' {} a -> s {referencePath = a} :: DomainPackageDetails)

instance Data.FromJSON DomainPackageDetails where
  parseJSON =
    Data.withObject
      "DomainPackageDetails"
      ( \x ->
          DomainPackageDetails'
            Prelude.<$> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "DomainPackageStatus")
            Prelude.<*> (x Data..:? "ErrorDetails")
            Prelude.<*> (x Data..:? "LastUpdated")
            Prelude.<*> (x Data..:? "PackageID")
            Prelude.<*> (x Data..:? "PackageName")
            Prelude.<*> (x Data..:? "PackageType")
            Prelude.<*> (x Data..:? "PackageVersion")
            Prelude.<*> (x Data..:? "ReferencePath")
      )

instance Prelude.Hashable DomainPackageDetails where
  hashWithSalt _salt DomainPackageDetails' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainPackageStatus
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` packageID
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` packageType
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` referencePath

instance Prelude.NFData DomainPackageDetails where
  rnf DomainPackageDetails' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainPackageStatus
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf packageID
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf packageType
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf referencePath
