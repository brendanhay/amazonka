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
-- Module      : Amazonka.ElasticSearch.Types.DomainPackageDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.DomainPackageDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.DomainPackageStatus
import Amazonka.ElasticSearch.Types.ErrorDetails
import Amazonka.ElasticSearch.Types.PackageType
import qualified Amazonka.Prelude as Prelude

-- | Information on a package that is associated with a domain.
--
-- /See:/ 'newDomainPackageDetails' smart constructor.
data DomainPackageDetails = DomainPackageDetails'
  { -- | Name of the domain you\'ve associated a package with.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | State of the association. Values are
    -- ASSOCIATING\/ASSOCIATION_FAILED\/ACTIVE\/DISSOCIATING\/DISSOCIATION_FAILED.
    domainPackageStatus :: Prelude.Maybe DomainPackageStatus,
    -- | Additional information if the package is in an error state. Null
    -- otherwise.
    errorDetails :: Prelude.Maybe ErrorDetails,
    -- | Timestamp of the most-recent update to the association status.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | Internal ID of the package.
    packageID :: Prelude.Maybe Prelude.Text,
    -- | User specified name of the package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | Currently supports only TXT-DICTIONARY.
    packageType :: Prelude.Maybe PackageType,
    packageVersion :: Prelude.Maybe Prelude.Text,
    -- | The relative path on Amazon ES nodes, which can be used as synonym_path
    -- when the package is synonym file.
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
-- 'domainName', 'domainPackageDetails_domainName' - Name of the domain you\'ve associated a package with.
--
-- 'domainPackageStatus', 'domainPackageDetails_domainPackageStatus' - State of the association. Values are
-- ASSOCIATING\/ASSOCIATION_FAILED\/ACTIVE\/DISSOCIATING\/DISSOCIATION_FAILED.
--
-- 'errorDetails', 'domainPackageDetails_errorDetails' - Additional information if the package is in an error state. Null
-- otherwise.
--
-- 'lastUpdated', 'domainPackageDetails_lastUpdated' - Timestamp of the most-recent update to the association status.
--
-- 'packageID', 'domainPackageDetails_packageID' - Internal ID of the package.
--
-- 'packageName', 'domainPackageDetails_packageName' - User specified name of the package.
--
-- 'packageType', 'domainPackageDetails_packageType' - Currently supports only TXT-DICTIONARY.
--
-- 'packageVersion', 'domainPackageDetails_packageVersion' - Undocumented member.
--
-- 'referencePath', 'domainPackageDetails_referencePath' - The relative path on Amazon ES nodes, which can be used as synonym_path
-- when the package is synonym file.
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

-- | Name of the domain you\'ve associated a package with.
domainPackageDetails_domainName :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_domainName = Lens.lens (\DomainPackageDetails' {domainName} -> domainName) (\s@DomainPackageDetails' {} a -> s {domainName = a} :: DomainPackageDetails)

-- | State of the association. Values are
-- ASSOCIATING\/ASSOCIATION_FAILED\/ACTIVE\/DISSOCIATING\/DISSOCIATION_FAILED.
domainPackageDetails_domainPackageStatus :: Lens.Lens' DomainPackageDetails (Prelude.Maybe DomainPackageStatus)
domainPackageDetails_domainPackageStatus = Lens.lens (\DomainPackageDetails' {domainPackageStatus} -> domainPackageStatus) (\s@DomainPackageDetails' {} a -> s {domainPackageStatus = a} :: DomainPackageDetails)

-- | Additional information if the package is in an error state. Null
-- otherwise.
domainPackageDetails_errorDetails :: Lens.Lens' DomainPackageDetails (Prelude.Maybe ErrorDetails)
domainPackageDetails_errorDetails = Lens.lens (\DomainPackageDetails' {errorDetails} -> errorDetails) (\s@DomainPackageDetails' {} a -> s {errorDetails = a} :: DomainPackageDetails)

-- | Timestamp of the most-recent update to the association status.
domainPackageDetails_lastUpdated :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.UTCTime)
domainPackageDetails_lastUpdated = Lens.lens (\DomainPackageDetails' {lastUpdated} -> lastUpdated) (\s@DomainPackageDetails' {} a -> s {lastUpdated = a} :: DomainPackageDetails) Prelude.. Lens.mapping Data._Time

-- | Internal ID of the package.
domainPackageDetails_packageID :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_packageID = Lens.lens (\DomainPackageDetails' {packageID} -> packageID) (\s@DomainPackageDetails' {} a -> s {packageID = a} :: DomainPackageDetails)

-- | User specified name of the package.
domainPackageDetails_packageName :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_packageName = Lens.lens (\DomainPackageDetails' {packageName} -> packageName) (\s@DomainPackageDetails' {} a -> s {packageName = a} :: DomainPackageDetails)

-- | Currently supports only TXT-DICTIONARY.
domainPackageDetails_packageType :: Lens.Lens' DomainPackageDetails (Prelude.Maybe PackageType)
domainPackageDetails_packageType = Lens.lens (\DomainPackageDetails' {packageType} -> packageType) (\s@DomainPackageDetails' {} a -> s {packageType = a} :: DomainPackageDetails)

-- | Undocumented member.
domainPackageDetails_packageVersion :: Lens.Lens' DomainPackageDetails (Prelude.Maybe Prelude.Text)
domainPackageDetails_packageVersion = Lens.lens (\DomainPackageDetails' {packageVersion} -> packageVersion) (\s@DomainPackageDetails' {} a -> s {packageVersion = a} :: DomainPackageDetails)

-- | The relative path on Amazon ES nodes, which can be used as synonym_path
-- when the package is synonym file.
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
    _salt
      `Prelude.hashWithSalt` domainName
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
    Prelude.rnf domainName `Prelude.seq`
      Prelude.rnf domainPackageStatus `Prelude.seq`
        Prelude.rnf errorDetails `Prelude.seq`
          Prelude.rnf lastUpdated `Prelude.seq`
            Prelude.rnf packageID `Prelude.seq`
              Prelude.rnf packageName `Prelude.seq`
                Prelude.rnf packageType `Prelude.seq`
                  Prelude.rnf packageVersion `Prelude.seq`
                    Prelude.rnf referencePath
