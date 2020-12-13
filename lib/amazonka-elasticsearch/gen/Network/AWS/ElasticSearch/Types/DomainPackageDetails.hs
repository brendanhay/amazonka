{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DomainPackageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DomainPackageDetails
  ( DomainPackageDetails (..),

    -- * Smart constructor
    mkDomainPackageDetails,

    -- * Lenses
    dpdLastUpdated,
    dpdPackageId,
    dpdPackageType,
    dpdPackageName,
    dpdPackageVersion,
    dpdDomainPackageStatus,
    dpdDomainName,
    dpdErrorDetails,
    dpdReferencePath,
  )
where

import Network.AWS.ElasticSearch.Types.DomainPackageStatus
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.PackageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information on a package that is associated with a domain.
--
-- /See:/ 'mkDomainPackageDetails' smart constructor.
data DomainPackageDetails = DomainPackageDetails'
  { -- | Timestamp of the most-recent update to the association status.
    lastUpdated :: Lude.Maybe Lude.Timestamp,
    -- | Internal ID of the package.
    packageId :: Lude.Maybe Lude.Text,
    -- | Currently supports only TXT-DICTIONARY.
    packageType :: Lude.Maybe PackageType,
    -- | User specified name of the package.
    packageName :: Lude.Maybe Lude.Text,
    packageVersion :: Lude.Maybe Lude.Text,
    -- | State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
    domainPackageStatus :: Lude.Maybe DomainPackageStatus,
    -- | Name of the domain you've associated a package with.
    domainName :: Lude.Maybe Lude.Text,
    -- | Additional information if the package is in an error state. Null otherwise.
    errorDetails :: Lude.Maybe ErrorDetails,
    -- | The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
    referencePath :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainPackageDetails' with the minimum fields required to make a request.
--
-- * 'lastUpdated' - Timestamp of the most-recent update to the association status.
-- * 'packageId' - Internal ID of the package.
-- * 'packageType' - Currently supports only TXT-DICTIONARY.
-- * 'packageName' - User specified name of the package.
-- * 'packageVersion' -
-- * 'domainPackageStatus' - State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
-- * 'domainName' - Name of the domain you've associated a package with.
-- * 'errorDetails' - Additional information if the package is in an error state. Null otherwise.
-- * 'referencePath' - The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
mkDomainPackageDetails ::
  DomainPackageDetails
mkDomainPackageDetails =
  DomainPackageDetails'
    { lastUpdated = Lude.Nothing,
      packageId = Lude.Nothing,
      packageType = Lude.Nothing,
      packageName = Lude.Nothing,
      packageVersion = Lude.Nothing,
      domainPackageStatus = Lude.Nothing,
      domainName = Lude.Nothing,
      errorDetails = Lude.Nothing,
      referencePath = Lude.Nothing
    }

-- | Timestamp of the most-recent update to the association status.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdLastUpdated :: Lens.Lens' DomainPackageDetails (Lude.Maybe Lude.Timestamp)
dpdLastUpdated = Lens.lens (lastUpdated :: DomainPackageDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdated = a} :: DomainPackageDetails)
{-# DEPRECATED dpdLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Internal ID of the package.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageId :: Lens.Lens' DomainPackageDetails (Lude.Maybe Lude.Text)
dpdPackageId = Lens.lens (packageId :: DomainPackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {packageId = a} :: DomainPackageDetails)
{-# DEPRECATED dpdPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | Currently supports only TXT-DICTIONARY.
--
-- /Note:/ Consider using 'packageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageType :: Lens.Lens' DomainPackageDetails (Lude.Maybe PackageType)
dpdPackageType = Lens.lens (packageType :: DomainPackageDetails -> Lude.Maybe PackageType) (\s a -> s {packageType = a} :: DomainPackageDetails)
{-# DEPRECATED dpdPackageType "Use generic-lens or generic-optics with 'packageType' instead." #-}

-- | User specified name of the package.
--
-- /Note:/ Consider using 'packageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageName :: Lens.Lens' DomainPackageDetails (Lude.Maybe Lude.Text)
dpdPackageName = Lens.lens (packageName :: DomainPackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {packageName = a} :: DomainPackageDetails)
{-# DEPRECATED dpdPackageName "Use generic-lens or generic-optics with 'packageName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageVersion :: Lens.Lens' DomainPackageDetails (Lude.Maybe Lude.Text)
dpdPackageVersion = Lens.lens (packageVersion :: DomainPackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {packageVersion = a} :: DomainPackageDetails)
{-# DEPRECATED dpdPackageVersion "Use generic-lens or generic-optics with 'packageVersion' instead." #-}

-- | State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
--
-- /Note:/ Consider using 'domainPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDomainPackageStatus :: Lens.Lens' DomainPackageDetails (Lude.Maybe DomainPackageStatus)
dpdDomainPackageStatus = Lens.lens (domainPackageStatus :: DomainPackageDetails -> Lude.Maybe DomainPackageStatus) (\s a -> s {domainPackageStatus = a} :: DomainPackageDetails)
{-# DEPRECATED dpdDomainPackageStatus "Use generic-lens or generic-optics with 'domainPackageStatus' instead." #-}

-- | Name of the domain you've associated a package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDomainName :: Lens.Lens' DomainPackageDetails (Lude.Maybe Lude.Text)
dpdDomainName = Lens.lens (domainName :: DomainPackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DomainPackageDetails)
{-# DEPRECATED dpdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | Additional information if the package is in an error state. Null otherwise.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdErrorDetails :: Lens.Lens' DomainPackageDetails (Lude.Maybe ErrorDetails)
dpdErrorDetails = Lens.lens (errorDetails :: DomainPackageDetails -> Lude.Maybe ErrorDetails) (\s a -> s {errorDetails = a} :: DomainPackageDetails)
{-# DEPRECATED dpdErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
--
-- /Note:/ Consider using 'referencePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdReferencePath :: Lens.Lens' DomainPackageDetails (Lude.Maybe Lude.Text)
dpdReferencePath = Lens.lens (referencePath :: DomainPackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {referencePath = a} :: DomainPackageDetails)
{-# DEPRECATED dpdReferencePath "Use generic-lens or generic-optics with 'referencePath' instead." #-}

instance Lude.FromJSON DomainPackageDetails where
  parseJSON =
    Lude.withObject
      "DomainPackageDetails"
      ( \x ->
          DomainPackageDetails'
            Lude.<$> (x Lude..:? "LastUpdated")
            Lude.<*> (x Lude..:? "PackageID")
            Lude.<*> (x Lude..:? "PackageType")
            Lude.<*> (x Lude..:? "PackageName")
            Lude.<*> (x Lude..:? "PackageVersion")
            Lude.<*> (x Lude..:? "DomainPackageStatus")
            Lude.<*> (x Lude..:? "DomainName")
            Lude.<*> (x Lude..:? "ErrorDetails")
            Lude.<*> (x Lude..:? "ReferencePath")
      )
