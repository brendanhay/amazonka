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
    dpdDomainName,
    dpdDomainPackageStatus,
    dpdErrorDetails,
    dpdLastUpdated,
    dpdPackageID,
    dpdPackageName,
    dpdPackageType,
    dpdPackageVersion,
    dpdReferencePath,
  )
where

import qualified Network.AWS.ElasticSearch.Types.DomainName as Types
import qualified Network.AWS.ElasticSearch.Types.DomainPackageStatus as Types
import qualified Network.AWS.ElasticSearch.Types.ErrorDetails as Types
import qualified Network.AWS.ElasticSearch.Types.PackageID as Types
import qualified Network.AWS.ElasticSearch.Types.PackageName as Types
import qualified Network.AWS.ElasticSearch.Types.PackageType as Types
import qualified Network.AWS.ElasticSearch.Types.PackageVersion as Types
import qualified Network.AWS.ElasticSearch.Types.ReferencePath as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information on a package that is associated with a domain.
--
-- /See:/ 'mkDomainPackageDetails' smart constructor.
data DomainPackageDetails = DomainPackageDetails'
  { -- | Name of the domain you've associated a package with.
    domainName :: Core.Maybe Types.DomainName,
    -- | State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
    domainPackageStatus :: Core.Maybe Types.DomainPackageStatus,
    -- | Additional information if the package is in an error state. Null otherwise.
    errorDetails :: Core.Maybe Types.ErrorDetails,
    -- | Timestamp of the most-recent update to the association status.
    lastUpdated :: Core.Maybe Core.NominalDiffTime,
    -- | Internal ID of the package.
    packageID :: Core.Maybe Types.PackageID,
    -- | User specified name of the package.
    packageName :: Core.Maybe Types.PackageName,
    -- | Currently supports only TXT-DICTIONARY.
    packageType :: Core.Maybe Types.PackageType,
    packageVersion :: Core.Maybe Types.PackageVersion,
    -- | The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
    referencePath :: Core.Maybe Types.ReferencePath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DomainPackageDetails' value with any optional fields omitted.
mkDomainPackageDetails ::
  DomainPackageDetails
mkDomainPackageDetails =
  DomainPackageDetails'
    { domainName = Core.Nothing,
      domainPackageStatus = Core.Nothing,
      errorDetails = Core.Nothing,
      lastUpdated = Core.Nothing,
      packageID = Core.Nothing,
      packageName = Core.Nothing,
      packageType = Core.Nothing,
      packageVersion = Core.Nothing,
      referencePath = Core.Nothing
    }

-- | Name of the domain you've associated a package with.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDomainName :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.DomainName)
dpdDomainName = Lens.field @"domainName"
{-# DEPRECATED dpdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | State of the association. Values are ASSOCIATING/ASSOCIATION_FAILED/ACTIVE/DISSOCIATING/DISSOCIATION_FAILED.
--
-- /Note:/ Consider using 'domainPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdDomainPackageStatus :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.DomainPackageStatus)
dpdDomainPackageStatus = Lens.field @"domainPackageStatus"
{-# DEPRECATED dpdDomainPackageStatus "Use generic-lens or generic-optics with 'domainPackageStatus' instead." #-}

-- | Additional information if the package is in an error state. Null otherwise.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdErrorDetails :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.ErrorDetails)
dpdErrorDetails = Lens.field @"errorDetails"
{-# DEPRECATED dpdErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | Timestamp of the most-recent update to the association status.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdLastUpdated :: Lens.Lens' DomainPackageDetails (Core.Maybe Core.NominalDiffTime)
dpdLastUpdated = Lens.field @"lastUpdated"
{-# DEPRECATED dpdLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Internal ID of the package.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageID :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.PackageID)
dpdPackageID = Lens.field @"packageID"
{-# DEPRECATED dpdPackageID "Use generic-lens or generic-optics with 'packageID' instead." #-}

-- | User specified name of the package.
--
-- /Note:/ Consider using 'packageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageName :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.PackageName)
dpdPackageName = Lens.field @"packageName"
{-# DEPRECATED dpdPackageName "Use generic-lens or generic-optics with 'packageName' instead." #-}

-- | Currently supports only TXT-DICTIONARY.
--
-- /Note:/ Consider using 'packageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageType :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.PackageType)
dpdPackageType = Lens.field @"packageType"
{-# DEPRECATED dpdPackageType "Use generic-lens or generic-optics with 'packageType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'packageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdPackageVersion :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.PackageVersion)
dpdPackageVersion = Lens.field @"packageVersion"
{-# DEPRECATED dpdPackageVersion "Use generic-lens or generic-optics with 'packageVersion' instead." #-}

-- | The relative path on Amazon ES nodes, which can be used as synonym_path when the package is synonym file.
--
-- /Note:/ Consider using 'referencePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdReferencePath :: Lens.Lens' DomainPackageDetails (Core.Maybe Types.ReferencePath)
dpdReferencePath = Lens.field @"referencePath"
{-# DEPRECATED dpdReferencePath "Use generic-lens or generic-optics with 'referencePath' instead." #-}

instance Core.FromJSON DomainPackageDetails where
  parseJSON =
    Core.withObject "DomainPackageDetails" Core.$
      \x ->
        DomainPackageDetails'
          Core.<$> (x Core..:? "DomainName")
          Core.<*> (x Core..:? "DomainPackageStatus")
          Core.<*> (x Core..:? "ErrorDetails")
          Core.<*> (x Core..:? "LastUpdated")
          Core.<*> (x Core..:? "PackageID")
          Core.<*> (x Core..:? "PackageName")
          Core.<*> (x Core..:? "PackageType")
          Core.<*> (x Core..:? "PackageVersion")
          Core.<*> (x Core..:? "ReferencePath")
