{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.PackageDetails
  ( PackageDetails (..)
  -- * Smart constructor
  , mkPackageDetails
  -- * Lenses
  , pdAvailablePackageVersion
  , pdCreatedAt
  , pdErrorDetails
  , pdLastUpdatedAt
  , pdPackageDescription
  , pdPackageID
  , pdPackageName
  , pdPackageStatus
  , pdPackageType
  ) where

import qualified Network.AWS.ElasticSearch.Types.ErrorDetails as Types
import qualified Network.AWS.ElasticSearch.Types.PackageDescription as Types
import qualified Network.AWS.ElasticSearch.Types.PackageID as Types
import qualified Network.AWS.ElasticSearch.Types.PackageName as Types
import qualified Network.AWS.ElasticSearch.Types.PackageStatus as Types
import qualified Network.AWS.ElasticSearch.Types.PackageType as Types
import qualified Network.AWS.ElasticSearch.Types.PackageVersion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Basic information about a package.
--
-- /See:/ 'mkPackageDetails' smart constructor.
data PackageDetails = PackageDetails'
  { availablePackageVersion :: Core.Maybe Types.PackageVersion
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ Timestamp which tells creation date of the package.
  , errorDetails :: Core.Maybe Types.ErrorDetails
    -- ^ Additional information if the package is in an error state. Null otherwise.
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
  , packageDescription :: Core.Maybe Types.PackageDescription
    -- ^ User-specified description of the package.
  , packageID :: Core.Maybe Types.PackageID
    -- ^ Internal ID of the package.
  , packageName :: Core.Maybe Types.PackageName
    -- ^ User specified name of the package.
  , packageStatus :: Core.Maybe Types.PackageStatus
    -- ^ Current state of the package. Values are COPYING/COPY_FAILED/AVAILABLE/DELETING/DELETE_FAILED
  , packageType :: Core.Maybe Types.PackageType
    -- ^ Currently supports only TXT-DICTIONARY.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PackageDetails' value with any optional fields omitted.
mkPackageDetails
    :: PackageDetails
mkPackageDetails
  = PackageDetails'{availablePackageVersion = Core.Nothing,
                    createdAt = Core.Nothing, errorDetails = Core.Nothing,
                    lastUpdatedAt = Core.Nothing, packageDescription = Core.Nothing,
                    packageID = Core.Nothing, packageName = Core.Nothing,
                    packageStatus = Core.Nothing, packageType = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'availablePackageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAvailablePackageVersion :: Lens.Lens' PackageDetails (Core.Maybe Types.PackageVersion)
pdAvailablePackageVersion = Lens.field @"availablePackageVersion"
{-# INLINEABLE pdAvailablePackageVersion #-}
{-# DEPRECATED availablePackageVersion "Use generic-lens or generic-optics with 'availablePackageVersion' instead"  #-}

-- | Timestamp which tells creation date of the package.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreatedAt :: Lens.Lens' PackageDetails (Core.Maybe Core.NominalDiffTime)
pdCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE pdCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | Additional information if the package is in an error state. Null otherwise.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdErrorDetails :: Lens.Lens' PackageDetails (Core.Maybe Types.ErrorDetails)
pdErrorDetails = Lens.field @"errorDetails"
{-# INLINEABLE pdErrorDetails #-}
{-# DEPRECATED errorDetails "Use generic-lens or generic-optics with 'errorDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLastUpdatedAt :: Lens.Lens' PackageDetails (Core.Maybe Core.NominalDiffTime)
pdLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE pdLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | User-specified description of the package.
--
-- /Note:/ Consider using 'packageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageDescription :: Lens.Lens' PackageDetails (Core.Maybe Types.PackageDescription)
pdPackageDescription = Lens.field @"packageDescription"
{-# INLINEABLE pdPackageDescription #-}
{-# DEPRECATED packageDescription "Use generic-lens or generic-optics with 'packageDescription' instead"  #-}

-- | Internal ID of the package.
--
-- /Note:/ Consider using 'packageID' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageID :: Lens.Lens' PackageDetails (Core.Maybe Types.PackageID)
pdPackageID = Lens.field @"packageID"
{-# INLINEABLE pdPackageID #-}
{-# DEPRECATED packageID "Use generic-lens or generic-optics with 'packageID' instead"  #-}

-- | User specified name of the package.
--
-- /Note:/ Consider using 'packageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageName :: Lens.Lens' PackageDetails (Core.Maybe Types.PackageName)
pdPackageName = Lens.field @"packageName"
{-# INLINEABLE pdPackageName #-}
{-# DEPRECATED packageName "Use generic-lens or generic-optics with 'packageName' instead"  #-}

-- | Current state of the package. Values are COPYING/COPY_FAILED/AVAILABLE/DELETING/DELETE_FAILED
--
-- /Note:/ Consider using 'packageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageStatus :: Lens.Lens' PackageDetails (Core.Maybe Types.PackageStatus)
pdPackageStatus = Lens.field @"packageStatus"
{-# INLINEABLE pdPackageStatus #-}
{-# DEPRECATED packageStatus "Use generic-lens or generic-optics with 'packageStatus' instead"  #-}

-- | Currently supports only TXT-DICTIONARY.
--
-- /Note:/ Consider using 'packageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageType :: Lens.Lens' PackageDetails (Core.Maybe Types.PackageType)
pdPackageType = Lens.field @"packageType"
{-# INLINEABLE pdPackageType #-}
{-# DEPRECATED packageType "Use generic-lens or generic-optics with 'packageType' instead"  #-}

instance Core.FromJSON PackageDetails where
        parseJSON
          = Core.withObject "PackageDetails" Core.$
              \ x ->
                PackageDetails' Core.<$>
                  (x Core..:? "AvailablePackageVersion") Core.<*>
                    x Core..:? "CreatedAt"
                    Core.<*> x Core..:? "ErrorDetails"
                    Core.<*> x Core..:? "LastUpdatedAt"
                    Core.<*> x Core..:? "PackageDescription"
                    Core.<*> x Core..:? "PackageID"
                    Core.<*> x Core..:? "PackageName"
                    Core.<*> x Core..:? "PackageStatus"
                    Core.<*> x Core..:? "PackageType"
