{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageDetails
  ( PackageDetails (..),

    -- * Smart constructor
    mkPackageDetails,

    -- * Lenses
    pdPackageId,
    pdPackageType,
    pdLastUpdatedAt,
    pdCreatedAt,
    pdPackageName,
    pdPackageStatus,
    pdPackageDescription,
    pdErrorDetails,
    pdAvailablePackageVersion,
  )
where

import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.PackageStatus
import Network.AWS.ElasticSearch.Types.PackageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Basic information about a package.
--
-- /See:/ 'mkPackageDetails' smart constructor.
data PackageDetails = PackageDetails'
  { packageId ::
      Lude.Maybe Lude.Text,
    packageType :: Lude.Maybe PackageType,
    lastUpdatedAt :: Lude.Maybe Lude.Timestamp,
    createdAt :: Lude.Maybe Lude.Timestamp,
    packageName :: Lude.Maybe Lude.Text,
    packageStatus :: Lude.Maybe PackageStatus,
    packageDescription :: Lude.Maybe Lude.Text,
    errorDetails :: Lude.Maybe ErrorDetails,
    availablePackageVersion :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PackageDetails' with the minimum fields required to make a request.
--
-- * 'availablePackageVersion' - Undocumented field.
-- * 'createdAt' - Timestamp which tells creation date of the package.
-- * 'errorDetails' - Additional information if the package is in an error state. Null otherwise.
-- * 'lastUpdatedAt' - Undocumented field.
-- * 'packageDescription' - User-specified description of the package.
-- * 'packageId' - Internal ID of the package.
-- * 'packageName' - User specified name of the package.
-- * 'packageStatus' - Current state of the package. Values are COPYING/COPY_FAILED/AVAILABLE/DELETING/DELETE_FAILED
-- * 'packageType' - Currently supports only TXT-DICTIONARY.
mkPackageDetails ::
  PackageDetails
mkPackageDetails =
  PackageDetails'
    { packageId = Lude.Nothing,
      packageType = Lude.Nothing,
      lastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      packageName = Lude.Nothing,
      packageStatus = Lude.Nothing,
      packageDescription = Lude.Nothing,
      errorDetails = Lude.Nothing,
      availablePackageVersion = Lude.Nothing
    }

-- | Internal ID of the package.
--
-- /Note:/ Consider using 'packageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageId :: Lens.Lens' PackageDetails (Lude.Maybe Lude.Text)
pdPackageId = Lens.lens (packageId :: PackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {packageId = a} :: PackageDetails)
{-# DEPRECATED pdPackageId "Use generic-lens or generic-optics with 'packageId' instead." #-}

-- | Currently supports only TXT-DICTIONARY.
--
-- /Note:/ Consider using 'packageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageType :: Lens.Lens' PackageDetails (Lude.Maybe PackageType)
pdPackageType = Lens.lens (packageType :: PackageDetails -> Lude.Maybe PackageType) (\s a -> s {packageType = a} :: PackageDetails)
{-# DEPRECATED pdPackageType "Use generic-lens or generic-optics with 'packageType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLastUpdatedAt :: Lens.Lens' PackageDetails (Lude.Maybe Lude.Timestamp)
pdLastUpdatedAt = Lens.lens (lastUpdatedAt :: PackageDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: PackageDetails)
{-# DEPRECATED pdLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | Timestamp which tells creation date of the package.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreatedAt :: Lens.Lens' PackageDetails (Lude.Maybe Lude.Timestamp)
pdCreatedAt = Lens.lens (createdAt :: PackageDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: PackageDetails)
{-# DEPRECATED pdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | User specified name of the package.
--
-- /Note:/ Consider using 'packageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageName :: Lens.Lens' PackageDetails (Lude.Maybe Lude.Text)
pdPackageName = Lens.lens (packageName :: PackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {packageName = a} :: PackageDetails)
{-# DEPRECATED pdPackageName "Use generic-lens or generic-optics with 'packageName' instead." #-}

-- | Current state of the package. Values are COPYING/COPY_FAILED/AVAILABLE/DELETING/DELETE_FAILED
--
-- /Note:/ Consider using 'packageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageStatus :: Lens.Lens' PackageDetails (Lude.Maybe PackageStatus)
pdPackageStatus = Lens.lens (packageStatus :: PackageDetails -> Lude.Maybe PackageStatus) (\s a -> s {packageStatus = a} :: PackageDetails)
{-# DEPRECATED pdPackageStatus "Use generic-lens or generic-optics with 'packageStatus' instead." #-}

-- | User-specified description of the package.
--
-- /Note:/ Consider using 'packageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdPackageDescription :: Lens.Lens' PackageDetails (Lude.Maybe Lude.Text)
pdPackageDescription = Lens.lens (packageDescription :: PackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {packageDescription = a} :: PackageDetails)
{-# DEPRECATED pdPackageDescription "Use generic-lens or generic-optics with 'packageDescription' instead." #-}

-- | Additional information if the package is in an error state. Null otherwise.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdErrorDetails :: Lens.Lens' PackageDetails (Lude.Maybe ErrorDetails)
pdErrorDetails = Lens.lens (errorDetails :: PackageDetails -> Lude.Maybe ErrorDetails) (\s a -> s {errorDetails = a} :: PackageDetails)
{-# DEPRECATED pdErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'availablePackageVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdAvailablePackageVersion :: Lens.Lens' PackageDetails (Lude.Maybe Lude.Text)
pdAvailablePackageVersion = Lens.lens (availablePackageVersion :: PackageDetails -> Lude.Maybe Lude.Text) (\s a -> s {availablePackageVersion = a} :: PackageDetails)
{-# DEPRECATED pdAvailablePackageVersion "Use generic-lens or generic-optics with 'availablePackageVersion' instead." #-}

instance Lude.FromJSON PackageDetails where
  parseJSON =
    Lude.withObject
      "PackageDetails"
      ( \x ->
          PackageDetails'
            Lude.<$> (x Lude..:? "PackageID")
            Lude.<*> (x Lude..:? "PackageType")
            Lude.<*> (x Lude..:? "LastUpdatedAt")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "PackageName")
            Lude.<*> (x Lude..:? "PackageStatus")
            Lude.<*> (x Lude..:? "PackageDescription")
            Lude.<*> (x Lude..:? "ErrorDetails")
            Lude.<*> (x Lude..:? "AvailablePackageVersion")
      )
