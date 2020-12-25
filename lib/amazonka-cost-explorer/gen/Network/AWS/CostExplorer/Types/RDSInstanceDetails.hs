{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RDSInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RDSInstanceDetails
  ( RDSInstanceDetails (..),

    -- * Smart constructor
    mkRDSInstanceDetails,

    -- * Lenses
    rdsidCurrentGeneration,
    rdsidDatabaseEdition,
    rdsidDatabaseEngine,
    rdsidDeploymentOption,
    rdsidFamily,
    rdsidInstanceType,
    rdsidLicenseModel,
    rdsidRegion,
    rdsidSizeFlexEligible,
  )
where

import qualified Network.AWS.CostExplorer.Types.DatabaseEdition as Types
import qualified Network.AWS.CostExplorer.Types.DatabaseEngine as Types
import qualified Network.AWS.CostExplorer.Types.DeploymentOption as Types
import qualified Network.AWS.CostExplorer.Types.Family as Types
import qualified Network.AWS.CostExplorer.Types.InstanceType as Types
import qualified Network.AWS.CostExplorer.Types.LicenseModel as Types
import qualified Network.AWS.CostExplorer.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the Amazon RDS instances that AWS recommends that you purchase.
--
-- /See:/ 'mkRDSInstanceDetails' smart constructor.
data RDSInstanceDetails = RDSInstanceDetails'
  { -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | The database edition that the recommended reservation supports.
    databaseEdition :: Core.Maybe Types.DatabaseEdition,
    -- | The database engine that the recommended reservation supports.
    databaseEngine :: Core.Maybe Types.DatabaseEngine,
    -- | Whether the recommendation is for a reservation in a single Availability Zone or a reservation with a backup in a second Availability Zone.
    deploymentOption :: Core.Maybe Types.DeploymentOption,
    -- | The instance family of the recommended reservation.
    family :: Core.Maybe Types.Family,
    -- | The type of instance that AWS recommends.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The license model that the recommended reservation supports.
    licenseModel :: Core.Maybe Types.LicenseModel,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Types.Region,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RDSInstanceDetails' value with any optional fields omitted.
mkRDSInstanceDetails ::
  RDSInstanceDetails
mkRDSInstanceDetails =
  RDSInstanceDetails'
    { currentGeneration = Core.Nothing,
      databaseEdition = Core.Nothing,
      databaseEngine = Core.Nothing,
      deploymentOption = Core.Nothing,
      family = Core.Nothing,
      instanceType = Core.Nothing,
      licenseModel = Core.Nothing,
      region = Core.Nothing,
      sizeFlexEligible = Core.Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidCurrentGeneration :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Bool)
rdsidCurrentGeneration = Lens.field @"currentGeneration"
{-# DEPRECATED rdsidCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The database edition that the recommended reservation supports.
--
-- /Note:/ Consider using 'databaseEdition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidDatabaseEdition :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.DatabaseEdition)
rdsidDatabaseEdition = Lens.field @"databaseEdition"
{-# DEPRECATED rdsidDatabaseEdition "Use generic-lens or generic-optics with 'databaseEdition' instead." #-}

-- | The database engine that the recommended reservation supports.
--
-- /Note:/ Consider using 'databaseEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidDatabaseEngine :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.DatabaseEngine)
rdsidDatabaseEngine = Lens.field @"databaseEngine"
{-# DEPRECATED rdsidDatabaseEngine "Use generic-lens or generic-optics with 'databaseEngine' instead." #-}

-- | Whether the recommendation is for a reservation in a single Availability Zone or a reservation with a backup in a second Availability Zone.
--
-- /Note:/ Consider using 'deploymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidDeploymentOption :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.DeploymentOption)
rdsidDeploymentOption = Lens.field @"deploymentOption"
{-# DEPRECATED rdsidDeploymentOption "Use generic-lens or generic-optics with 'deploymentOption' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidFamily :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.Family)
rdsidFamily = Lens.field @"family"
{-# DEPRECATED rdsidFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The type of instance that AWS recommends.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidInstanceType :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.InstanceType)
rdsidInstanceType = Lens.field @"instanceType"
{-# DEPRECATED rdsidInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The license model that the recommended reservation supports.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidLicenseModel :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.LicenseModel)
rdsidLicenseModel = Lens.field @"licenseModel"
{-# DEPRECATED rdsidLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidRegion :: Lens.Lens' RDSInstanceDetails (Core.Maybe Types.Region)
rdsidRegion = Lens.field @"region"
{-# DEPRECATED rdsidRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsidSizeFlexEligible :: Lens.Lens' RDSInstanceDetails (Core.Maybe Core.Bool)
rdsidSizeFlexEligible = Lens.field @"sizeFlexEligible"
{-# DEPRECATED rdsidSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

instance Core.FromJSON RDSInstanceDetails where
  parseJSON =
    Core.withObject "RDSInstanceDetails" Core.$
      \x ->
        RDSInstanceDetails'
          Core.<$> (x Core..:? "CurrentGeneration")
          Core.<*> (x Core..:? "DatabaseEdition")
          Core.<*> (x Core..:? "DatabaseEngine")
          Core.<*> (x Core..:? "DeploymentOption")
          Core.<*> (x Core..:? "Family")
          Core.<*> (x Core..:? "InstanceType")
          Core.<*> (x Core..:? "LicenseModel")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "SizeFlexEligible")
