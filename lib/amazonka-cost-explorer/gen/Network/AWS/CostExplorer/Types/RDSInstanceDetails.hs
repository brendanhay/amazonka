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
    ridCurrentGeneration,
    ridDeploymentOption,
    ridFamily,
    ridInstanceType,
    ridLicenseModel,
    ridSizeFlexEligible,
    ridRegion,
    ridDatabaseEngine,
    ridDatabaseEdition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Amazon RDS instances that AWS recommends that you purchase.
--
-- /See:/ 'mkRDSInstanceDetails' smart constructor.
data RDSInstanceDetails = RDSInstanceDetails'
  { currentGeneration ::
      Lude.Maybe Lude.Bool,
    deploymentOption :: Lude.Maybe Lude.Text,
    family :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe Lude.Text,
    licenseModel :: Lude.Maybe Lude.Text,
    sizeFlexEligible :: Lude.Maybe Lude.Bool,
    region :: Lude.Maybe Lude.Text,
    databaseEngine :: Lude.Maybe Lude.Text,
    databaseEdition :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RDSInstanceDetails' with the minimum fields required to make a request.
--
-- * 'currentGeneration' - Whether the recommendation is for a current-generation instance.
-- * 'databaseEdition' - The database edition that the recommended reservation supports.
-- * 'databaseEngine' - The database engine that the recommended reservation supports.
-- * 'deploymentOption' - Whether the recommendation is for a reservation in a single Availability Zone or a reservation with a backup in a second Availability Zone.
-- * 'family' - The instance family of the recommended reservation.
-- * 'instanceType' - The type of instance that AWS recommends.
-- * 'licenseModel' - The license model that the recommended reservation supports.
-- * 'region' - The AWS Region of the recommended reservation.
-- * 'sizeFlexEligible' - Whether the recommended reservation is size flexible.
mkRDSInstanceDetails ::
  RDSInstanceDetails
mkRDSInstanceDetails =
  RDSInstanceDetails'
    { currentGeneration = Lude.Nothing,
      deploymentOption = Lude.Nothing,
      family = Lude.Nothing,
      instanceType = Lude.Nothing,
      licenseModel = Lude.Nothing,
      sizeFlexEligible = Lude.Nothing,
      region = Lude.Nothing,
      databaseEngine = Lude.Nothing,
      databaseEdition = Lude.Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridCurrentGeneration :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Bool)
ridCurrentGeneration = Lens.lens (currentGeneration :: RDSInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {currentGeneration = a} :: RDSInstanceDetails)
{-# DEPRECATED ridCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | Whether the recommendation is for a reservation in a single Availability Zone or a reservation with a backup in a second Availability Zone.
--
-- /Note:/ Consider using 'deploymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridDeploymentOption :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridDeploymentOption = Lens.lens (deploymentOption :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {deploymentOption = a} :: RDSInstanceDetails)
{-# DEPRECATED ridDeploymentOption "Use generic-lens or generic-optics with 'deploymentOption' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridFamily :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridFamily = Lens.lens (family :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: RDSInstanceDetails)
{-# DEPRECATED ridFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The type of instance that AWS recommends.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridInstanceType :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridInstanceType = Lens.lens (instanceType :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: RDSInstanceDetails)
{-# DEPRECATED ridInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The license model that the recommended reservation supports.
--
-- /Note:/ Consider using 'licenseModel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridLicenseModel :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridLicenseModel = Lens.lens (licenseModel :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {licenseModel = a} :: RDSInstanceDetails)
{-# DEPRECATED ridLicenseModel "Use generic-lens or generic-optics with 'licenseModel' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridSizeFlexEligible :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Bool)
ridSizeFlexEligible = Lens.lens (sizeFlexEligible :: RDSInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {sizeFlexEligible = a} :: RDSInstanceDetails)
{-# DEPRECATED ridSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridRegion :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridRegion = Lens.lens (region :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: RDSInstanceDetails)
{-# DEPRECATED ridRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The database engine that the recommended reservation supports.
--
-- /Note:/ Consider using 'databaseEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridDatabaseEngine :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridDatabaseEngine = Lens.lens (databaseEngine :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {databaseEngine = a} :: RDSInstanceDetails)
{-# DEPRECATED ridDatabaseEngine "Use generic-lens or generic-optics with 'databaseEngine' instead." #-}

-- | The database edition that the recommended reservation supports.
--
-- /Note:/ Consider using 'databaseEdition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridDatabaseEdition :: Lens.Lens' RDSInstanceDetails (Lude.Maybe Lude.Text)
ridDatabaseEdition = Lens.lens (databaseEdition :: RDSInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {databaseEdition = a} :: RDSInstanceDetails)
{-# DEPRECATED ridDatabaseEdition "Use generic-lens or generic-optics with 'databaseEdition' instead." #-}

instance Lude.FromJSON RDSInstanceDetails where
  parseJSON =
    Lude.withObject
      "RDSInstanceDetails"
      ( \x ->
          RDSInstanceDetails'
            Lude.<$> (x Lude..:? "CurrentGeneration")
            Lude.<*> (x Lude..:? "DeploymentOption")
            Lude.<*> (x Lude..:? "Family")
            Lude.<*> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "LicenseModel")
            Lude.<*> (x Lude..:? "SizeFlexEligible")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "DatabaseEngine")
            Lude.<*> (x Lude..:? "DatabaseEdition")
      )
