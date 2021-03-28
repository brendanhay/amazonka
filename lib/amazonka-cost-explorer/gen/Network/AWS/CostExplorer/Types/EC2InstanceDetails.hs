{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.EC2InstanceDetails
  ( EC2InstanceDetails (..)
  -- * Smart constructor
  , mkEC2InstanceDetails
  -- * Lenses
  , ecidAvailabilityZone
  , ecidCurrentGeneration
  , ecidFamily
  , ecidInstanceType
  , ecidPlatform
  , ecidRegion
  , ecidSizeFlexEligible
  , ecidTenancy
  ) where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the Amazon EC2 instances that AWS recommends that you purchase.
--
-- /See:/ 'mkEC2InstanceDetails' smart constructor.
data EC2InstanceDetails = EC2InstanceDetails'
  { availabilityZone :: Core.Maybe Types.GenericString
    -- ^ The Availability Zone of the recommended reservation.
  , currentGeneration :: Core.Maybe Core.Bool
    -- ^ Whether the recommendation is for a current-generation instance. 
  , family :: Core.Maybe Types.GenericString
    -- ^ The instance family of the recommended reservation.
  , instanceType :: Core.Maybe Types.GenericString
    -- ^ The type of instance that AWS recommends.
  , platform :: Core.Maybe Types.GenericString
    -- ^ The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
  , region :: Core.Maybe Types.GenericString
    -- ^ The AWS Region of the recommended reservation.
  , sizeFlexEligible :: Core.Maybe Core.Bool
    -- ^ Whether the recommended reservation is size flexible.
  , tenancy :: Core.Maybe Types.GenericString
    -- ^ Whether the recommended reservation is dedicated or shared.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2InstanceDetails' value with any optional fields omitted.
mkEC2InstanceDetails
    :: EC2InstanceDetails
mkEC2InstanceDetails
  = EC2InstanceDetails'{availabilityZone = Core.Nothing,
                        currentGeneration = Core.Nothing, family = Core.Nothing,
                        instanceType = Core.Nothing, platform = Core.Nothing,
                        region = Core.Nothing, sizeFlexEligible = Core.Nothing,
                        tenancy = Core.Nothing}

-- | The Availability Zone of the recommended reservation.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidAvailabilityZone :: Lens.Lens' EC2InstanceDetails (Core.Maybe Types.GenericString)
ecidAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ecidAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | Whether the recommendation is for a current-generation instance. 
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidCurrentGeneration :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Bool)
ecidCurrentGeneration = Lens.field @"currentGeneration"
{-# INLINEABLE ecidCurrentGeneration #-}
{-# DEPRECATED currentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead"  #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidFamily :: Lens.Lens' EC2InstanceDetails (Core.Maybe Types.GenericString)
ecidFamily = Lens.field @"family"
{-# INLINEABLE ecidFamily #-}
{-# DEPRECATED family "Use generic-lens or generic-optics with 'family' instead"  #-}

-- | The type of instance that AWS recommends.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidInstanceType :: Lens.Lens' EC2InstanceDetails (Core.Maybe Types.GenericString)
ecidInstanceType = Lens.field @"instanceType"
{-# INLINEABLE ecidInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The platform of the recommended reservation. The platform is the specific combination of operating system, license model, and software on an instance.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidPlatform :: Lens.Lens' EC2InstanceDetails (Core.Maybe Types.GenericString)
ecidPlatform = Lens.field @"platform"
{-# INLINEABLE ecidPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidRegion :: Lens.Lens' EC2InstanceDetails (Core.Maybe Types.GenericString)
ecidRegion = Lens.field @"region"
{-# INLINEABLE ecidRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidSizeFlexEligible :: Lens.Lens' EC2InstanceDetails (Core.Maybe Core.Bool)
ecidSizeFlexEligible = Lens.field @"sizeFlexEligible"
{-# INLINEABLE ecidSizeFlexEligible #-}
{-# DEPRECATED sizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead"  #-}

-- | Whether the recommended reservation is dedicated or shared.
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidTenancy :: Lens.Lens' EC2InstanceDetails (Core.Maybe Types.GenericString)
ecidTenancy = Lens.field @"tenancy"
{-# INLINEABLE ecidTenancy #-}
{-# DEPRECATED tenancy "Use generic-lens or generic-optics with 'tenancy' instead"  #-}

instance Core.FromJSON EC2InstanceDetails where
        parseJSON
          = Core.withObject "EC2InstanceDetails" Core.$
              \ x ->
                EC2InstanceDetails' Core.<$>
                  (x Core..:? "AvailabilityZone") Core.<*>
                    x Core..:? "CurrentGeneration"
                    Core.<*> x Core..:? "Family"
                    Core.<*> x Core..:? "InstanceType"
                    Core.<*> x Core..:? "Platform"
                    Core.<*> x Core..:? "Region"
                    Core.<*> x Core..:? "SizeFlexEligible"
                    Core.<*> x Core..:? "Tenancy"
