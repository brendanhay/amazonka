{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProductionVariantSummary
  ( ProductionVariantSummary (..)
  -- * Smart constructor
  , mkProductionVariantSummary
  -- * Lenses
  , pvsVariantName
  , pvsCurrentInstanceCount
  , pvsCurrentWeight
  , pvsDeployedImages
  , pvsDesiredInstanceCount
  , pvsDesiredWeight
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DeployedImage as Types
import qualified Network.AWS.SageMaker.Types.VariantName as Types

-- | Describes weight and capacities for a production variant associated with an endpoint. If you sent a request to the @UpdateEndpointWeightsAndCapacities@ API and the endpoint status is @Updating@ , you get different desired and current values. 
--
-- /See:/ 'mkProductionVariantSummary' smart constructor.
data ProductionVariantSummary = ProductionVariantSummary'
  { variantName :: Types.VariantName
    -- ^ The name of the variant.
  , currentInstanceCount :: Core.Maybe Core.Natural
    -- ^ The number of instances associated with the variant.
  , currentWeight :: Core.Maybe Core.Double
    -- ^ The weight associated with the variant.
  , deployedImages :: Core.Maybe [Types.DeployedImage]
    -- ^ An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
  , desiredInstanceCount :: Core.Maybe Core.Natural
    -- ^ The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request. 
  , desiredWeight :: Core.Maybe Core.Double
    -- ^ The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProductionVariantSummary' value with any optional fields omitted.
mkProductionVariantSummary
    :: Types.VariantName -- ^ 'variantName'
    -> ProductionVariantSummary
mkProductionVariantSummary variantName
  = ProductionVariantSummary'{variantName,
                              currentInstanceCount = Core.Nothing, currentWeight = Core.Nothing,
                              deployedImages = Core.Nothing, desiredInstanceCount = Core.Nothing,
                              desiredWeight = Core.Nothing}

-- | The name of the variant.
--
-- /Note:/ Consider using 'variantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsVariantName :: Lens.Lens' ProductionVariantSummary Types.VariantName
pvsVariantName = Lens.field @"variantName"
{-# INLINEABLE pvsVariantName #-}
{-# DEPRECATED variantName "Use generic-lens or generic-optics with 'variantName' instead"  #-}

-- | The number of instances associated with the variant.
--
-- /Note:/ Consider using 'currentInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsCurrentInstanceCount :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Natural)
pvsCurrentInstanceCount = Lens.field @"currentInstanceCount"
{-# INLINEABLE pvsCurrentInstanceCount #-}
{-# DEPRECATED currentInstanceCount "Use generic-lens or generic-optics with 'currentInstanceCount' instead"  #-}

-- | The weight associated with the variant.
--
-- /Note:/ Consider using 'currentWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsCurrentWeight :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Double)
pvsCurrentWeight = Lens.field @"currentWeight"
{-# INLINEABLE pvsCurrentWeight #-}
{-# DEPRECATED currentWeight "Use generic-lens or generic-optics with 'currentWeight' instead"  #-}

-- | An array of @DeployedImage@ objects that specify the Amazon EC2 Container Registry paths of the inference images deployed on instances of this @ProductionVariant@ .
--
-- /Note:/ Consider using 'deployedImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDeployedImages :: Lens.Lens' ProductionVariantSummary (Core.Maybe [Types.DeployedImage])
pvsDeployedImages = Lens.field @"deployedImages"
{-# INLINEABLE pvsDeployedImages #-}
{-# DEPRECATED deployedImages "Use generic-lens or generic-optics with 'deployedImages' instead"  #-}

-- | The number of instances requested in the @UpdateEndpointWeightsAndCapacities@ request. 
--
-- /Note:/ Consider using 'desiredInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDesiredInstanceCount :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Natural)
pvsDesiredInstanceCount = Lens.field @"desiredInstanceCount"
{-# INLINEABLE pvsDesiredInstanceCount #-}
{-# DEPRECATED desiredInstanceCount "Use generic-lens or generic-optics with 'desiredInstanceCount' instead"  #-}

-- | The requested weight, as specified in the @UpdateEndpointWeightsAndCapacities@ request. 
--
-- /Note:/ Consider using 'desiredWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDesiredWeight :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Double)
pvsDesiredWeight = Lens.field @"desiredWeight"
{-# INLINEABLE pvsDesiredWeight #-}
{-# DEPRECATED desiredWeight "Use generic-lens or generic-optics with 'desiredWeight' instead"  #-}

instance Core.FromJSON ProductionVariantSummary where
        parseJSON
          = Core.withObject "ProductionVariantSummary" Core.$
              \ x ->
                ProductionVariantSummary' Core.<$>
                  (x Core..: "VariantName") Core.<*>
                    x Core..:? "CurrentInstanceCount"
                    Core.<*> x Core..:? "CurrentWeight"
                    Core.<*> x Core..:? "DeployedImages"
                    Core.<*> x Core..:? "DesiredInstanceCount"
                    Core.<*> x Core..:? "DesiredWeight"
