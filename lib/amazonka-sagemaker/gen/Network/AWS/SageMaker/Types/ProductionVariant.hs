{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariant
  ( ProductionVariant (..),

    -- * Smart constructor
    mkProductionVariant,

    -- * Lenses
    pvVariantName,
    pvModelName,
    pvInitialInstanceCount,
    pvInstanceType,
    pvAcceleratorType,
    pvInitialVariantWeight,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ModelName as Types
import qualified Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType as Types
import qualified Network.AWS.SageMaker.Types.ProductionVariantInstanceType as Types
import qualified Network.AWS.SageMaker.Types.VariantName as Types

-- | Identifies a model that you want to host and the resources to deploy for hosting it. If you are deploying multiple models, tell Amazon SageMaker how to distribute traffic among the models by specifying variant weights.
--
-- /See:/ 'mkProductionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { -- | The name of the production variant.
    variantName :: Types.VariantName,
    -- | The name of the model that you want to host. This is the name that you specified when creating the model.
    modelName :: Types.ModelName,
    -- | Number of instances to launch initially.
    initialInstanceCount :: Core.Natural,
    -- | The ML compute instance type.
    instanceType :: Types.ProductionVariantInstanceType,
    -- | The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
    acceleratorType :: Core.Maybe Types.ProductionVariantAcceleratorType,
    -- | Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
    initialVariantWeight :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProductionVariant' value with any optional fields omitted.
mkProductionVariant ::
  -- | 'variantName'
  Types.VariantName ->
  -- | 'modelName'
  Types.ModelName ->
  -- | 'initialInstanceCount'
  Core.Natural ->
  -- | 'instanceType'
  Types.ProductionVariantInstanceType ->
  ProductionVariant
mkProductionVariant
  variantName
  modelName
  initialInstanceCount
  instanceType =
    ProductionVariant'
      { variantName,
        modelName,
        initialInstanceCount,
        instanceType,
        acceleratorType = Core.Nothing,
        initialVariantWeight = Core.Nothing
      }

-- | The name of the production variant.
--
-- /Note:/ Consider using 'variantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvVariantName :: Lens.Lens' ProductionVariant Types.VariantName
pvVariantName = Lens.field @"variantName"
{-# DEPRECATED pvVariantName "Use generic-lens or generic-optics with 'variantName' instead." #-}

-- | The name of the model that you want to host. This is the name that you specified when creating the model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvModelName :: Lens.Lens' ProductionVariant Types.ModelName
pvModelName = Lens.field @"modelName"
{-# DEPRECATED pvModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Number of instances to launch initially.
--
-- /Note:/ Consider using 'initialInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvInitialInstanceCount :: Lens.Lens' ProductionVariant Core.Natural
pvInitialInstanceCount = Lens.field @"initialInstanceCount"
{-# DEPRECATED pvInitialInstanceCount "Use generic-lens or generic-optics with 'initialInstanceCount' instead." #-}

-- | The ML compute instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvInstanceType :: Lens.Lens' ProductionVariant Types.ProductionVariantInstanceType
pvInstanceType = Lens.field @"instanceType"
{-# DEPRECATED pvInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvAcceleratorType :: Lens.Lens' ProductionVariant (Core.Maybe Types.ProductionVariantAcceleratorType)
pvAcceleratorType = Lens.field @"acceleratorType"
{-# DEPRECATED pvAcceleratorType "Use generic-lens or generic-optics with 'acceleratorType' instead." #-}

-- | Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
--
-- /Note:/ Consider using 'initialVariantWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvInitialVariantWeight :: Lens.Lens' ProductionVariant (Core.Maybe Core.Double)
pvInitialVariantWeight = Lens.field @"initialVariantWeight"
{-# DEPRECATED pvInitialVariantWeight "Use generic-lens or generic-optics with 'initialVariantWeight' instead." #-}

instance Core.FromJSON ProductionVariant where
  toJSON ProductionVariant {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VariantName" Core..= variantName),
            Core.Just ("ModelName" Core..= modelName),
            Core.Just ("InitialInstanceCount" Core..= initialInstanceCount),
            Core.Just ("InstanceType" Core..= instanceType),
            ("AcceleratorType" Core..=) Core.<$> acceleratorType,
            ("InitialVariantWeight" Core..=) Core.<$> initialVariantWeight
          ]
      )

instance Core.FromJSON ProductionVariant where
  parseJSON =
    Core.withObject "ProductionVariant" Core.$
      \x ->
        ProductionVariant'
          Core.<$> (x Core..: "VariantName")
          Core.<*> (x Core..: "ModelName")
          Core.<*> (x Core..: "InitialInstanceCount")
          Core.<*> (x Core..: "InstanceType")
          Core.<*> (x Core..:? "AcceleratorType")
          Core.<*> (x Core..:? "InitialVariantWeight")
