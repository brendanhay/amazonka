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
    pvAcceleratorType,
    pvInitialVariantWeight,
    pvVariantName,
    pvModelName,
    pvInitialInstanceCount,
    pvInstanceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType

-- | Identifies a model that you want to host and the resources to deploy for hosting it. If you are deploying multiple models, tell Amazon SageMaker how to distribute traffic among the models by specifying variant weights.
--
-- /See:/ 'mkProductionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { acceleratorType ::
      Lude.Maybe ProductionVariantAcceleratorType,
    initialVariantWeight :: Lude.Maybe Lude.Double,
    variantName :: Lude.Text,
    modelName :: Lude.Text,
    initialInstanceCount :: Lude.Natural,
    instanceType :: ProductionVariantInstanceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProductionVariant' with the minimum fields required to make a request.
--
-- * 'acceleratorType' - The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
-- * 'initialInstanceCount' - Number of instances to launch initially.
-- * 'initialVariantWeight' - Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
-- * 'instanceType' - The ML compute instance type.
-- * 'modelName' - The name of the model that you want to host. This is the name that you specified when creating the model.
-- * 'variantName' - The name of the production variant.
mkProductionVariant ::
  -- | 'variantName'
  Lude.Text ->
  -- | 'modelName'
  Lude.Text ->
  -- | 'initialInstanceCount'
  Lude.Natural ->
  -- | 'instanceType'
  ProductionVariantInstanceType ->
  ProductionVariant
mkProductionVariant
  pVariantName_
  pModelName_
  pInitialInstanceCount_
  pInstanceType_ =
    ProductionVariant'
      { acceleratorType = Lude.Nothing,
        initialVariantWeight = Lude.Nothing,
        variantName = pVariantName_,
        modelName = pModelName_,
        initialInstanceCount = pInitialInstanceCount_,
        instanceType = pInstanceType_
      }

-- | The size of the Elastic Inference (EI) instance to use for the production variant. EI instances provide on-demand GPU computing for inference. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvAcceleratorType :: Lens.Lens' ProductionVariant (Lude.Maybe ProductionVariantAcceleratorType)
pvAcceleratorType = Lens.lens (acceleratorType :: ProductionVariant -> Lude.Maybe ProductionVariantAcceleratorType) (\s a -> s {acceleratorType = a} :: ProductionVariant)
{-# DEPRECATED pvAcceleratorType "Use generic-lens or generic-optics with 'acceleratorType' instead." #-}

-- | Determines initial traffic distribution among all of the models that you specify in the endpoint configuration. The traffic to a production variant is determined by the ratio of the @VariantWeight@ to the sum of all @VariantWeight@ values across all ProductionVariants. If unspecified, it defaults to 1.0.
--
-- /Note:/ Consider using 'initialVariantWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvInitialVariantWeight :: Lens.Lens' ProductionVariant (Lude.Maybe Lude.Double)
pvInitialVariantWeight = Lens.lens (initialVariantWeight :: ProductionVariant -> Lude.Maybe Lude.Double) (\s a -> s {initialVariantWeight = a} :: ProductionVariant)
{-# DEPRECATED pvInitialVariantWeight "Use generic-lens or generic-optics with 'initialVariantWeight' instead." #-}

-- | The name of the production variant.
--
-- /Note:/ Consider using 'variantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvVariantName :: Lens.Lens' ProductionVariant Lude.Text
pvVariantName = Lens.lens (variantName :: ProductionVariant -> Lude.Text) (\s a -> s {variantName = a} :: ProductionVariant)
{-# DEPRECATED pvVariantName "Use generic-lens or generic-optics with 'variantName' instead." #-}

-- | The name of the model that you want to host. This is the name that you specified when creating the model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvModelName :: Lens.Lens' ProductionVariant Lude.Text
pvModelName = Lens.lens (modelName :: ProductionVariant -> Lude.Text) (\s a -> s {modelName = a} :: ProductionVariant)
{-# DEPRECATED pvModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | Number of instances to launch initially.
--
-- /Note:/ Consider using 'initialInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvInitialInstanceCount :: Lens.Lens' ProductionVariant Lude.Natural
pvInitialInstanceCount = Lens.lens (initialInstanceCount :: ProductionVariant -> Lude.Natural) (\s a -> s {initialInstanceCount = a} :: ProductionVariant)
{-# DEPRECATED pvInitialInstanceCount "Use generic-lens or generic-optics with 'initialInstanceCount' instead." #-}

-- | The ML compute instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvInstanceType :: Lens.Lens' ProductionVariant ProductionVariantInstanceType
pvInstanceType = Lens.lens (instanceType :: ProductionVariant -> ProductionVariantInstanceType) (\s a -> s {instanceType = a} :: ProductionVariant)
{-# DEPRECATED pvInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

instance Lude.FromJSON ProductionVariant where
  parseJSON =
    Lude.withObject
      "ProductionVariant"
      ( \x ->
          ProductionVariant'
            Lude.<$> (x Lude..:? "AcceleratorType")
            Lude.<*> (x Lude..:? "InitialVariantWeight")
            Lude.<*> (x Lude..: "VariantName")
            Lude.<*> (x Lude..: "ModelName")
            Lude.<*> (x Lude..: "InitialInstanceCount")
            Lude.<*> (x Lude..: "InstanceType")
      )

instance Lude.ToJSON ProductionVariant where
  toJSON ProductionVariant' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceleratorType" Lude..=) Lude.<$> acceleratorType,
            ("InitialVariantWeight" Lude..=) Lude.<$> initialVariantWeight,
            Lude.Just ("VariantName" Lude..= variantName),
            Lude.Just ("ModelName" Lude..= modelName),
            Lude.Just ("InitialInstanceCount" Lude..= initialInstanceCount),
            Lude.Just ("InstanceType" Lude..= instanceType)
          ]
      )
