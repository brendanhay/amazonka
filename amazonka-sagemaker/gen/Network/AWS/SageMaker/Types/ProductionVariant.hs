{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariant where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
import Network.AWS.SageMaker.Types.ProductionVariantCoreDumpConfig
import Network.AWS.SageMaker.Types.ProductionVariantInstanceType

-- | Identifies a model that you want to host and the resources to deploy for
-- hosting it. If you are deploying multiple models, tell Amazon SageMaker
-- how to distribute traffic among the models by specifying variant
-- weights.
--
-- /See:/ 'newProductionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { -- | Determines initial traffic distribution among all of the models that you
    -- specify in the endpoint configuration. The traffic to a production
    -- variant is determined by the ratio of the @VariantWeight@ to the sum of
    -- all @VariantWeight@ values across all ProductionVariants. If
    -- unspecified, it defaults to 1.0.
    initialVariantWeight :: Core.Maybe Core.Double,
    -- | The size of the Elastic Inference (EI) instance to use for the
    -- production variant. EI instances provide on-demand GPU computing for
    -- inference. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorType :: Core.Maybe ProductionVariantAcceleratorType,
    -- | Specifies configuration for a core dump from the model container when
    -- the process crashes.
    coreDumpConfig :: Core.Maybe ProductionVariantCoreDumpConfig,
    -- | The name of the production variant.
    variantName :: Core.Text,
    -- | The name of the model that you want to host. This is the name that you
    -- specified when creating the model.
    modelName :: Core.Text,
    -- | Number of instances to launch initially.
    initialInstanceCount :: Core.Natural,
    -- | The ML compute instance type.
    instanceType :: ProductionVariantInstanceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProductionVariant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialVariantWeight', 'productionVariant_initialVariantWeight' - Determines initial traffic distribution among all of the models that you
-- specify in the endpoint configuration. The traffic to a production
-- variant is determined by the ratio of the @VariantWeight@ to the sum of
-- all @VariantWeight@ values across all ProductionVariants. If
-- unspecified, it defaults to 1.0.
--
-- 'acceleratorType', 'productionVariant_acceleratorType' - The size of the Elastic Inference (EI) instance to use for the
-- production variant. EI instances provide on-demand GPU computing for
-- inference. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
--
-- 'coreDumpConfig', 'productionVariant_coreDumpConfig' - Specifies configuration for a core dump from the model container when
-- the process crashes.
--
-- 'variantName', 'productionVariant_variantName' - The name of the production variant.
--
-- 'modelName', 'productionVariant_modelName' - The name of the model that you want to host. This is the name that you
-- specified when creating the model.
--
-- 'initialInstanceCount', 'productionVariant_initialInstanceCount' - Number of instances to launch initially.
--
-- 'instanceType', 'productionVariant_instanceType' - The ML compute instance type.
newProductionVariant ::
  -- | 'variantName'
  Core.Text ->
  -- | 'modelName'
  Core.Text ->
  -- | 'initialInstanceCount'
  Core.Natural ->
  -- | 'instanceType'
  ProductionVariantInstanceType ->
  ProductionVariant
newProductionVariant
  pVariantName_
  pModelName_
  pInitialInstanceCount_
  pInstanceType_ =
    ProductionVariant'
      { initialVariantWeight =
          Core.Nothing,
        acceleratorType = Core.Nothing,
        coreDumpConfig = Core.Nothing,
        variantName = pVariantName_,
        modelName = pModelName_,
        initialInstanceCount = pInitialInstanceCount_,
        instanceType = pInstanceType_
      }

-- | Determines initial traffic distribution among all of the models that you
-- specify in the endpoint configuration. The traffic to a production
-- variant is determined by the ratio of the @VariantWeight@ to the sum of
-- all @VariantWeight@ values across all ProductionVariants. If
-- unspecified, it defaults to 1.0.
productionVariant_initialVariantWeight :: Lens.Lens' ProductionVariant (Core.Maybe Core.Double)
productionVariant_initialVariantWeight = Lens.lens (\ProductionVariant' {initialVariantWeight} -> initialVariantWeight) (\s@ProductionVariant' {} a -> s {initialVariantWeight = a} :: ProductionVariant)

-- | The size of the Elastic Inference (EI) instance to use for the
-- production variant. EI instances provide on-demand GPU computing for
-- inference. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
productionVariant_acceleratorType :: Lens.Lens' ProductionVariant (Core.Maybe ProductionVariantAcceleratorType)
productionVariant_acceleratorType = Lens.lens (\ProductionVariant' {acceleratorType} -> acceleratorType) (\s@ProductionVariant' {} a -> s {acceleratorType = a} :: ProductionVariant)

-- | Specifies configuration for a core dump from the model container when
-- the process crashes.
productionVariant_coreDumpConfig :: Lens.Lens' ProductionVariant (Core.Maybe ProductionVariantCoreDumpConfig)
productionVariant_coreDumpConfig = Lens.lens (\ProductionVariant' {coreDumpConfig} -> coreDumpConfig) (\s@ProductionVariant' {} a -> s {coreDumpConfig = a} :: ProductionVariant)

-- | The name of the production variant.
productionVariant_variantName :: Lens.Lens' ProductionVariant Core.Text
productionVariant_variantName = Lens.lens (\ProductionVariant' {variantName} -> variantName) (\s@ProductionVariant' {} a -> s {variantName = a} :: ProductionVariant)

-- | The name of the model that you want to host. This is the name that you
-- specified when creating the model.
productionVariant_modelName :: Lens.Lens' ProductionVariant Core.Text
productionVariant_modelName = Lens.lens (\ProductionVariant' {modelName} -> modelName) (\s@ProductionVariant' {} a -> s {modelName = a} :: ProductionVariant)

-- | Number of instances to launch initially.
productionVariant_initialInstanceCount :: Lens.Lens' ProductionVariant Core.Natural
productionVariant_initialInstanceCount = Lens.lens (\ProductionVariant' {initialInstanceCount} -> initialInstanceCount) (\s@ProductionVariant' {} a -> s {initialInstanceCount = a} :: ProductionVariant)

-- | The ML compute instance type.
productionVariant_instanceType :: Lens.Lens' ProductionVariant ProductionVariantInstanceType
productionVariant_instanceType = Lens.lens (\ProductionVariant' {instanceType} -> instanceType) (\s@ProductionVariant' {} a -> s {instanceType = a} :: ProductionVariant)

instance Core.FromJSON ProductionVariant where
  parseJSON =
    Core.withObject
      "ProductionVariant"
      ( \x ->
          ProductionVariant'
            Core.<$> (x Core..:? "InitialVariantWeight")
            Core.<*> (x Core..:? "AcceleratorType")
            Core.<*> (x Core..:? "CoreDumpConfig")
            Core.<*> (x Core..: "VariantName")
            Core.<*> (x Core..: "ModelName")
            Core.<*> (x Core..: "InitialInstanceCount")
            Core.<*> (x Core..: "InstanceType")
      )

instance Core.Hashable ProductionVariant

instance Core.NFData ProductionVariant

instance Core.ToJSON ProductionVariant where
  toJSON ProductionVariant' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitialVariantWeight" Core..=)
              Core.<$> initialVariantWeight,
            ("AcceleratorType" Core..=) Core.<$> acceleratorType,
            ("CoreDumpConfig" Core..=) Core.<$> coreDumpConfig,
            Core.Just ("VariantName" Core..= variantName),
            Core.Just ("ModelName" Core..= modelName),
            Core.Just
              ( "InitialInstanceCount"
                  Core..= initialInstanceCount
              ),
            Core.Just ("InstanceType" Core..= instanceType)
          ]
      )
