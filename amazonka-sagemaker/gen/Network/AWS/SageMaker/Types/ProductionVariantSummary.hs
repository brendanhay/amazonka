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
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.DeployedImage

-- | Describes weight and capacities for a production variant associated with
-- an endpoint. If you sent a request to the
-- @UpdateEndpointWeightsAndCapacities@ API and the endpoint status is
-- @Updating@, you get different desired and current values.
--
-- /See:/ 'newProductionVariantSummary' smart constructor.
data ProductionVariantSummary = ProductionVariantSummary'
  { -- | An array of @DeployedImage@ objects that specify the Amazon EC2
    -- Container Registry paths of the inference images deployed on instances
    -- of this @ProductionVariant@.
    deployedImages :: Core.Maybe [DeployedImage],
    -- | The number of instances requested in the
    -- @UpdateEndpointWeightsAndCapacities@ request.
    desiredInstanceCount :: Core.Maybe Core.Natural,
    -- | The weight associated with the variant.
    currentWeight :: Core.Maybe Core.Double,
    -- | The number of instances associated with the variant.
    currentInstanceCount :: Core.Maybe Core.Natural,
    -- | The requested weight, as specified in the
    -- @UpdateEndpointWeightsAndCapacities@ request.
    desiredWeight :: Core.Maybe Core.Double,
    -- | The name of the variant.
    variantName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProductionVariantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployedImages', 'productionVariantSummary_deployedImages' - An array of @DeployedImage@ objects that specify the Amazon EC2
-- Container Registry paths of the inference images deployed on instances
-- of this @ProductionVariant@.
--
-- 'desiredInstanceCount', 'productionVariantSummary_desiredInstanceCount' - The number of instances requested in the
-- @UpdateEndpointWeightsAndCapacities@ request.
--
-- 'currentWeight', 'productionVariantSummary_currentWeight' - The weight associated with the variant.
--
-- 'currentInstanceCount', 'productionVariantSummary_currentInstanceCount' - The number of instances associated with the variant.
--
-- 'desiredWeight', 'productionVariantSummary_desiredWeight' - The requested weight, as specified in the
-- @UpdateEndpointWeightsAndCapacities@ request.
--
-- 'variantName', 'productionVariantSummary_variantName' - The name of the variant.
newProductionVariantSummary ::
  -- | 'variantName'
  Core.Text ->
  ProductionVariantSummary
newProductionVariantSummary pVariantName_ =
  ProductionVariantSummary'
    { deployedImages =
        Core.Nothing,
      desiredInstanceCount = Core.Nothing,
      currentWeight = Core.Nothing,
      currentInstanceCount = Core.Nothing,
      desiredWeight = Core.Nothing,
      variantName = pVariantName_
    }

-- | An array of @DeployedImage@ objects that specify the Amazon EC2
-- Container Registry paths of the inference images deployed on instances
-- of this @ProductionVariant@.
productionVariantSummary_deployedImages :: Lens.Lens' ProductionVariantSummary (Core.Maybe [DeployedImage])
productionVariantSummary_deployedImages = Lens.lens (\ProductionVariantSummary' {deployedImages} -> deployedImages) (\s@ProductionVariantSummary' {} a -> s {deployedImages = a} :: ProductionVariantSummary) Core.. Lens.mapping Lens._Coerce

-- | The number of instances requested in the
-- @UpdateEndpointWeightsAndCapacities@ request.
productionVariantSummary_desiredInstanceCount :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Natural)
productionVariantSummary_desiredInstanceCount = Lens.lens (\ProductionVariantSummary' {desiredInstanceCount} -> desiredInstanceCount) (\s@ProductionVariantSummary' {} a -> s {desiredInstanceCount = a} :: ProductionVariantSummary)

-- | The weight associated with the variant.
productionVariantSummary_currentWeight :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Double)
productionVariantSummary_currentWeight = Lens.lens (\ProductionVariantSummary' {currentWeight} -> currentWeight) (\s@ProductionVariantSummary' {} a -> s {currentWeight = a} :: ProductionVariantSummary)

-- | The number of instances associated with the variant.
productionVariantSummary_currentInstanceCount :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Natural)
productionVariantSummary_currentInstanceCount = Lens.lens (\ProductionVariantSummary' {currentInstanceCount} -> currentInstanceCount) (\s@ProductionVariantSummary' {} a -> s {currentInstanceCount = a} :: ProductionVariantSummary)

-- | The requested weight, as specified in the
-- @UpdateEndpointWeightsAndCapacities@ request.
productionVariantSummary_desiredWeight :: Lens.Lens' ProductionVariantSummary (Core.Maybe Core.Double)
productionVariantSummary_desiredWeight = Lens.lens (\ProductionVariantSummary' {desiredWeight} -> desiredWeight) (\s@ProductionVariantSummary' {} a -> s {desiredWeight = a} :: ProductionVariantSummary)

-- | The name of the variant.
productionVariantSummary_variantName :: Lens.Lens' ProductionVariantSummary Core.Text
productionVariantSummary_variantName = Lens.lens (\ProductionVariantSummary' {variantName} -> variantName) (\s@ProductionVariantSummary' {} a -> s {variantName = a} :: ProductionVariantSummary)

instance Core.FromJSON ProductionVariantSummary where
  parseJSON =
    Core.withObject
      "ProductionVariantSummary"
      ( \x ->
          ProductionVariantSummary'
            Core.<$> (x Core..:? "DeployedImages" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DesiredInstanceCount")
            Core.<*> (x Core..:? "CurrentWeight")
            Core.<*> (x Core..:? "CurrentInstanceCount")
            Core.<*> (x Core..:? "DesiredWeight")
            Core.<*> (x Core..: "VariantName")
      )

instance Core.Hashable ProductionVariantSummary

instance Core.NFData ProductionVariantSummary
