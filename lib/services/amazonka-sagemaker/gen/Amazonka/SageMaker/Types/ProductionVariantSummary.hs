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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DeployedImage
import Amazonka.SageMaker.Types.ProductionVariantServerlessConfig
import Amazonka.SageMaker.Types.ProductionVariantStatus

-- | Describes weight and capacities for a production variant associated with
-- an endpoint. If you sent a request to the
-- @UpdateEndpointWeightsAndCapacities@ API and the endpoint status is
-- @Updating@, you get different desired and current values.
--
-- /See:/ 'newProductionVariantSummary' smart constructor.
data ProductionVariantSummary = ProductionVariantSummary'
  { -- | The serverless configuration requested for the endpoint update.
    desiredServerlessConfig :: Prelude.Maybe ProductionVariantServerlessConfig,
    -- | The requested weight, as specified in the
    -- @UpdateEndpointWeightsAndCapacities@ request.
    desiredWeight :: Prelude.Maybe Prelude.Double,
    -- | The serverless configuration for the endpoint.
    currentServerlessConfig :: Prelude.Maybe ProductionVariantServerlessConfig,
    -- | The endpoint variant status which describes the current deployment stage
    -- status or operational status.
    variantStatus :: Prelude.Maybe [ProductionVariantStatus],
    -- | The number of instances requested in the
    -- @UpdateEndpointWeightsAndCapacities@ request.
    desiredInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The weight associated with the variant.
    currentWeight :: Prelude.Maybe Prelude.Double,
    -- | An array of @DeployedImage@ objects that specify the Amazon EC2
    -- Container Registry paths of the inference images deployed on instances
    -- of this @ProductionVariant@.
    deployedImages :: Prelude.Maybe [DeployedImage],
    -- | The number of instances associated with the variant.
    currentInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The name of the variant.
    variantName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionVariantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredServerlessConfig', 'productionVariantSummary_desiredServerlessConfig' - The serverless configuration requested for the endpoint update.
--
-- 'desiredWeight', 'productionVariantSummary_desiredWeight' - The requested weight, as specified in the
-- @UpdateEndpointWeightsAndCapacities@ request.
--
-- 'currentServerlessConfig', 'productionVariantSummary_currentServerlessConfig' - The serverless configuration for the endpoint.
--
-- 'variantStatus', 'productionVariantSummary_variantStatus' - The endpoint variant status which describes the current deployment stage
-- status or operational status.
--
-- 'desiredInstanceCount', 'productionVariantSummary_desiredInstanceCount' - The number of instances requested in the
-- @UpdateEndpointWeightsAndCapacities@ request.
--
-- 'currentWeight', 'productionVariantSummary_currentWeight' - The weight associated with the variant.
--
-- 'deployedImages', 'productionVariantSummary_deployedImages' - An array of @DeployedImage@ objects that specify the Amazon EC2
-- Container Registry paths of the inference images deployed on instances
-- of this @ProductionVariant@.
--
-- 'currentInstanceCount', 'productionVariantSummary_currentInstanceCount' - The number of instances associated with the variant.
--
-- 'variantName', 'productionVariantSummary_variantName' - The name of the variant.
newProductionVariantSummary ::
  -- | 'variantName'
  Prelude.Text ->
  ProductionVariantSummary
newProductionVariantSummary pVariantName_ =
  ProductionVariantSummary'
    { desiredServerlessConfig =
        Prelude.Nothing,
      desiredWeight = Prelude.Nothing,
      currentServerlessConfig = Prelude.Nothing,
      variantStatus = Prelude.Nothing,
      desiredInstanceCount = Prelude.Nothing,
      currentWeight = Prelude.Nothing,
      deployedImages = Prelude.Nothing,
      currentInstanceCount = Prelude.Nothing,
      variantName = pVariantName_
    }

-- | The serverless configuration requested for the endpoint update.
productionVariantSummary_desiredServerlessConfig :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe ProductionVariantServerlessConfig)
productionVariantSummary_desiredServerlessConfig = Lens.lens (\ProductionVariantSummary' {desiredServerlessConfig} -> desiredServerlessConfig) (\s@ProductionVariantSummary' {} a -> s {desiredServerlessConfig = a} :: ProductionVariantSummary)

-- | The requested weight, as specified in the
-- @UpdateEndpointWeightsAndCapacities@ request.
productionVariantSummary_desiredWeight :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe Prelude.Double)
productionVariantSummary_desiredWeight = Lens.lens (\ProductionVariantSummary' {desiredWeight} -> desiredWeight) (\s@ProductionVariantSummary' {} a -> s {desiredWeight = a} :: ProductionVariantSummary)

-- | The serverless configuration for the endpoint.
productionVariantSummary_currentServerlessConfig :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe ProductionVariantServerlessConfig)
productionVariantSummary_currentServerlessConfig = Lens.lens (\ProductionVariantSummary' {currentServerlessConfig} -> currentServerlessConfig) (\s@ProductionVariantSummary' {} a -> s {currentServerlessConfig = a} :: ProductionVariantSummary)

-- | The endpoint variant status which describes the current deployment stage
-- status or operational status.
productionVariantSummary_variantStatus :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe [ProductionVariantStatus])
productionVariantSummary_variantStatus = Lens.lens (\ProductionVariantSummary' {variantStatus} -> variantStatus) (\s@ProductionVariantSummary' {} a -> s {variantStatus = a} :: ProductionVariantSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of instances requested in the
-- @UpdateEndpointWeightsAndCapacities@ request.
productionVariantSummary_desiredInstanceCount :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe Prelude.Natural)
productionVariantSummary_desiredInstanceCount = Lens.lens (\ProductionVariantSummary' {desiredInstanceCount} -> desiredInstanceCount) (\s@ProductionVariantSummary' {} a -> s {desiredInstanceCount = a} :: ProductionVariantSummary)

-- | The weight associated with the variant.
productionVariantSummary_currentWeight :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe Prelude.Double)
productionVariantSummary_currentWeight = Lens.lens (\ProductionVariantSummary' {currentWeight} -> currentWeight) (\s@ProductionVariantSummary' {} a -> s {currentWeight = a} :: ProductionVariantSummary)

-- | An array of @DeployedImage@ objects that specify the Amazon EC2
-- Container Registry paths of the inference images deployed on instances
-- of this @ProductionVariant@.
productionVariantSummary_deployedImages :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe [DeployedImage])
productionVariantSummary_deployedImages = Lens.lens (\ProductionVariantSummary' {deployedImages} -> deployedImages) (\s@ProductionVariantSummary' {} a -> s {deployedImages = a} :: ProductionVariantSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of instances associated with the variant.
productionVariantSummary_currentInstanceCount :: Lens.Lens' ProductionVariantSummary (Prelude.Maybe Prelude.Natural)
productionVariantSummary_currentInstanceCount = Lens.lens (\ProductionVariantSummary' {currentInstanceCount} -> currentInstanceCount) (\s@ProductionVariantSummary' {} a -> s {currentInstanceCount = a} :: ProductionVariantSummary)

-- | The name of the variant.
productionVariantSummary_variantName :: Lens.Lens' ProductionVariantSummary Prelude.Text
productionVariantSummary_variantName = Lens.lens (\ProductionVariantSummary' {variantName} -> variantName) (\s@ProductionVariantSummary' {} a -> s {variantName = a} :: ProductionVariantSummary)

instance Data.FromJSON ProductionVariantSummary where
  parseJSON =
    Data.withObject
      "ProductionVariantSummary"
      ( \x ->
          ProductionVariantSummary'
            Prelude.<$> (x Data..:? "DesiredServerlessConfig")
            Prelude.<*> (x Data..:? "DesiredWeight")
            Prelude.<*> (x Data..:? "CurrentServerlessConfig")
            Prelude.<*> (x Data..:? "VariantStatus" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DesiredInstanceCount")
            Prelude.<*> (x Data..:? "CurrentWeight")
            Prelude.<*> (x Data..:? "DeployedImages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CurrentInstanceCount")
            Prelude.<*> (x Data..: "VariantName")
      )

instance Prelude.Hashable ProductionVariantSummary where
  hashWithSalt _salt ProductionVariantSummary' {..} =
    _salt
      `Prelude.hashWithSalt` desiredServerlessConfig
      `Prelude.hashWithSalt` desiredWeight
      `Prelude.hashWithSalt` currentServerlessConfig
      `Prelude.hashWithSalt` variantStatus
      `Prelude.hashWithSalt` desiredInstanceCount
      `Prelude.hashWithSalt` currentWeight
      `Prelude.hashWithSalt` deployedImages
      `Prelude.hashWithSalt` currentInstanceCount
      `Prelude.hashWithSalt` variantName

instance Prelude.NFData ProductionVariantSummary where
  rnf ProductionVariantSummary' {..} =
    Prelude.rnf desiredServerlessConfig
      `Prelude.seq` Prelude.rnf desiredWeight
      `Prelude.seq` Prelude.rnf currentServerlessConfig
      `Prelude.seq` Prelude.rnf variantStatus
      `Prelude.seq` Prelude.rnf desiredInstanceCount
      `Prelude.seq` Prelude.rnf currentWeight
      `Prelude.seq` Prelude.rnf deployedImages
      `Prelude.seq` Prelude.rnf currentInstanceCount
      `Prelude.seq` Prelude.rnf variantName
