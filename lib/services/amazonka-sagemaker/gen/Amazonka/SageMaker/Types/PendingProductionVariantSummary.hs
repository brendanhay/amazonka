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
-- Module      : Amazonka.SageMaker.Types.PendingProductionVariantSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PendingProductionVariantSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DeployedImage
import Amazonka.SageMaker.Types.ProductionVariantAcceleratorType
import Amazonka.SageMaker.Types.ProductionVariantInstanceType
import Amazonka.SageMaker.Types.ProductionVariantServerlessConfig
import Amazonka.SageMaker.Types.ProductionVariantStatus

-- | The production variant summary for a deployment when an endpoint is
-- creating or updating with the @ @@CreateEndpoint@@ @ or
-- @ @@UpdateEndpoint@@ @ operations. Describes the @VariantStatus @,
-- weight and capacity for a production variant associated with an
-- endpoint.
--
-- /See:/ 'newPendingProductionVariantSummary' smart constructor.
data PendingProductionVariantSummary = PendingProductionVariantSummary'
  { -- | The size of the Elastic Inference (EI) instance to use for the
    -- production variant. EI instances provide on-demand GPU computing for
    -- inference. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorType :: Prelude.Maybe ProductionVariantAcceleratorType,
    -- | The number of instances associated with the variant.
    currentInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The serverless configuration for the endpoint.
    currentServerlessConfig :: Prelude.Maybe ProductionVariantServerlessConfig,
    -- | The weight associated with the variant.
    currentWeight :: Prelude.Maybe Prelude.Double,
    -- | An array of @DeployedImage@ objects that specify the Amazon EC2
    -- Container Registry paths of the inference images deployed on instances
    -- of this @ProductionVariant@.
    deployedImages :: Prelude.Maybe [DeployedImage],
    -- | The number of instances requested in this deployment, as specified in
    -- the endpoint configuration for the endpoint. The value is taken from the
    -- request to the @ @@CreateEndpointConfig@@ @ operation.
    desiredInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | The serverless configuration requested for this deployment, as specified
    -- in the endpoint configuration for the endpoint.
    desiredServerlessConfig :: Prelude.Maybe ProductionVariantServerlessConfig,
    -- | The requested weight for the variant in this deployment, as specified in
    -- the endpoint configuration for the endpoint. The value is taken from the
    -- request to the @ @@CreateEndpointConfig@@ @ operation.
    desiredWeight :: Prelude.Maybe Prelude.Double,
    -- | The type of instances associated with the variant.
    instanceType :: Prelude.Maybe ProductionVariantInstanceType,
    -- | The endpoint variant status which describes the current deployment stage
    -- status or operational status.
    variantStatus :: Prelude.Maybe [ProductionVariantStatus],
    -- | The name of the variant.
    variantName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingProductionVariantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorType', 'pendingProductionVariantSummary_acceleratorType' - The size of the Elastic Inference (EI) instance to use for the
-- production variant. EI instances provide on-demand GPU computing for
-- inference. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
--
-- 'currentInstanceCount', 'pendingProductionVariantSummary_currentInstanceCount' - The number of instances associated with the variant.
--
-- 'currentServerlessConfig', 'pendingProductionVariantSummary_currentServerlessConfig' - The serverless configuration for the endpoint.
--
-- 'currentWeight', 'pendingProductionVariantSummary_currentWeight' - The weight associated with the variant.
--
-- 'deployedImages', 'pendingProductionVariantSummary_deployedImages' - An array of @DeployedImage@ objects that specify the Amazon EC2
-- Container Registry paths of the inference images deployed on instances
-- of this @ProductionVariant@.
--
-- 'desiredInstanceCount', 'pendingProductionVariantSummary_desiredInstanceCount' - The number of instances requested in this deployment, as specified in
-- the endpoint configuration for the endpoint. The value is taken from the
-- request to the @ @@CreateEndpointConfig@@ @ operation.
--
-- 'desiredServerlessConfig', 'pendingProductionVariantSummary_desiredServerlessConfig' - The serverless configuration requested for this deployment, as specified
-- in the endpoint configuration for the endpoint.
--
-- 'desiredWeight', 'pendingProductionVariantSummary_desiredWeight' - The requested weight for the variant in this deployment, as specified in
-- the endpoint configuration for the endpoint. The value is taken from the
-- request to the @ @@CreateEndpointConfig@@ @ operation.
--
-- 'instanceType', 'pendingProductionVariantSummary_instanceType' - The type of instances associated with the variant.
--
-- 'variantStatus', 'pendingProductionVariantSummary_variantStatus' - The endpoint variant status which describes the current deployment stage
-- status or operational status.
--
-- 'variantName', 'pendingProductionVariantSummary_variantName' - The name of the variant.
newPendingProductionVariantSummary ::
  -- | 'variantName'
  Prelude.Text ->
  PendingProductionVariantSummary
newPendingProductionVariantSummary pVariantName_ =
  PendingProductionVariantSummary'
    { acceleratorType =
        Prelude.Nothing,
      currentInstanceCount = Prelude.Nothing,
      currentServerlessConfig = Prelude.Nothing,
      currentWeight = Prelude.Nothing,
      deployedImages = Prelude.Nothing,
      desiredInstanceCount = Prelude.Nothing,
      desiredServerlessConfig = Prelude.Nothing,
      desiredWeight = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      variantStatus = Prelude.Nothing,
      variantName = pVariantName_
    }

-- | The size of the Elastic Inference (EI) instance to use for the
-- production variant. EI instances provide on-demand GPU computing for
-- inference. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
pendingProductionVariantSummary_acceleratorType :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe ProductionVariantAcceleratorType)
pendingProductionVariantSummary_acceleratorType = Lens.lens (\PendingProductionVariantSummary' {acceleratorType} -> acceleratorType) (\s@PendingProductionVariantSummary' {} a -> s {acceleratorType = a} :: PendingProductionVariantSummary)

-- | The number of instances associated with the variant.
pendingProductionVariantSummary_currentInstanceCount :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe Prelude.Natural)
pendingProductionVariantSummary_currentInstanceCount = Lens.lens (\PendingProductionVariantSummary' {currentInstanceCount} -> currentInstanceCount) (\s@PendingProductionVariantSummary' {} a -> s {currentInstanceCount = a} :: PendingProductionVariantSummary)

-- | The serverless configuration for the endpoint.
pendingProductionVariantSummary_currentServerlessConfig :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe ProductionVariantServerlessConfig)
pendingProductionVariantSummary_currentServerlessConfig = Lens.lens (\PendingProductionVariantSummary' {currentServerlessConfig} -> currentServerlessConfig) (\s@PendingProductionVariantSummary' {} a -> s {currentServerlessConfig = a} :: PendingProductionVariantSummary)

-- | The weight associated with the variant.
pendingProductionVariantSummary_currentWeight :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe Prelude.Double)
pendingProductionVariantSummary_currentWeight = Lens.lens (\PendingProductionVariantSummary' {currentWeight} -> currentWeight) (\s@PendingProductionVariantSummary' {} a -> s {currentWeight = a} :: PendingProductionVariantSummary)

-- | An array of @DeployedImage@ objects that specify the Amazon EC2
-- Container Registry paths of the inference images deployed on instances
-- of this @ProductionVariant@.
pendingProductionVariantSummary_deployedImages :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe [DeployedImage])
pendingProductionVariantSummary_deployedImages = Lens.lens (\PendingProductionVariantSummary' {deployedImages} -> deployedImages) (\s@PendingProductionVariantSummary' {} a -> s {deployedImages = a} :: PendingProductionVariantSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of instances requested in this deployment, as specified in
-- the endpoint configuration for the endpoint. The value is taken from the
-- request to the @ @@CreateEndpointConfig@@ @ operation.
pendingProductionVariantSummary_desiredInstanceCount :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe Prelude.Natural)
pendingProductionVariantSummary_desiredInstanceCount = Lens.lens (\PendingProductionVariantSummary' {desiredInstanceCount} -> desiredInstanceCount) (\s@PendingProductionVariantSummary' {} a -> s {desiredInstanceCount = a} :: PendingProductionVariantSummary)

-- | The serverless configuration requested for this deployment, as specified
-- in the endpoint configuration for the endpoint.
pendingProductionVariantSummary_desiredServerlessConfig :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe ProductionVariantServerlessConfig)
pendingProductionVariantSummary_desiredServerlessConfig = Lens.lens (\PendingProductionVariantSummary' {desiredServerlessConfig} -> desiredServerlessConfig) (\s@PendingProductionVariantSummary' {} a -> s {desiredServerlessConfig = a} :: PendingProductionVariantSummary)

-- | The requested weight for the variant in this deployment, as specified in
-- the endpoint configuration for the endpoint. The value is taken from the
-- request to the @ @@CreateEndpointConfig@@ @ operation.
pendingProductionVariantSummary_desiredWeight :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe Prelude.Double)
pendingProductionVariantSummary_desiredWeight = Lens.lens (\PendingProductionVariantSummary' {desiredWeight} -> desiredWeight) (\s@PendingProductionVariantSummary' {} a -> s {desiredWeight = a} :: PendingProductionVariantSummary)

-- | The type of instances associated with the variant.
pendingProductionVariantSummary_instanceType :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe ProductionVariantInstanceType)
pendingProductionVariantSummary_instanceType = Lens.lens (\PendingProductionVariantSummary' {instanceType} -> instanceType) (\s@PendingProductionVariantSummary' {} a -> s {instanceType = a} :: PendingProductionVariantSummary)

-- | The endpoint variant status which describes the current deployment stage
-- status or operational status.
pendingProductionVariantSummary_variantStatus :: Lens.Lens' PendingProductionVariantSummary (Prelude.Maybe [ProductionVariantStatus])
pendingProductionVariantSummary_variantStatus = Lens.lens (\PendingProductionVariantSummary' {variantStatus} -> variantStatus) (\s@PendingProductionVariantSummary' {} a -> s {variantStatus = a} :: PendingProductionVariantSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the variant.
pendingProductionVariantSummary_variantName :: Lens.Lens' PendingProductionVariantSummary Prelude.Text
pendingProductionVariantSummary_variantName = Lens.lens (\PendingProductionVariantSummary' {variantName} -> variantName) (\s@PendingProductionVariantSummary' {} a -> s {variantName = a} :: PendingProductionVariantSummary)

instance
  Data.FromJSON
    PendingProductionVariantSummary
  where
  parseJSON =
    Data.withObject
      "PendingProductionVariantSummary"
      ( \x ->
          PendingProductionVariantSummary'
            Prelude.<$> (x Data..:? "AcceleratorType")
            Prelude.<*> (x Data..:? "CurrentInstanceCount")
            Prelude.<*> (x Data..:? "CurrentServerlessConfig")
            Prelude.<*> (x Data..:? "CurrentWeight")
            Prelude.<*> (x Data..:? "DeployedImages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DesiredInstanceCount")
            Prelude.<*> (x Data..:? "DesiredServerlessConfig")
            Prelude.<*> (x Data..:? "DesiredWeight")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "VariantStatus" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "VariantName")
      )

instance
  Prelude.Hashable
    PendingProductionVariantSummary
  where
  hashWithSalt
    _salt
    PendingProductionVariantSummary' {..} =
      _salt
        `Prelude.hashWithSalt` acceleratorType
        `Prelude.hashWithSalt` currentInstanceCount
        `Prelude.hashWithSalt` currentServerlessConfig
        `Prelude.hashWithSalt` currentWeight
        `Prelude.hashWithSalt` deployedImages
        `Prelude.hashWithSalt` desiredInstanceCount
        `Prelude.hashWithSalt` desiredServerlessConfig
        `Prelude.hashWithSalt` desiredWeight
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` variantStatus
        `Prelude.hashWithSalt` variantName

instance
  Prelude.NFData
    PendingProductionVariantSummary
  where
  rnf PendingProductionVariantSummary' {..} =
    Prelude.rnf acceleratorType
      `Prelude.seq` Prelude.rnf currentInstanceCount
      `Prelude.seq` Prelude.rnf currentServerlessConfig
      `Prelude.seq` Prelude.rnf currentWeight
      `Prelude.seq` Prelude.rnf deployedImages
      `Prelude.seq` Prelude.rnf desiredInstanceCount
      `Prelude.seq` Prelude.rnf desiredServerlessConfig
      `Prelude.seq` Prelude.rnf desiredWeight
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf variantStatus
      `Prelude.seq` Prelude.rnf variantName
