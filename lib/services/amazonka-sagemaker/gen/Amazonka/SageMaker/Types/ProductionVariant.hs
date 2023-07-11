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
-- Module      : Amazonka.SageMaker.Types.ProductionVariant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProductionVariantAcceleratorType
import Amazonka.SageMaker.Types.ProductionVariantCoreDumpConfig
import Amazonka.SageMaker.Types.ProductionVariantInstanceType
import Amazonka.SageMaker.Types.ProductionVariantServerlessConfig

-- | Identifies a model that you want to host and the resources chosen to
-- deploy for hosting it. If you are deploying multiple models, tell
-- SageMaker how to distribute traffic among the models by specifying
-- variant weights.
--
-- /See:/ 'newProductionVariant' smart constructor.
data ProductionVariant = ProductionVariant'
  { -- | The size of the Elastic Inference (EI) instance to use for the
    -- production variant. EI instances provide on-demand GPU computing for
    -- inference. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorType :: Prelude.Maybe ProductionVariantAcceleratorType,
    -- | The timeout value, in seconds, for your inference container to pass
    -- health check by SageMaker Hosting. For more information about health
    -- check, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-inference-code.html#your-algorithms-inference-algo-ping-requests How Your Container Should Respond to Health Check (Ping) Requests>.
    containerStartupHealthCheckTimeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Specifies configuration for a core dump from the model container when
    -- the process crashes.
    coreDumpConfig :: Prelude.Maybe ProductionVariantCoreDumpConfig,
    -- | Number of instances to launch initially.
    initialInstanceCount :: Prelude.Maybe Prelude.Natural,
    -- | Determines initial traffic distribution among all of the models that you
    -- specify in the endpoint configuration. The traffic to a production
    -- variant is determined by the ratio of the @VariantWeight@ to the sum of
    -- all @VariantWeight@ values across all ProductionVariants. If
    -- unspecified, it defaults to 1.0.
    initialVariantWeight :: Prelude.Maybe Prelude.Double,
    -- | The ML compute instance type.
    instanceType :: Prelude.Maybe ProductionVariantInstanceType,
    -- | The timeout value, in seconds, to download and extract the model that
    -- you want to host from Amazon S3 to the individual inference instance
    -- associated with this production variant.
    modelDataDownloadTimeoutInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The serverless configuration for an endpoint. Specifies a serverless
    -- endpoint configuration instead of an instance-based endpoint
    -- configuration.
    serverlessConfig :: Prelude.Maybe ProductionVariantServerlessConfig,
    -- | The size, in GB, of the ML storage volume attached to individual
    -- inference instance associated with the production variant. Currenly only
    -- Amazon EBS gp2 storage volumes are supported.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural,
    -- | The name of the production variant.
    variantName :: Prelude.Text,
    -- | The name of the model that you want to host. This is the name that you
    -- specified when creating the model.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionVariant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorType', 'productionVariant_acceleratorType' - The size of the Elastic Inference (EI) instance to use for the
-- production variant. EI instances provide on-demand GPU computing for
-- inference. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
--
-- 'containerStartupHealthCheckTimeoutInSeconds', 'productionVariant_containerStartupHealthCheckTimeoutInSeconds' - The timeout value, in seconds, for your inference container to pass
-- health check by SageMaker Hosting. For more information about health
-- check, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-inference-code.html#your-algorithms-inference-algo-ping-requests How Your Container Should Respond to Health Check (Ping) Requests>.
--
-- 'coreDumpConfig', 'productionVariant_coreDumpConfig' - Specifies configuration for a core dump from the model container when
-- the process crashes.
--
-- 'initialInstanceCount', 'productionVariant_initialInstanceCount' - Number of instances to launch initially.
--
-- 'initialVariantWeight', 'productionVariant_initialVariantWeight' - Determines initial traffic distribution among all of the models that you
-- specify in the endpoint configuration. The traffic to a production
-- variant is determined by the ratio of the @VariantWeight@ to the sum of
-- all @VariantWeight@ values across all ProductionVariants. If
-- unspecified, it defaults to 1.0.
--
-- 'instanceType', 'productionVariant_instanceType' - The ML compute instance type.
--
-- 'modelDataDownloadTimeoutInSeconds', 'productionVariant_modelDataDownloadTimeoutInSeconds' - The timeout value, in seconds, to download and extract the model that
-- you want to host from Amazon S3 to the individual inference instance
-- associated with this production variant.
--
-- 'serverlessConfig', 'productionVariant_serverlessConfig' - The serverless configuration for an endpoint. Specifies a serverless
-- endpoint configuration instead of an instance-based endpoint
-- configuration.
--
-- 'volumeSizeInGB', 'productionVariant_volumeSizeInGB' - The size, in GB, of the ML storage volume attached to individual
-- inference instance associated with the production variant. Currenly only
-- Amazon EBS gp2 storage volumes are supported.
--
-- 'variantName', 'productionVariant_variantName' - The name of the production variant.
--
-- 'modelName', 'productionVariant_modelName' - The name of the model that you want to host. This is the name that you
-- specified when creating the model.
newProductionVariant ::
  -- | 'variantName'
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
  ProductionVariant
newProductionVariant pVariantName_ pModelName_ =
  ProductionVariant'
    { acceleratorType =
        Prelude.Nothing,
      containerStartupHealthCheckTimeoutInSeconds =
        Prelude.Nothing,
      coreDumpConfig = Prelude.Nothing,
      initialInstanceCount = Prelude.Nothing,
      initialVariantWeight = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      modelDataDownloadTimeoutInSeconds = Prelude.Nothing,
      serverlessConfig = Prelude.Nothing,
      volumeSizeInGB = Prelude.Nothing,
      variantName = pVariantName_,
      modelName = pModelName_
    }

-- | The size of the Elastic Inference (EI) instance to use for the
-- production variant. EI instances provide on-demand GPU computing for
-- inference. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
productionVariant_acceleratorType :: Lens.Lens' ProductionVariant (Prelude.Maybe ProductionVariantAcceleratorType)
productionVariant_acceleratorType = Lens.lens (\ProductionVariant' {acceleratorType} -> acceleratorType) (\s@ProductionVariant' {} a -> s {acceleratorType = a} :: ProductionVariant)

-- | The timeout value, in seconds, for your inference container to pass
-- health check by SageMaker Hosting. For more information about health
-- check, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-inference-code.html#your-algorithms-inference-algo-ping-requests How Your Container Should Respond to Health Check (Ping) Requests>.
productionVariant_containerStartupHealthCheckTimeoutInSeconds :: Lens.Lens' ProductionVariant (Prelude.Maybe Prelude.Natural)
productionVariant_containerStartupHealthCheckTimeoutInSeconds = Lens.lens (\ProductionVariant' {containerStartupHealthCheckTimeoutInSeconds} -> containerStartupHealthCheckTimeoutInSeconds) (\s@ProductionVariant' {} a -> s {containerStartupHealthCheckTimeoutInSeconds = a} :: ProductionVariant)

-- | Specifies configuration for a core dump from the model container when
-- the process crashes.
productionVariant_coreDumpConfig :: Lens.Lens' ProductionVariant (Prelude.Maybe ProductionVariantCoreDumpConfig)
productionVariant_coreDumpConfig = Lens.lens (\ProductionVariant' {coreDumpConfig} -> coreDumpConfig) (\s@ProductionVariant' {} a -> s {coreDumpConfig = a} :: ProductionVariant)

-- | Number of instances to launch initially.
productionVariant_initialInstanceCount :: Lens.Lens' ProductionVariant (Prelude.Maybe Prelude.Natural)
productionVariant_initialInstanceCount = Lens.lens (\ProductionVariant' {initialInstanceCount} -> initialInstanceCount) (\s@ProductionVariant' {} a -> s {initialInstanceCount = a} :: ProductionVariant)

-- | Determines initial traffic distribution among all of the models that you
-- specify in the endpoint configuration. The traffic to a production
-- variant is determined by the ratio of the @VariantWeight@ to the sum of
-- all @VariantWeight@ values across all ProductionVariants. If
-- unspecified, it defaults to 1.0.
productionVariant_initialVariantWeight :: Lens.Lens' ProductionVariant (Prelude.Maybe Prelude.Double)
productionVariant_initialVariantWeight = Lens.lens (\ProductionVariant' {initialVariantWeight} -> initialVariantWeight) (\s@ProductionVariant' {} a -> s {initialVariantWeight = a} :: ProductionVariant)

-- | The ML compute instance type.
productionVariant_instanceType :: Lens.Lens' ProductionVariant (Prelude.Maybe ProductionVariantInstanceType)
productionVariant_instanceType = Lens.lens (\ProductionVariant' {instanceType} -> instanceType) (\s@ProductionVariant' {} a -> s {instanceType = a} :: ProductionVariant)

-- | The timeout value, in seconds, to download and extract the model that
-- you want to host from Amazon S3 to the individual inference instance
-- associated with this production variant.
productionVariant_modelDataDownloadTimeoutInSeconds :: Lens.Lens' ProductionVariant (Prelude.Maybe Prelude.Natural)
productionVariant_modelDataDownloadTimeoutInSeconds = Lens.lens (\ProductionVariant' {modelDataDownloadTimeoutInSeconds} -> modelDataDownloadTimeoutInSeconds) (\s@ProductionVariant' {} a -> s {modelDataDownloadTimeoutInSeconds = a} :: ProductionVariant)

-- | The serverless configuration for an endpoint. Specifies a serverless
-- endpoint configuration instead of an instance-based endpoint
-- configuration.
productionVariant_serverlessConfig :: Lens.Lens' ProductionVariant (Prelude.Maybe ProductionVariantServerlessConfig)
productionVariant_serverlessConfig = Lens.lens (\ProductionVariant' {serverlessConfig} -> serverlessConfig) (\s@ProductionVariant' {} a -> s {serverlessConfig = a} :: ProductionVariant)

-- | The size, in GB, of the ML storage volume attached to individual
-- inference instance associated with the production variant. Currenly only
-- Amazon EBS gp2 storage volumes are supported.
productionVariant_volumeSizeInGB :: Lens.Lens' ProductionVariant (Prelude.Maybe Prelude.Natural)
productionVariant_volumeSizeInGB = Lens.lens (\ProductionVariant' {volumeSizeInGB} -> volumeSizeInGB) (\s@ProductionVariant' {} a -> s {volumeSizeInGB = a} :: ProductionVariant)

-- | The name of the production variant.
productionVariant_variantName :: Lens.Lens' ProductionVariant Prelude.Text
productionVariant_variantName = Lens.lens (\ProductionVariant' {variantName} -> variantName) (\s@ProductionVariant' {} a -> s {variantName = a} :: ProductionVariant)

-- | The name of the model that you want to host. This is the name that you
-- specified when creating the model.
productionVariant_modelName :: Lens.Lens' ProductionVariant Prelude.Text
productionVariant_modelName = Lens.lens (\ProductionVariant' {modelName} -> modelName) (\s@ProductionVariant' {} a -> s {modelName = a} :: ProductionVariant)

instance Data.FromJSON ProductionVariant where
  parseJSON =
    Data.withObject
      "ProductionVariant"
      ( \x ->
          ProductionVariant'
            Prelude.<$> (x Data..:? "AcceleratorType")
            Prelude.<*> ( x
                            Data..:? "ContainerStartupHealthCheckTimeoutInSeconds"
                        )
            Prelude.<*> (x Data..:? "CoreDumpConfig")
            Prelude.<*> (x Data..:? "InitialInstanceCount")
            Prelude.<*> (x Data..:? "InitialVariantWeight")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "ModelDataDownloadTimeoutInSeconds")
            Prelude.<*> (x Data..:? "ServerlessConfig")
            Prelude.<*> (x Data..:? "VolumeSizeInGB")
            Prelude.<*> (x Data..: "VariantName")
            Prelude.<*> (x Data..: "ModelName")
      )

instance Prelude.Hashable ProductionVariant where
  hashWithSalt _salt ProductionVariant' {..} =
    _salt
      `Prelude.hashWithSalt` acceleratorType
      `Prelude.hashWithSalt` containerStartupHealthCheckTimeoutInSeconds
      `Prelude.hashWithSalt` coreDumpConfig
      `Prelude.hashWithSalt` initialInstanceCount
      `Prelude.hashWithSalt` initialVariantWeight
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` modelDataDownloadTimeoutInSeconds
      `Prelude.hashWithSalt` serverlessConfig
      `Prelude.hashWithSalt` volumeSizeInGB
      `Prelude.hashWithSalt` variantName
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData ProductionVariant where
  rnf ProductionVariant' {..} =
    Prelude.rnf acceleratorType
      `Prelude.seq` Prelude.rnf
        containerStartupHealthCheckTimeoutInSeconds
      `Prelude.seq` Prelude.rnf coreDumpConfig
      `Prelude.seq` Prelude.rnf initialInstanceCount
      `Prelude.seq` Prelude.rnf initialVariantWeight
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf modelDataDownloadTimeoutInSeconds
      `Prelude.seq` Prelude.rnf serverlessConfig
      `Prelude.seq` Prelude.rnf volumeSizeInGB
      `Prelude.seq` Prelude.rnf variantName
      `Prelude.seq` Prelude.rnf modelName

instance Data.ToJSON ProductionVariant where
  toJSON ProductionVariant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceleratorType" Data..=)
              Prelude.<$> acceleratorType,
            ( "ContainerStartupHealthCheckTimeoutInSeconds"
                Data..=
            )
              Prelude.<$> containerStartupHealthCheckTimeoutInSeconds,
            ("CoreDumpConfig" Data..=)
              Prelude.<$> coreDumpConfig,
            ("InitialInstanceCount" Data..=)
              Prelude.<$> initialInstanceCount,
            ("InitialVariantWeight" Data..=)
              Prelude.<$> initialVariantWeight,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("ModelDataDownloadTimeoutInSeconds" Data..=)
              Prelude.<$> modelDataDownloadTimeoutInSeconds,
            ("ServerlessConfig" Data..=)
              Prelude.<$> serverlessConfig,
            ("VolumeSizeInGB" Data..=)
              Prelude.<$> volumeSizeInGB,
            Prelude.Just ("VariantName" Data..= variantName),
            Prelude.Just ("ModelName" Data..= modelName)
          ]
      )
