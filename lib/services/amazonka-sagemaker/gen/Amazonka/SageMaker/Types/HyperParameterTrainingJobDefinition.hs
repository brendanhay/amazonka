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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.CheckpointConfig
import Amazonka.SageMaker.Types.HyperParameterAlgorithmSpecification
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
import Amazonka.SageMaker.Types.HyperParameterTuningResourceConfig
import Amazonka.SageMaker.Types.OutputDataConfig
import Amazonka.SageMaker.Types.ParameterRanges
import Amazonka.SageMaker.Types.ResourceConfig
import Amazonka.SageMaker.Types.RetryStrategy
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.VpcConfig

-- | Defines the training jobs launched by a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTrainingJobDefinition' smart constructor.
data HyperParameterTrainingJobDefinition = HyperParameterTrainingJobDefinition'
  { tuningObjective :: Prelude.Maybe HyperParameterTuningJobObjective,
    -- | A Boolean indicating whether managed spot training is enabled (@True@)
    -- or not (@False@).
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | The configuration for the hyperparameter tuning resources, including the
    -- compute instances and storage volumes, used for training jobs launched
    -- by the tuning job. By default, storage volumes hold model artifacts and
    -- incremental states. Choose @File@ for @TrainingInputMode@ in the
    -- @AlgorithmSpecification@ parameter to additionally store training data
    -- in the storage volume (optional).
    hyperParameterTuningResourceConfig :: Prelude.Maybe HyperParameterTuningResourceConfig,
    hyperParameterRanges :: Prelude.Maybe ParameterRanges,
    -- | The number of times to retry the job when the job fails due to an
    -- @InternalServerError@.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | The VpcConfig object that specifies the VPC that you want the training
    -- jobs that this hyperparameter tuning job launches to connect to. Control
    -- access to and from your training container by configuring the VPC. For
    -- more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Isolates the training container. No inbound or outbound network calls
    -- can be made, except for calls between peers within a training cluster
    -- for distributed training. If network isolation is used for training jobs
    -- that are configured to use a VPC, SageMaker downloads and uploads
    -- customer data and model artifacts through the specified VPC, but the
    -- training container does not have network access.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | The resources, including the compute instances and storage volumes, to
    -- use for the training jobs that the tuning job launches.
    --
    -- Storage volumes store model artifacts and incremental states. Training
    -- algorithms might also use storage volumes for scratch space. If you want
    -- SageMaker to use the storage volume to store the training data, choose
    -- @File@ as the @TrainingInputMode@ in the algorithm specification. For
    -- distributed training algorithms, specify an instance count greater than
    -- 1.
    --
    -- If you want to use hyperparameter optimization with instance type
    -- flexibility, use @HyperParameterTuningResourceConfig@ instead.
    resourceConfig :: Prelude.Maybe ResourceConfig,
    -- | The job definition name.
    definitionName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the values of hyperparameters that do not change for the
    -- tuning job.
    staticHyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | An array of Channel objects that specify the input for the training jobs
    -- that the tuning job launches.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | The HyperParameterAlgorithmSpecification object that specifies the
    -- resource algorithm to use for the training jobs that the tuning job
    -- launches.
    algorithmSpecification :: HyperParameterAlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the
    -- training jobs that the tuning job launches.
    roleArn :: Prelude.Text,
    -- | Specifies the path to the Amazon S3 bucket where you store model
    -- artifacts from the training jobs that the tuning job launches.
    outputDataConfig :: OutputDataConfig,
    -- | Specifies a limit to how long a model hyperparameter training job can
    -- run. It also specifies how long a managed spot training job has to
    -- complete. When the job reaches the time limit, SageMaker ends the
    -- training job. Use this API to cap model training costs.
    stoppingCondition :: StoppingCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTrainingJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tuningObjective', 'hyperParameterTrainingJobDefinition_tuningObjective' - Undocumented member.
--
-- 'enableManagedSpotTraining', 'hyperParameterTrainingJobDefinition_enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
--
-- 'hyperParameterTuningResourceConfig', 'hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig' - The configuration for the hyperparameter tuning resources, including the
-- compute instances and storage volumes, used for training jobs launched
-- by the tuning job. By default, storage volumes hold model artifacts and
-- incremental states. Choose @File@ for @TrainingInputMode@ in the
-- @AlgorithmSpecification@ parameter to additionally store training data
-- in the storage volume (optional).
--
-- 'hyperParameterRanges', 'hyperParameterTrainingJobDefinition_hyperParameterRanges' - Undocumented member.
--
-- 'retryStrategy', 'hyperParameterTrainingJobDefinition_retryStrategy' - The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
--
-- 'vpcConfig', 'hyperParameterTrainingJobDefinition_vpcConfig' - The VpcConfig object that specifies the VPC that you want the training
-- jobs that this hyperparameter tuning job launches to connect to. Control
-- access to and from your training container by configuring the VPC. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'enableNetworkIsolation', 'hyperParameterTrainingJobDefinition_enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
--
-- 'resourceConfig', 'hyperParameterTrainingJobDefinition_resourceConfig' - The resources, including the compute instances and storage volumes, to
-- use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training
-- algorithms might also use storage volumes for scratch space. If you want
-- SageMaker to use the storage volume to store the training data, choose
-- @File@ as the @TrainingInputMode@ in the algorithm specification. For
-- distributed training algorithms, specify an instance count greater than
-- 1.
--
-- If you want to use hyperparameter optimization with instance type
-- flexibility, use @HyperParameterTuningResourceConfig@ instead.
--
-- 'definitionName', 'hyperParameterTrainingJobDefinition_definitionName' - The job definition name.
--
-- 'staticHyperParameters', 'hyperParameterTrainingJobDefinition_staticHyperParameters' - Specifies the values of hyperparameters that do not change for the
-- tuning job.
--
-- 'checkpointConfig', 'hyperParameterTrainingJobDefinition_checkpointConfig' - Undocumented member.
--
-- 'enableInterContainerTrafficEncryption', 'hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
--
-- 'inputDataConfig', 'hyperParameterTrainingJobDefinition_inputDataConfig' - An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
--
-- 'algorithmSpecification', 'hyperParameterTrainingJobDefinition_algorithmSpecification' - The HyperParameterAlgorithmSpecification object that specifies the
-- resource algorithm to use for the training jobs that the tuning job
-- launches.
--
-- 'roleArn', 'hyperParameterTrainingJobDefinition_roleArn' - The Amazon Resource Name (ARN) of the IAM role associated with the
-- training jobs that the tuning job launches.
--
-- 'outputDataConfig', 'hyperParameterTrainingJobDefinition_outputDataConfig' - Specifies the path to the Amazon S3 bucket where you store model
-- artifacts from the training jobs that the tuning job launches.
--
-- 'stoppingCondition', 'hyperParameterTrainingJobDefinition_stoppingCondition' - Specifies a limit to how long a model hyperparameter training job can
-- run. It also specifies how long a managed spot training job has to
-- complete. When the job reaches the time limit, SageMaker ends the
-- training job. Use this API to cap model training costs.
newHyperParameterTrainingJobDefinition ::
  -- | 'algorithmSpecification'
  HyperParameterAlgorithmSpecification ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  HyperParameterTrainingJobDefinition
newHyperParameterTrainingJobDefinition
  pAlgorithmSpecification_
  pRoleArn_
  pOutputDataConfig_
  pStoppingCondition_ =
    HyperParameterTrainingJobDefinition'
      { tuningObjective =
          Prelude.Nothing,
        enableManagedSpotTraining =
          Prelude.Nothing,
        hyperParameterTuningResourceConfig =
          Prelude.Nothing,
        hyperParameterRanges = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        enableNetworkIsolation =
          Prelude.Nothing,
        resourceConfig = Prelude.Nothing,
        definitionName = Prelude.Nothing,
        staticHyperParameters =
          Prelude.Nothing,
        checkpointConfig = Prelude.Nothing,
        enableInterContainerTrafficEncryption =
          Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        algorithmSpecification =
          pAlgorithmSpecification_,
        roleArn = pRoleArn_,
        outputDataConfig = pOutputDataConfig_,
        stoppingCondition =
          pStoppingCondition_
      }

-- | Undocumented member.
hyperParameterTrainingJobDefinition_tuningObjective :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe HyperParameterTuningJobObjective)
hyperParameterTrainingJobDefinition_tuningObjective = Lens.lens (\HyperParameterTrainingJobDefinition' {tuningObjective} -> tuningObjective) (\s@HyperParameterTrainingJobDefinition' {} a -> s {tuningObjective = a} :: HyperParameterTrainingJobDefinition)

-- | A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
hyperParameterTrainingJobDefinition_enableManagedSpotTraining :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableManagedSpotTraining = Lens.lens (\HyperParameterTrainingJobDefinition' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableManagedSpotTraining = a} :: HyperParameterTrainingJobDefinition)

-- | The configuration for the hyperparameter tuning resources, including the
-- compute instances and storage volumes, used for training jobs launched
-- by the tuning job. By default, storage volumes hold model artifacts and
-- incremental states. Choose @File@ for @TrainingInputMode@ in the
-- @AlgorithmSpecification@ parameter to additionally store training data
-- in the storage volume (optional).
hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe HyperParameterTuningResourceConfig)
hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {hyperParameterTuningResourceConfig} -> hyperParameterTuningResourceConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {hyperParameterTuningResourceConfig = a} :: HyperParameterTrainingJobDefinition)

-- | Undocumented member.
hyperParameterTrainingJobDefinition_hyperParameterRanges :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe ParameterRanges)
hyperParameterTrainingJobDefinition_hyperParameterRanges = Lens.lens (\HyperParameterTrainingJobDefinition' {hyperParameterRanges} -> hyperParameterRanges) (\s@HyperParameterTrainingJobDefinition' {} a -> s {hyperParameterRanges = a} :: HyperParameterTrainingJobDefinition)

-- | The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
hyperParameterTrainingJobDefinition_retryStrategy :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe RetryStrategy)
hyperParameterTrainingJobDefinition_retryStrategy = Lens.lens (\HyperParameterTrainingJobDefinition' {retryStrategy} -> retryStrategy) (\s@HyperParameterTrainingJobDefinition' {} a -> s {retryStrategy = a} :: HyperParameterTrainingJobDefinition)

-- | The VpcConfig object that specifies the VPC that you want the training
-- jobs that this hyperparameter tuning job launches to connect to. Control
-- access to and from your training container by configuring the VPC. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
hyperParameterTrainingJobDefinition_vpcConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe VpcConfig)
hyperParameterTrainingJobDefinition_vpcConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {vpcConfig} -> vpcConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {vpcConfig = a} :: HyperParameterTrainingJobDefinition)

-- | Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
hyperParameterTrainingJobDefinition_enableNetworkIsolation :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableNetworkIsolation = Lens.lens (\HyperParameterTrainingJobDefinition' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableNetworkIsolation = a} :: HyperParameterTrainingJobDefinition)

-- | The resources, including the compute instances and storage volumes, to
-- use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training
-- algorithms might also use storage volumes for scratch space. If you want
-- SageMaker to use the storage volume to store the training data, choose
-- @File@ as the @TrainingInputMode@ in the algorithm specification. For
-- distributed training algorithms, specify an instance count greater than
-- 1.
--
-- If you want to use hyperparameter optimization with instance type
-- flexibility, use @HyperParameterTuningResourceConfig@ instead.
hyperParameterTrainingJobDefinition_resourceConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe ResourceConfig)
hyperParameterTrainingJobDefinition_resourceConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {resourceConfig} -> resourceConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {resourceConfig = a} :: HyperParameterTrainingJobDefinition)

-- | The job definition name.
hyperParameterTrainingJobDefinition_definitionName :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Text)
hyperParameterTrainingJobDefinition_definitionName = Lens.lens (\HyperParameterTrainingJobDefinition' {definitionName} -> definitionName) (\s@HyperParameterTrainingJobDefinition' {} a -> s {definitionName = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies the values of hyperparameters that do not change for the
-- tuning job.
hyperParameterTrainingJobDefinition_staticHyperParameters :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
hyperParameterTrainingJobDefinition_staticHyperParameters = Lens.lens (\HyperParameterTrainingJobDefinition' {staticHyperParameters} -> staticHyperParameters) (\s@HyperParameterTrainingJobDefinition' {} a -> s {staticHyperParameters = a} :: HyperParameterTrainingJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
hyperParameterTrainingJobDefinition_checkpointConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe CheckpointConfig)
hyperParameterTrainingJobDefinition_checkpointConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {checkpointConfig} -> checkpointConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {checkpointConfig = a} :: HyperParameterTrainingJobDefinition)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption = Lens.lens (\HyperParameterTrainingJobDefinition' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableInterContainerTrafficEncryption = a} :: HyperParameterTrainingJobDefinition)

-- | An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
hyperParameterTrainingJobDefinition_inputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe (Prelude.NonEmpty Channel))
hyperParameterTrainingJobDefinition_inputDataConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {inputDataConfig} -> inputDataConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {inputDataConfig = a} :: HyperParameterTrainingJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The HyperParameterAlgorithmSpecification object that specifies the
-- resource algorithm to use for the training jobs that the tuning job
-- launches.
hyperParameterTrainingJobDefinition_algorithmSpecification :: Lens.Lens' HyperParameterTrainingJobDefinition HyperParameterAlgorithmSpecification
hyperParameterTrainingJobDefinition_algorithmSpecification = Lens.lens (\HyperParameterTrainingJobDefinition' {algorithmSpecification} -> algorithmSpecification) (\s@HyperParameterTrainingJobDefinition' {} a -> s {algorithmSpecification = a} :: HyperParameterTrainingJobDefinition)

-- | The Amazon Resource Name (ARN) of the IAM role associated with the
-- training jobs that the tuning job launches.
hyperParameterTrainingJobDefinition_roleArn :: Lens.Lens' HyperParameterTrainingJobDefinition Prelude.Text
hyperParameterTrainingJobDefinition_roleArn = Lens.lens (\HyperParameterTrainingJobDefinition' {roleArn} -> roleArn) (\s@HyperParameterTrainingJobDefinition' {} a -> s {roleArn = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies the path to the Amazon S3 bucket where you store model
-- artifacts from the training jobs that the tuning job launches.
hyperParameterTrainingJobDefinition_outputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition OutputDataConfig
hyperParameterTrainingJobDefinition_outputDataConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {outputDataConfig} -> outputDataConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {outputDataConfig = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies a limit to how long a model hyperparameter training job can
-- run. It also specifies how long a managed spot training job has to
-- complete. When the job reaches the time limit, SageMaker ends the
-- training job. Use this API to cap model training costs.
hyperParameterTrainingJobDefinition_stoppingCondition :: Lens.Lens' HyperParameterTrainingJobDefinition StoppingCondition
hyperParameterTrainingJobDefinition_stoppingCondition = Lens.lens (\HyperParameterTrainingJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@HyperParameterTrainingJobDefinition' {} a -> s {stoppingCondition = a} :: HyperParameterTrainingJobDefinition)

instance
  Core.FromJSON
    HyperParameterTrainingJobDefinition
  where
  parseJSON =
    Core.withObject
      "HyperParameterTrainingJobDefinition"
      ( \x ->
          HyperParameterTrainingJobDefinition'
            Prelude.<$> (x Core..:? "TuningObjective")
            Prelude.<*> (x Core..:? "EnableManagedSpotTraining")
            Prelude.<*> (x Core..:? "HyperParameterTuningResourceConfig")
            Prelude.<*> (x Core..:? "HyperParameterRanges")
            Prelude.<*> (x Core..:? "RetryStrategy")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "EnableNetworkIsolation")
            Prelude.<*> (x Core..:? "ResourceConfig")
            Prelude.<*> (x Core..:? "DefinitionName")
            Prelude.<*> ( x Core..:? "StaticHyperParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CheckpointConfig")
            Prelude.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..: "AlgorithmSpecification")
            Prelude.<*> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "OutputDataConfig")
            Prelude.<*> (x Core..: "StoppingCondition")
      )

instance
  Prelude.Hashable
    HyperParameterTrainingJobDefinition
  where
  hashWithSalt
    _salt
    HyperParameterTrainingJobDefinition' {..} =
      _salt `Prelude.hashWithSalt` tuningObjective
        `Prelude.hashWithSalt` enableManagedSpotTraining
        `Prelude.hashWithSalt` hyperParameterTuningResourceConfig
        `Prelude.hashWithSalt` hyperParameterRanges
        `Prelude.hashWithSalt` retryStrategy
        `Prelude.hashWithSalt` vpcConfig
        `Prelude.hashWithSalt` enableNetworkIsolation
        `Prelude.hashWithSalt` resourceConfig
        `Prelude.hashWithSalt` definitionName
        `Prelude.hashWithSalt` staticHyperParameters
        `Prelude.hashWithSalt` checkpointConfig
        `Prelude.hashWithSalt` enableInterContainerTrafficEncryption
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` algorithmSpecification
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` stoppingCondition

instance
  Prelude.NFData
    HyperParameterTrainingJobDefinition
  where
  rnf HyperParameterTrainingJobDefinition' {..} =
    Prelude.rnf tuningObjective
      `Prelude.seq` Prelude.rnf enableManagedSpotTraining
      `Prelude.seq` Prelude.rnf hyperParameterTuningResourceConfig
      `Prelude.seq` Prelude.rnf hyperParameterRanges
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf definitionName
      `Prelude.seq` Prelude.rnf staticHyperParameters
      `Prelude.seq` Prelude.rnf checkpointConfig
      `Prelude.seq` Prelude.rnf
        enableInterContainerTrafficEncryption
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf algorithmSpecification
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf stoppingCondition

instance
  Core.ToJSON
    HyperParameterTrainingJobDefinition
  where
  toJSON HyperParameterTrainingJobDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TuningObjective" Core..=)
              Prelude.<$> tuningObjective,
            ("EnableManagedSpotTraining" Core..=)
              Prelude.<$> enableManagedSpotTraining,
            ("HyperParameterTuningResourceConfig" Core..=)
              Prelude.<$> hyperParameterTuningResourceConfig,
            ("HyperParameterRanges" Core..=)
              Prelude.<$> hyperParameterRanges,
            ("RetryStrategy" Core..=) Prelude.<$> retryStrategy,
            ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("EnableNetworkIsolation" Core..=)
              Prelude.<$> enableNetworkIsolation,
            ("ResourceConfig" Core..=)
              Prelude.<$> resourceConfig,
            ("DefinitionName" Core..=)
              Prelude.<$> definitionName,
            ("StaticHyperParameters" Core..=)
              Prelude.<$> staticHyperParameters,
            ("CheckpointConfig" Core..=)
              Prelude.<$> checkpointConfig,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("InputDataConfig" Core..=)
              Prelude.<$> inputDataConfig,
            Prelude.Just
              ( "AlgorithmSpecification"
                  Core..= algorithmSpecification
              ),
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("StoppingCondition" Core..= stoppingCondition)
          ]
      )
