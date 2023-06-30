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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | The job definition name.
    definitionName :: Prelude.Maybe Prelude.Text,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean indicating whether managed spot training is enabled (@True@)
    -- or not (@False@).
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | Isolates the training container. No inbound or outbound network calls
    -- can be made, except for calls between peers within a training cluster
    -- for distributed training. If network isolation is used for training jobs
    -- that are configured to use a VPC, SageMaker downloads and uploads
    -- customer data and model artifacts through the specified VPC, but the
    -- training container does not have network access.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    hyperParameterRanges :: Prelude.Maybe ParameterRanges,
    -- | The configuration for the hyperparameter tuning resources, including the
    -- compute instances and storage volumes, used for training jobs launched
    -- by the tuning job. By default, storage volumes hold model artifacts and
    -- incremental states. Choose @File@ for @TrainingInputMode@ in the
    -- @AlgorithmSpecification@ parameter to additionally store training data
    -- in the storage volume (optional).
    hyperParameterTuningResourceConfig :: Prelude.Maybe HyperParameterTuningResourceConfig,
    -- | An array of Channel objects that specify the input for the training jobs
    -- that the tuning job launches.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
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
    -- | The number of times to retry the job when the job fails due to an
    -- @InternalServerError@.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | Specifies the values of hyperparameters that do not change for the
    -- tuning job.
    staticHyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    tuningObjective :: Prelude.Maybe HyperParameterTuningJobObjective,
    -- | The VpcConfig object that specifies the VPC that you want the training
    -- jobs that this hyperparameter tuning job launches to connect to. Control
    -- access to and from your training container by configuring the VPC. For
    -- more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
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
-- 'checkpointConfig', 'hyperParameterTrainingJobDefinition_checkpointConfig' - Undocumented member.
--
-- 'definitionName', 'hyperParameterTrainingJobDefinition_definitionName' - The job definition name.
--
-- 'enableInterContainerTrafficEncryption', 'hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
--
-- 'enableManagedSpotTraining', 'hyperParameterTrainingJobDefinition_enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
--
-- 'enableNetworkIsolation', 'hyperParameterTrainingJobDefinition_enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
--
-- 'hyperParameterRanges', 'hyperParameterTrainingJobDefinition_hyperParameterRanges' - Undocumented member.
--
-- 'hyperParameterTuningResourceConfig', 'hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig' - The configuration for the hyperparameter tuning resources, including the
-- compute instances and storage volumes, used for training jobs launched
-- by the tuning job. By default, storage volumes hold model artifacts and
-- incremental states. Choose @File@ for @TrainingInputMode@ in the
-- @AlgorithmSpecification@ parameter to additionally store training data
-- in the storage volume (optional).
--
-- 'inputDataConfig', 'hyperParameterTrainingJobDefinition_inputDataConfig' - An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
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
-- 'retryStrategy', 'hyperParameterTrainingJobDefinition_retryStrategy' - The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
--
-- 'staticHyperParameters', 'hyperParameterTrainingJobDefinition_staticHyperParameters' - Specifies the values of hyperparameters that do not change for the
-- tuning job.
--
-- 'tuningObjective', 'hyperParameterTrainingJobDefinition_tuningObjective' - Undocumented member.
--
-- 'vpcConfig', 'hyperParameterTrainingJobDefinition_vpcConfig' - The VpcConfig object that specifies the VPC that you want the training
-- jobs that this hyperparameter tuning job launches to connect to. Control
-- access to and from your training container by configuring the VPC. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
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
      { checkpointConfig =
          Prelude.Nothing,
        definitionName = Prelude.Nothing,
        enableInterContainerTrafficEncryption =
          Prelude.Nothing,
        enableManagedSpotTraining =
          Prelude.Nothing,
        enableNetworkIsolation =
          Prelude.Nothing,
        hyperParameterRanges = Prelude.Nothing,
        hyperParameterTuningResourceConfig =
          Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        resourceConfig = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        staticHyperParameters =
          Prelude.Nothing,
        tuningObjective = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        algorithmSpecification =
          pAlgorithmSpecification_,
        roleArn = pRoleArn_,
        outputDataConfig = pOutputDataConfig_,
        stoppingCondition =
          pStoppingCondition_
      }

-- | Undocumented member.
hyperParameterTrainingJobDefinition_checkpointConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe CheckpointConfig)
hyperParameterTrainingJobDefinition_checkpointConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {checkpointConfig} -> checkpointConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {checkpointConfig = a} :: HyperParameterTrainingJobDefinition)

-- | The job definition name.
hyperParameterTrainingJobDefinition_definitionName :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Text)
hyperParameterTrainingJobDefinition_definitionName = Lens.lens (\HyperParameterTrainingJobDefinition' {definitionName} -> definitionName) (\s@HyperParameterTrainingJobDefinition' {} a -> s {definitionName = a} :: HyperParameterTrainingJobDefinition)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption = Lens.lens (\HyperParameterTrainingJobDefinition' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableInterContainerTrafficEncryption = a} :: HyperParameterTrainingJobDefinition)

-- | A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
hyperParameterTrainingJobDefinition_enableManagedSpotTraining :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableManagedSpotTraining = Lens.lens (\HyperParameterTrainingJobDefinition' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableManagedSpotTraining = a} :: HyperParameterTrainingJobDefinition)

-- | Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
hyperParameterTrainingJobDefinition_enableNetworkIsolation :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableNetworkIsolation = Lens.lens (\HyperParameterTrainingJobDefinition' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableNetworkIsolation = a} :: HyperParameterTrainingJobDefinition)

-- | Undocumented member.
hyperParameterTrainingJobDefinition_hyperParameterRanges :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe ParameterRanges)
hyperParameterTrainingJobDefinition_hyperParameterRanges = Lens.lens (\HyperParameterTrainingJobDefinition' {hyperParameterRanges} -> hyperParameterRanges) (\s@HyperParameterTrainingJobDefinition' {} a -> s {hyperParameterRanges = a} :: HyperParameterTrainingJobDefinition)

-- | The configuration for the hyperparameter tuning resources, including the
-- compute instances and storage volumes, used for training jobs launched
-- by the tuning job. By default, storage volumes hold model artifacts and
-- incremental states. Choose @File@ for @TrainingInputMode@ in the
-- @AlgorithmSpecification@ parameter to additionally store training data
-- in the storage volume (optional).
hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe HyperParameterTuningResourceConfig)
hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {hyperParameterTuningResourceConfig} -> hyperParameterTuningResourceConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {hyperParameterTuningResourceConfig = a} :: HyperParameterTrainingJobDefinition)

-- | An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
hyperParameterTrainingJobDefinition_inputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe (Prelude.NonEmpty Channel))
hyperParameterTrainingJobDefinition_inputDataConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {inputDataConfig} -> inputDataConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {inputDataConfig = a} :: HyperParameterTrainingJobDefinition) Prelude.. Lens.mapping Lens.coerced

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

-- | The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
hyperParameterTrainingJobDefinition_retryStrategy :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe RetryStrategy)
hyperParameterTrainingJobDefinition_retryStrategy = Lens.lens (\HyperParameterTrainingJobDefinition' {retryStrategy} -> retryStrategy) (\s@HyperParameterTrainingJobDefinition' {} a -> s {retryStrategy = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies the values of hyperparameters that do not change for the
-- tuning job.
hyperParameterTrainingJobDefinition_staticHyperParameters :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
hyperParameterTrainingJobDefinition_staticHyperParameters = Lens.lens (\HyperParameterTrainingJobDefinition' {staticHyperParameters} -> staticHyperParameters) (\s@HyperParameterTrainingJobDefinition' {} a -> s {staticHyperParameters = a} :: HyperParameterTrainingJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
hyperParameterTrainingJobDefinition_tuningObjective :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe HyperParameterTuningJobObjective)
hyperParameterTrainingJobDefinition_tuningObjective = Lens.lens (\HyperParameterTrainingJobDefinition' {tuningObjective} -> tuningObjective) (\s@HyperParameterTrainingJobDefinition' {} a -> s {tuningObjective = a} :: HyperParameterTrainingJobDefinition)

-- | The VpcConfig object that specifies the VPC that you want the training
-- jobs that this hyperparameter tuning job launches to connect to. Control
-- access to and from your training container by configuring the VPC. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
hyperParameterTrainingJobDefinition_vpcConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe VpcConfig)
hyperParameterTrainingJobDefinition_vpcConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {vpcConfig} -> vpcConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {vpcConfig = a} :: HyperParameterTrainingJobDefinition)

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
  Data.FromJSON
    HyperParameterTrainingJobDefinition
  where
  parseJSON =
    Data.withObject
      "HyperParameterTrainingJobDefinition"
      ( \x ->
          HyperParameterTrainingJobDefinition'
            Prelude.<$> (x Data..:? "CheckpointConfig")
            Prelude.<*> (x Data..:? "DefinitionName")
            Prelude.<*> (x Data..:? "EnableInterContainerTrafficEncryption")
            Prelude.<*> (x Data..:? "EnableManagedSpotTraining")
            Prelude.<*> (x Data..:? "EnableNetworkIsolation")
            Prelude.<*> (x Data..:? "HyperParameterRanges")
            Prelude.<*> (x Data..:? "HyperParameterTuningResourceConfig")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "ResourceConfig")
            Prelude.<*> (x Data..:? "RetryStrategy")
            Prelude.<*> ( x
                            Data..:? "StaticHyperParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TuningObjective")
            Prelude.<*> (x Data..:? "VpcConfig")
            Prelude.<*> (x Data..: "AlgorithmSpecification")
            Prelude.<*> (x Data..: "RoleArn")
            Prelude.<*> (x Data..: "OutputDataConfig")
            Prelude.<*> (x Data..: "StoppingCondition")
      )

instance
  Prelude.Hashable
    HyperParameterTrainingJobDefinition
  where
  hashWithSalt
    _salt
    HyperParameterTrainingJobDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` checkpointConfig
        `Prelude.hashWithSalt` definitionName
        `Prelude.hashWithSalt` enableInterContainerTrafficEncryption
        `Prelude.hashWithSalt` enableManagedSpotTraining
        `Prelude.hashWithSalt` enableNetworkIsolation
        `Prelude.hashWithSalt` hyperParameterRanges
        `Prelude.hashWithSalt` hyperParameterTuningResourceConfig
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` resourceConfig
        `Prelude.hashWithSalt` retryStrategy
        `Prelude.hashWithSalt` staticHyperParameters
        `Prelude.hashWithSalt` tuningObjective
        `Prelude.hashWithSalt` vpcConfig
        `Prelude.hashWithSalt` algorithmSpecification
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` stoppingCondition

instance
  Prelude.NFData
    HyperParameterTrainingJobDefinition
  where
  rnf HyperParameterTrainingJobDefinition' {..} =
    Prelude.rnf checkpointConfig
      `Prelude.seq` Prelude.rnf definitionName
      `Prelude.seq` Prelude.rnf enableInterContainerTrafficEncryption
      `Prelude.seq` Prelude.rnf enableManagedSpotTraining
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf hyperParameterRanges
      `Prelude.seq` Prelude.rnf hyperParameterTuningResourceConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf staticHyperParameters
      `Prelude.seq` Prelude.rnf tuningObjective
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf algorithmSpecification
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf stoppingCondition

instance
  Data.ToJSON
    HyperParameterTrainingJobDefinition
  where
  toJSON HyperParameterTrainingJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CheckpointConfig" Data..=)
              Prelude.<$> checkpointConfig,
            ("DefinitionName" Data..=)
              Prelude.<$> definitionName,
            ("EnableInterContainerTrafficEncryption" Data..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("EnableManagedSpotTraining" Data..=)
              Prelude.<$> enableManagedSpotTraining,
            ("EnableNetworkIsolation" Data..=)
              Prelude.<$> enableNetworkIsolation,
            ("HyperParameterRanges" Data..=)
              Prelude.<$> hyperParameterRanges,
            ("HyperParameterTuningResourceConfig" Data..=)
              Prelude.<$> hyperParameterTuningResourceConfig,
            ("InputDataConfig" Data..=)
              Prelude.<$> inputDataConfig,
            ("ResourceConfig" Data..=)
              Prelude.<$> resourceConfig,
            ("RetryStrategy" Data..=) Prelude.<$> retryStrategy,
            ("StaticHyperParameters" Data..=)
              Prelude.<$> staticHyperParameters,
            ("TuningObjective" Data..=)
              Prelude.<$> tuningObjective,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just
              ( "AlgorithmSpecification"
                  Data..= algorithmSpecification
              ),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("StoppingCondition" Data..= stoppingCondition)
          ]
      )
