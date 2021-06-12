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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.CheckpointConfig
import Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.VpcConfig

-- | Defines the training jobs launched by a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTrainingJobDefinition' smart constructor.
data HyperParameterTrainingJobDefinition = HyperParameterTrainingJobDefinition'
  { -- | The VpcConfig object that specifies the VPC that you want the training
    -- jobs that this hyperparameter tuning job launches to connect to. Control
    -- access to and from your training container by configuring the VPC. For
    -- more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | An array of Channel objects that specify the input for the training jobs
    -- that the tuning job launches.
    inputDataConfig :: Core.Maybe (Core.NonEmpty Channel),
    -- | A Boolean indicating whether managed spot training is enabled (@True@)
    -- or not (@False@).
    enableManagedSpotTraining :: Core.Maybe Core.Bool,
    -- | Specifies the values of hyperparameters that do not change for the
    -- tuning job.
    staticHyperParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    hyperParameterRanges :: Core.Maybe ParameterRanges,
    -- | Isolates the training container. No inbound or outbound network calls
    -- can be made, except for calls between peers within a training cluster
    -- for distributed training. If network isolation is used for training jobs
    -- that are configured to use a VPC, Amazon SageMaker downloads and uploads
    -- customer data and model artifacts through the specified VPC, but the
    -- training container does not have network access.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    checkpointConfig :: Core.Maybe CheckpointConfig,
    tuningObjective :: Core.Maybe HyperParameterTuningJobObjective,
    -- | The job definition name.
    definitionName :: Core.Maybe Core.Text,
    -- | The HyperParameterAlgorithmSpecification object that specifies the
    -- resource algorithm to use for the training jobs that the tuning job
    -- launches.
    algorithmSpecification :: HyperParameterAlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the
    -- training jobs that the tuning job launches.
    roleArn :: Core.Text,
    -- | Specifies the path to the Amazon S3 bucket where you store model
    -- artifacts from the training jobs that the tuning job launches.
    outputDataConfig :: OutputDataConfig,
    -- | The resources, including the compute instances and storage volumes, to
    -- use for the training jobs that the tuning job launches.
    --
    -- Storage volumes store model artifacts and incremental states. Training
    -- algorithms might also use storage volumes for scratch space. If you want
    -- Amazon SageMaker to use the storage volume to store the training data,
    -- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
    -- For distributed training algorithms, specify an instance count greater
    -- than 1.
    resourceConfig :: ResourceConfig,
    -- | Specifies a limit to how long a model hyperparameter training job can
    -- run. It also specifies how long you are willing to wait for a managed
    -- spot training job to complete. When the job reaches the a limit, Amazon
    -- SageMaker ends the training job. Use this API to cap model training
    -- costs.
    stoppingCondition :: StoppingCondition
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HyperParameterTrainingJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'hyperParameterTrainingJobDefinition_vpcConfig' - The VpcConfig object that specifies the VPC that you want the training
-- jobs that this hyperparameter tuning job launches to connect to. Control
-- access to and from your training container by configuring the VPC. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'inputDataConfig', 'hyperParameterTrainingJobDefinition_inputDataConfig' - An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
--
-- 'enableManagedSpotTraining', 'hyperParameterTrainingJobDefinition_enableManagedSpotTraining' - A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
--
-- 'staticHyperParameters', 'hyperParameterTrainingJobDefinition_staticHyperParameters' - Specifies the values of hyperparameters that do not change for the
-- tuning job.
--
-- 'hyperParameterRanges', 'hyperParameterTrainingJobDefinition_hyperParameterRanges' - Undocumented member.
--
-- 'enableNetworkIsolation', 'hyperParameterTrainingJobDefinition_enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, Amazon SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
--
-- 'enableInterContainerTrafficEncryption', 'hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
--
-- 'checkpointConfig', 'hyperParameterTrainingJobDefinition_checkpointConfig' - Undocumented member.
--
-- 'tuningObjective', 'hyperParameterTrainingJobDefinition_tuningObjective' - Undocumented member.
--
-- 'definitionName', 'hyperParameterTrainingJobDefinition_definitionName' - The job definition name.
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
-- 'resourceConfig', 'hyperParameterTrainingJobDefinition_resourceConfig' - The resources, including the compute instances and storage volumes, to
-- use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training
-- algorithms might also use storage volumes for scratch space. If you want
-- Amazon SageMaker to use the storage volume to store the training data,
-- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
-- For distributed training algorithms, specify an instance count greater
-- than 1.
--
-- 'stoppingCondition', 'hyperParameterTrainingJobDefinition_stoppingCondition' - Specifies a limit to how long a model hyperparameter training job can
-- run. It also specifies how long you are willing to wait for a managed
-- spot training job to complete. When the job reaches the a limit, Amazon
-- SageMaker ends the training job. Use this API to cap model training
-- costs.
newHyperParameterTrainingJobDefinition ::
  -- | 'algorithmSpecification'
  HyperParameterAlgorithmSpecification ->
  -- | 'roleArn'
  Core.Text ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  HyperParameterTrainingJobDefinition
newHyperParameterTrainingJobDefinition
  pAlgorithmSpecification_
  pRoleArn_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    HyperParameterTrainingJobDefinition'
      { vpcConfig =
          Core.Nothing,
        inputDataConfig = Core.Nothing,
        enableManagedSpotTraining =
          Core.Nothing,
        staticHyperParameters = Core.Nothing,
        hyperParameterRanges = Core.Nothing,
        enableNetworkIsolation = Core.Nothing,
        enableInterContainerTrafficEncryption =
          Core.Nothing,
        checkpointConfig = Core.Nothing,
        tuningObjective = Core.Nothing,
        definitionName = Core.Nothing,
        algorithmSpecification =
          pAlgorithmSpecification_,
        roleArn = pRoleArn_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition =
          pStoppingCondition_
      }

-- | The VpcConfig object that specifies the VPC that you want the training
-- jobs that this hyperparameter tuning job launches to connect to. Control
-- access to and from your training container by configuring the VPC. For
-- more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
hyperParameterTrainingJobDefinition_vpcConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe VpcConfig)
hyperParameterTrainingJobDefinition_vpcConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {vpcConfig} -> vpcConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {vpcConfig = a} :: HyperParameterTrainingJobDefinition)

-- | An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
hyperParameterTrainingJobDefinition_inputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe (Core.NonEmpty Channel))
hyperParameterTrainingJobDefinition_inputDataConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {inputDataConfig} -> inputDataConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {inputDataConfig = a} :: HyperParameterTrainingJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
hyperParameterTrainingJobDefinition_enableManagedSpotTraining :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Bool)
hyperParameterTrainingJobDefinition_enableManagedSpotTraining = Lens.lens (\HyperParameterTrainingJobDefinition' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableManagedSpotTraining = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies the values of hyperparameters that do not change for the
-- tuning job.
hyperParameterTrainingJobDefinition_staticHyperParameters :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
hyperParameterTrainingJobDefinition_staticHyperParameters = Lens.lens (\HyperParameterTrainingJobDefinition' {staticHyperParameters} -> staticHyperParameters) (\s@HyperParameterTrainingJobDefinition' {} a -> s {staticHyperParameters = a} :: HyperParameterTrainingJobDefinition) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
hyperParameterTrainingJobDefinition_hyperParameterRanges :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe ParameterRanges)
hyperParameterTrainingJobDefinition_hyperParameterRanges = Lens.lens (\HyperParameterTrainingJobDefinition' {hyperParameterRanges} -> hyperParameterRanges) (\s@HyperParameterTrainingJobDefinition' {} a -> s {hyperParameterRanges = a} :: HyperParameterTrainingJobDefinition)

-- | Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, Amazon SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
hyperParameterTrainingJobDefinition_enableNetworkIsolation :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Bool)
hyperParameterTrainingJobDefinition_enableNetworkIsolation = Lens.lens (\HyperParameterTrainingJobDefinition' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableNetworkIsolation = a} :: HyperParameterTrainingJobDefinition)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Bool)
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption = Lens.lens (\HyperParameterTrainingJobDefinition' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableInterContainerTrafficEncryption = a} :: HyperParameterTrainingJobDefinition)

-- | Undocumented member.
hyperParameterTrainingJobDefinition_checkpointConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe CheckpointConfig)
hyperParameterTrainingJobDefinition_checkpointConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {checkpointConfig} -> checkpointConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {checkpointConfig = a} :: HyperParameterTrainingJobDefinition)

-- | Undocumented member.
hyperParameterTrainingJobDefinition_tuningObjective :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe HyperParameterTuningJobObjective)
hyperParameterTrainingJobDefinition_tuningObjective = Lens.lens (\HyperParameterTrainingJobDefinition' {tuningObjective} -> tuningObjective) (\s@HyperParameterTrainingJobDefinition' {} a -> s {tuningObjective = a} :: HyperParameterTrainingJobDefinition)

-- | The job definition name.
hyperParameterTrainingJobDefinition_definitionName :: Lens.Lens' HyperParameterTrainingJobDefinition (Core.Maybe Core.Text)
hyperParameterTrainingJobDefinition_definitionName = Lens.lens (\HyperParameterTrainingJobDefinition' {definitionName} -> definitionName) (\s@HyperParameterTrainingJobDefinition' {} a -> s {definitionName = a} :: HyperParameterTrainingJobDefinition)

-- | The HyperParameterAlgorithmSpecification object that specifies the
-- resource algorithm to use for the training jobs that the tuning job
-- launches.
hyperParameterTrainingJobDefinition_algorithmSpecification :: Lens.Lens' HyperParameterTrainingJobDefinition HyperParameterAlgorithmSpecification
hyperParameterTrainingJobDefinition_algorithmSpecification = Lens.lens (\HyperParameterTrainingJobDefinition' {algorithmSpecification} -> algorithmSpecification) (\s@HyperParameterTrainingJobDefinition' {} a -> s {algorithmSpecification = a} :: HyperParameterTrainingJobDefinition)

-- | The Amazon Resource Name (ARN) of the IAM role associated with the
-- training jobs that the tuning job launches.
hyperParameterTrainingJobDefinition_roleArn :: Lens.Lens' HyperParameterTrainingJobDefinition Core.Text
hyperParameterTrainingJobDefinition_roleArn = Lens.lens (\HyperParameterTrainingJobDefinition' {roleArn} -> roleArn) (\s@HyperParameterTrainingJobDefinition' {} a -> s {roleArn = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies the path to the Amazon S3 bucket where you store model
-- artifacts from the training jobs that the tuning job launches.
hyperParameterTrainingJobDefinition_outputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition OutputDataConfig
hyperParameterTrainingJobDefinition_outputDataConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {outputDataConfig} -> outputDataConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {outputDataConfig = a} :: HyperParameterTrainingJobDefinition)

-- | The resources, including the compute instances and storage volumes, to
-- use for the training jobs that the tuning job launches.
--
-- Storage volumes store model artifacts and incremental states. Training
-- algorithms might also use storage volumes for scratch space. If you want
-- Amazon SageMaker to use the storage volume to store the training data,
-- choose @File@ as the @TrainingInputMode@ in the algorithm specification.
-- For distributed training algorithms, specify an instance count greater
-- than 1.
hyperParameterTrainingJobDefinition_resourceConfig :: Lens.Lens' HyperParameterTrainingJobDefinition ResourceConfig
hyperParameterTrainingJobDefinition_resourceConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {resourceConfig} -> resourceConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {resourceConfig = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies a limit to how long a model hyperparameter training job can
-- run. It also specifies how long you are willing to wait for a managed
-- spot training job to complete. When the job reaches the a limit, Amazon
-- SageMaker ends the training job. Use this API to cap model training
-- costs.
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
            Core.<$> (x Core..:? "VpcConfig")
            Core.<*> (x Core..:? "InputDataConfig")
            Core.<*> (x Core..:? "EnableManagedSpotTraining")
            Core.<*> ( x Core..:? "StaticHyperParameters"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "HyperParameterRanges")
            Core.<*> (x Core..:? "EnableNetworkIsolation")
            Core.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Core.<*> (x Core..:? "CheckpointConfig")
            Core.<*> (x Core..:? "TuningObjective")
            Core.<*> (x Core..:? "DefinitionName")
            Core.<*> (x Core..: "AlgorithmSpecification")
            Core.<*> (x Core..: "RoleArn")
            Core.<*> (x Core..: "OutputDataConfig")
            Core.<*> (x Core..: "ResourceConfig")
            Core.<*> (x Core..: "StoppingCondition")
      )

instance
  Core.Hashable
    HyperParameterTrainingJobDefinition

instance
  Core.NFData
    HyperParameterTrainingJobDefinition

instance
  Core.ToJSON
    HyperParameterTrainingJobDefinition
  where
  toJSON HyperParameterTrainingJobDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VpcConfig" Core..=) Core.<$> vpcConfig,
            ("InputDataConfig" Core..=) Core.<$> inputDataConfig,
            ("EnableManagedSpotTraining" Core..=)
              Core.<$> enableManagedSpotTraining,
            ("StaticHyperParameters" Core..=)
              Core.<$> staticHyperParameters,
            ("HyperParameterRanges" Core..=)
              Core.<$> hyperParameterRanges,
            ("EnableNetworkIsolation" Core..=)
              Core.<$> enableNetworkIsolation,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Core.<$> enableInterContainerTrafficEncryption,
            ("CheckpointConfig" Core..=)
              Core.<$> checkpointConfig,
            ("TuningObjective" Core..=) Core.<$> tuningObjective,
            ("DefinitionName" Core..=) Core.<$> definitionName,
            Core.Just
              ( "AlgorithmSpecification"
                  Core..= algorithmSpecification
              ),
            Core.Just ("RoleArn" Core..= roleArn),
            Core.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("ResourceConfig" Core..= resourceConfig),
            Core.Just
              ("StoppingCondition" Core..= stoppingCondition)
          ]
      )
