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
import qualified Network.AWS.Prelude as Prelude
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
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | An array of Channel objects that specify the input for the training jobs
    -- that the tuning job launches.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | A Boolean indicating whether managed spot training is enabled (@True@)
    -- or not (@False@).
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the values of hyperparameters that do not change for the
    -- tuning job.
    staticHyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    hyperParameterRanges :: Prelude.Maybe ParameterRanges,
    -- | Isolates the training container. No inbound or outbound network calls
    -- can be made, except for calls between peers within a training cluster
    -- for distributed training. If network isolation is used for training jobs
    -- that are configured to use a VPC, Amazon SageMaker downloads and uploads
    -- customer data and model artifacts through the specified VPC, but the
    -- training container does not have network access.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    tuningObjective :: Prelude.Maybe HyperParameterTuningJobObjective,
    -- | The job definition name.
    definitionName :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
          Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        enableManagedSpotTraining =
          Prelude.Nothing,
        staticHyperParameters =
          Prelude.Nothing,
        hyperParameterRanges = Prelude.Nothing,
        enableNetworkIsolation =
          Prelude.Nothing,
        enableInterContainerTrafficEncryption =
          Prelude.Nothing,
        checkpointConfig = Prelude.Nothing,
        tuningObjective = Prelude.Nothing,
        definitionName = Prelude.Nothing,
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
hyperParameterTrainingJobDefinition_vpcConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe VpcConfig)
hyperParameterTrainingJobDefinition_vpcConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {vpcConfig} -> vpcConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {vpcConfig = a} :: HyperParameterTrainingJobDefinition)

-- | An array of Channel objects that specify the input for the training jobs
-- that the tuning job launches.
hyperParameterTrainingJobDefinition_inputDataConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe (Prelude.NonEmpty Channel))
hyperParameterTrainingJobDefinition_inputDataConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {inputDataConfig} -> inputDataConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {inputDataConfig = a} :: HyperParameterTrainingJobDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | A Boolean indicating whether managed spot training is enabled (@True@)
-- or not (@False@).
hyperParameterTrainingJobDefinition_enableManagedSpotTraining :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableManagedSpotTraining = Lens.lens (\HyperParameterTrainingJobDefinition' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableManagedSpotTraining = a} :: HyperParameterTrainingJobDefinition)

-- | Specifies the values of hyperparameters that do not change for the
-- tuning job.
hyperParameterTrainingJobDefinition_staticHyperParameters :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
hyperParameterTrainingJobDefinition_staticHyperParameters = Lens.lens (\HyperParameterTrainingJobDefinition' {staticHyperParameters} -> staticHyperParameters) (\s@HyperParameterTrainingJobDefinition' {} a -> s {staticHyperParameters = a} :: HyperParameterTrainingJobDefinition) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
hyperParameterTrainingJobDefinition_hyperParameterRanges :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe ParameterRanges)
hyperParameterTrainingJobDefinition_hyperParameterRanges = Lens.lens (\HyperParameterTrainingJobDefinition' {hyperParameterRanges} -> hyperParameterRanges) (\s@HyperParameterTrainingJobDefinition' {} a -> s {hyperParameterRanges = a} :: HyperParameterTrainingJobDefinition)

-- | Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If network isolation is used for training jobs
-- that are configured to use a VPC, Amazon SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
hyperParameterTrainingJobDefinition_enableNetworkIsolation :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableNetworkIsolation = Lens.lens (\HyperParameterTrainingJobDefinition' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableNetworkIsolation = a} :: HyperParameterTrainingJobDefinition)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training.
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Bool)
hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption = Lens.lens (\HyperParameterTrainingJobDefinition' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@HyperParameterTrainingJobDefinition' {} a -> s {enableInterContainerTrafficEncryption = a} :: HyperParameterTrainingJobDefinition)

-- | Undocumented member.
hyperParameterTrainingJobDefinition_checkpointConfig :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe CheckpointConfig)
hyperParameterTrainingJobDefinition_checkpointConfig = Lens.lens (\HyperParameterTrainingJobDefinition' {checkpointConfig} -> checkpointConfig) (\s@HyperParameterTrainingJobDefinition' {} a -> s {checkpointConfig = a} :: HyperParameterTrainingJobDefinition)

-- | Undocumented member.
hyperParameterTrainingJobDefinition_tuningObjective :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe HyperParameterTuningJobObjective)
hyperParameterTrainingJobDefinition_tuningObjective = Lens.lens (\HyperParameterTrainingJobDefinition' {tuningObjective} -> tuningObjective) (\s@HyperParameterTrainingJobDefinition' {} a -> s {tuningObjective = a} :: HyperParameterTrainingJobDefinition)

-- | The job definition name.
hyperParameterTrainingJobDefinition_definitionName :: Lens.Lens' HyperParameterTrainingJobDefinition (Prelude.Maybe Prelude.Text)
hyperParameterTrainingJobDefinition_definitionName = Lens.lens (\HyperParameterTrainingJobDefinition' {definitionName} -> definitionName) (\s@HyperParameterTrainingJobDefinition' {} a -> s {definitionName = a} :: HyperParameterTrainingJobDefinition)

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
            Prelude.<$> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "EnableManagedSpotTraining")
            Prelude.<*> ( x Core..:? "StaticHyperParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "HyperParameterRanges")
            Prelude.<*> (x Core..:? "EnableNetworkIsolation")
            Prelude.<*> (x Core..:? "EnableInterContainerTrafficEncryption")
            Prelude.<*> (x Core..:? "CheckpointConfig")
            Prelude.<*> (x Core..:? "TuningObjective")
            Prelude.<*> (x Core..:? "DefinitionName")
            Prelude.<*> (x Core..: "AlgorithmSpecification")
            Prelude.<*> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "OutputDataConfig")
            Prelude.<*> (x Core..: "ResourceConfig")
            Prelude.<*> (x Core..: "StoppingCondition")
      )

instance
  Prelude.Hashable
    HyperParameterTrainingJobDefinition

instance
  Prelude.NFData
    HyperParameterTrainingJobDefinition

instance
  Core.ToJSON
    HyperParameterTrainingJobDefinition
  where
  toJSON HyperParameterTrainingJobDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("InputDataConfig" Core..=)
              Prelude.<$> inputDataConfig,
            ("EnableManagedSpotTraining" Core..=)
              Prelude.<$> enableManagedSpotTraining,
            ("StaticHyperParameters" Core..=)
              Prelude.<$> staticHyperParameters,
            ("HyperParameterRanges" Core..=)
              Prelude.<$> hyperParameterRanges,
            ("EnableNetworkIsolation" Core..=)
              Prelude.<$> enableNetworkIsolation,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("CheckpointConfig" Core..=)
              Prelude.<$> checkpointConfig,
            ("TuningObjective" Core..=)
              Prelude.<$> tuningObjective,
            ("DefinitionName" Core..=)
              Prelude.<$> definitionName,
            Prelude.Just
              ( "AlgorithmSpecification"
                  Core..= algorithmSpecification
              ),
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just
              ("OutputDataConfig" Core..= outputDataConfig),
            Prelude.Just
              ("ResourceConfig" Core..= resourceConfig),
            Prelude.Just
              ("StoppingCondition" Core..= stoppingCondition)
          ]
      )
