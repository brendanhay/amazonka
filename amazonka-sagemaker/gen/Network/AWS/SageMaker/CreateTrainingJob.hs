{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateTrainingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model training job. After training completes, Amazon SageMaker
-- saves the resulting model artifacts to an Amazon S3 location that you
-- specify.
--
-- If you choose to host your model using Amazon SageMaker hosting
-- services, you can use the resulting model artifacts as part of the
-- model. You can also use the artifacts in a machine learning service
-- other than Amazon SageMaker, provided that you know how to use them for
-- inference.
--
-- In the request body, you provide the following:
--
-- -   @AlgorithmSpecification@ - Identifies the training algorithm to use.
--
-- -   @HyperParameters@ - Specify these algorithm-specific parameters to
--     enable the estimation of model parameters during training.
--     Hyperparameters can be tuned to optimize this learning process. For
--     a list of hyperparameters for each training algorithm provided by
--     Amazon SageMaker, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- -   @InputDataConfig@ - Describes the training dataset and the Amazon
--     S3, EFS, or FSx location where it is stored.
--
-- -   @OutputDataConfig@ - Identifies the Amazon S3 bucket where you want
--     Amazon SageMaker to save the results of model training.
--
-- -   @ResourceConfig@ - Identifies the resources, ML compute instances,
--     and ML storage volumes to deploy for model training. In distributed
--     training, you specify more than one instance.
--
-- -   @EnableManagedSpotTraining@ - Optimize the cost of training machine
--     learning models by up to 80% by using Amazon EC2 Spot instances. For
--     more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/model-managed-spot-training.html Managed Spot Training>.
--
-- -   @RoleArn@ - The Amazon Resource Name (ARN) that Amazon SageMaker
--     assumes to perform tasks on your behalf during model training. You
--     must grant this role the necessary permissions so that Amazon
--     SageMaker can successfully complete model training.
--
-- -   @StoppingCondition@ - To help cap training costs, use
--     @MaxRuntimeInSeconds@ to set a time limit for training. Use
--     @MaxWaitTimeInSeconds@ to specify how long you are willing to wait
--     for a managed spot training job to complete.
--
-- For more information about Amazon SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works>.
module Network.AWS.SageMaker.CreateTrainingJob
  ( -- * Creating a Request
    CreateTrainingJob (..),
    newCreateTrainingJob,

    -- * Request Lenses
    createTrainingJob_vpcConfig,
    createTrainingJob_debugRuleConfigurations,
    createTrainingJob_inputDataConfig,
    createTrainingJob_hyperParameters,
    createTrainingJob_enableManagedSpotTraining,
    createTrainingJob_profilerConfig,
    createTrainingJob_experimentConfig,
    createTrainingJob_enableNetworkIsolation,
    createTrainingJob_enableInterContainerTrafficEncryption,
    createTrainingJob_checkpointConfig,
    createTrainingJob_profilerRuleConfigurations,
    createTrainingJob_tags,
    createTrainingJob_tensorBoardOutputConfig,
    createTrainingJob_debugHookConfig,
    createTrainingJob_trainingJobName,
    createTrainingJob_algorithmSpecification,
    createTrainingJob_roleArn,
    createTrainingJob_outputDataConfig,
    createTrainingJob_resourceConfig,
    createTrainingJob_stoppingCondition,

    -- * Destructuring the Response
    CreateTrainingJobResponse (..),
    newCreateTrainingJobResponse,

    -- * Response Lenses
    createTrainingJobResponse_httpStatus,
    createTrainingJobResponse_trainingJobArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { -- | A VpcConfig object that specifies the VPC that you want your training
    -- job to connect to. Control access to and from your training container by
    -- configuring the VPC. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Configuration information for Debugger rules for debugging output
    -- tensors.
    debugRuleConfigurations :: Prelude.Maybe [DebugRuleConfiguration],
    -- | An array of @Channel@ objects. Each channel is a named input source.
    -- @InputDataConfig@ describes the input data and its location.
    --
    -- Algorithms can accept input data from one or more channels. For example,
    -- an algorithm might have two channels of input data, @training_data@ and
    -- @validation_data@. The configuration for each channel provides the S3,
    -- EFS, or FSx location where the input data is stored. It also provides
    -- information about the stored data: the MIME type, compression method,
    -- and whether the data is wrapped in RecordIO format.
    --
    -- Depending on the input mode that the algorithm supports, Amazon
    -- SageMaker either copies input data files from an S3 bucket to a local
    -- directory in the Docker container, or makes it available as input
    -- streams. For example, if you specify an EFS location, input data files
    -- will be made available as input streams. They do not need to be
    -- downloaded.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    -- | Algorithm-specific parameters that influence the quality of the model.
    -- You set hyperparameters before you start the learning process. For a
    -- list of hyperparameters for each training algorithm provided by Amazon
    -- SageMaker, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    --
    -- You can specify a maximum of 100 hyperparameters. Each hyperparameter is
    -- a key-value pair. Each key and value is limited to 256 characters, as
    -- specified by the @Length Constraint@.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | To train models using managed spot training, choose @True@. Managed spot
    -- training provides a fully managed and scalable infrastructure for
    -- training machine learning models. this option is useful when training
    -- jobs can be interrupted and when there is flexibility when the training
    -- job is run.
    --
    -- The complete and intermediate results of jobs are stored in an Amazon S3
    -- bucket, and can be used as a starting point to train models
    -- incrementally. Amazon SageMaker provides metrics and logs in CloudWatch.
    -- They can be used to see when managed spot training jobs are running,
    -- interrupted, resumed, or completed.
    enableManagedSpotTraining :: Prelude.Maybe Prelude.Bool,
    profilerConfig :: Prelude.Maybe ProfilerConfig,
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | Isolates the training container. No inbound or outbound network calls
    -- can be made, except for calls between peers within a training cluster
    -- for distributed training. If you enable network isolation for training
    -- jobs that are configured to use a VPC, Amazon SageMaker downloads and
    -- uploads customer data and model artifacts through the specified VPC, but
    -- the training container does not have network access.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job>.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
    -- | Contains information about the output location for managed spot training
    -- checkpoint data.
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    -- | Configuration information for Debugger rules for profiling system and
    -- framework metrics.
    profilerRuleConfigurations :: Prelude.Maybe [ProfilerRuleConfiguration],
    -- | An array of key-value pairs. You can use tags to categorize your AWS
    -- resources in different ways, for example, by purpose, owner, or
    -- environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
    tags :: Prelude.Maybe [Tag],
    tensorBoardOutputConfig :: Prelude.Maybe TensorBoardOutputConfig,
    debugHookConfig :: Prelude.Maybe DebugHookConfig,
    -- | The name of the training job. The name must be unique within an AWS
    -- Region in an AWS account.
    trainingJobName :: Prelude.Text,
    -- | The registry path of the Docker image that contains the training
    -- algorithm and algorithm-specific metadata, including the input mode. For
    -- more information about algorithms provided by Amazon SageMaker, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    -- For information about providing your own algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    algorithmSpecification :: AlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
    -- assume to perform tasks on your behalf.
    --
    -- During model training, Amazon SageMaker needs your permission to read
    -- input data from an S3 bucket, download a Docker image that contains
    -- training code, write model artifacts to an S3 bucket, write logs to
    -- Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You
    -- grant permissions for all of these tasks to an IAM role. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
    --
    -- To be able to pass this role to Amazon SageMaker, the caller of this API
    -- must have the @iam:PassRole@ permission.
    roleArn :: Prelude.Text,
    -- | Specifies the path to the S3 location where you want to store model
    -- artifacts. Amazon SageMaker creates subfolders for the artifacts.
    outputDataConfig :: OutputDataConfig,
    -- | The resources, including the ML compute instances and ML storage
    -- volumes, to use for model training.
    --
    -- ML storage volumes store model artifacts and incremental states.
    -- Training algorithms might also use ML storage volumes for scratch space.
    -- If you want Amazon SageMaker to use the ML storage volume to store the
    -- training data, choose @File@ as the @TrainingInputMode@ in the algorithm
    -- specification. For distributed training algorithms, specify an instance
    -- count greater than 1.
    resourceConfig :: ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. When the job
    -- reaches the time limit, Amazon SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
    -- signal, which delays job termination for 120 seconds. Algorithms can use
    -- this 120-second window to save the model artifacts, so the results of
    -- training are not lost.
    stoppingCondition :: StoppingCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrainingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'createTrainingJob_vpcConfig' - A VpcConfig object that specifies the VPC that you want your training
-- job to connect to. Control access to and from your training container by
-- configuring the VPC. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'debugRuleConfigurations', 'createTrainingJob_debugRuleConfigurations' - Configuration information for Debugger rules for debugging output
-- tensors.
--
-- 'inputDataConfig', 'createTrainingJob_inputDataConfig' - An array of @Channel@ objects. Each channel is a named input source.
-- @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example,
-- an algorithm might have two channels of input data, @training_data@ and
-- @validation_data@. The configuration for each channel provides the S3,
-- EFS, or FSx location where the input data is stored. It also provides
-- information about the stored data: the MIME type, compression method,
-- and whether the data is wrapped in RecordIO format.
--
-- Depending on the input mode that the algorithm supports, Amazon
-- SageMaker either copies input data files from an S3 bucket to a local
-- directory in the Docker container, or makes it available as input
-- streams. For example, if you specify an EFS location, input data files
-- will be made available as input streams. They do not need to be
-- downloaded.
--
-- 'hyperParameters', 'createTrainingJob_hyperParameters' - Algorithm-specific parameters that influence the quality of the model.
-- You set hyperparameters before you start the learning process. For a
-- list of hyperparameters for each training algorithm provided by Amazon
-- SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is
-- a key-value pair. Each key and value is limited to 256 characters, as
-- specified by the @Length Constraint@.
--
-- 'enableManagedSpotTraining', 'createTrainingJob_enableManagedSpotTraining' - To train models using managed spot training, choose @True@. Managed spot
-- training provides a fully managed and scalable infrastructure for
-- training machine learning models. this option is useful when training
-- jobs can be interrupted and when there is flexibility when the training
-- job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3
-- bucket, and can be used as a starting point to train models
-- incrementally. Amazon SageMaker provides metrics and logs in CloudWatch.
-- They can be used to see when managed spot training jobs are running,
-- interrupted, resumed, or completed.
--
-- 'profilerConfig', 'createTrainingJob_profilerConfig' - Undocumented member.
--
-- 'experimentConfig', 'createTrainingJob_experimentConfig' - Undocumented member.
--
-- 'enableNetworkIsolation', 'createTrainingJob_enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If you enable network isolation for training
-- jobs that are configured to use a VPC, Amazon SageMaker downloads and
-- uploads customer data and model artifacts through the specified VPC, but
-- the training container does not have network access.
--
-- 'enableInterContainerTrafficEncryption', 'createTrainingJob_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job>.
--
-- 'checkpointConfig', 'createTrainingJob_checkpointConfig' - Contains information about the output location for managed spot training
-- checkpoint data.
--
-- 'profilerRuleConfigurations', 'createTrainingJob_profilerRuleConfigurations' - Configuration information for Debugger rules for profiling system and
-- framework metrics.
--
-- 'tags', 'createTrainingJob_tags' - An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
--
-- 'tensorBoardOutputConfig', 'createTrainingJob_tensorBoardOutputConfig' - Undocumented member.
--
-- 'debugHookConfig', 'createTrainingJob_debugHookConfig' - Undocumented member.
--
-- 'trainingJobName', 'createTrainingJob_trainingJobName' - The name of the training job. The name must be unique within an AWS
-- Region in an AWS account.
--
-- 'algorithmSpecification', 'createTrainingJob_algorithmSpecification' - The registry path of the Docker image that contains the training
-- algorithm and algorithm-specific metadata, including the input mode. For
-- more information about algorithms provided by Amazon SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- For information about providing your own algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- 'roleArn', 'createTrainingJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read
-- input data from an S3 bucket, download a Docker image that contains
-- training code, write model artifacts to an S3 bucket, write logs to
-- Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You
-- grant permissions for all of these tasks to an IAM role. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
--
-- 'outputDataConfig', 'createTrainingJob_outputDataConfig' - Specifies the path to the S3 location where you want to store model
-- artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- 'resourceConfig', 'createTrainingJob_resourceConfig' - The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use ML storage volumes for scratch space.
-- If you want Amazon SageMaker to use the ML storage volume to store the
-- training data, choose @File@ as the @TrainingInputMode@ in the algorithm
-- specification. For distributed training algorithms, specify an instance
-- count greater than 1.
--
-- 'stoppingCondition', 'createTrainingJob_stoppingCondition' - Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
newCreateTrainingJob ::
  -- | 'trainingJobName'
  Prelude.Text ->
  -- | 'algorithmSpecification'
  AlgorithmSpecification ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  CreateTrainingJob
newCreateTrainingJob
  pTrainingJobName_
  pAlgorithmSpecification_
  pRoleArn_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    CreateTrainingJob'
      { vpcConfig = Prelude.Nothing,
        debugRuleConfigurations = Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        hyperParameters = Prelude.Nothing,
        enableManagedSpotTraining = Prelude.Nothing,
        profilerConfig = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        enableNetworkIsolation = Prelude.Nothing,
        enableInterContainerTrafficEncryption =
          Prelude.Nothing,
        checkpointConfig = Prelude.Nothing,
        profilerRuleConfigurations = Prelude.Nothing,
        tags = Prelude.Nothing,
        tensorBoardOutputConfig = Prelude.Nothing,
        debugHookConfig = Prelude.Nothing,
        trainingJobName = pTrainingJobName_,
        algorithmSpecification = pAlgorithmSpecification_,
        roleArn = pRoleArn_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | A VpcConfig object that specifies the VPC that you want your training
-- job to connect to. Control access to and from your training container by
-- configuring the VPC. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
createTrainingJob_vpcConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe VpcConfig)
createTrainingJob_vpcConfig = Lens.lens (\CreateTrainingJob' {vpcConfig} -> vpcConfig) (\s@CreateTrainingJob' {} a -> s {vpcConfig = a} :: CreateTrainingJob)

-- | Configuration information for Debugger rules for debugging output
-- tensors.
createTrainingJob_debugRuleConfigurations :: Lens.Lens' CreateTrainingJob (Prelude.Maybe [DebugRuleConfiguration])
createTrainingJob_debugRuleConfigurations = Lens.lens (\CreateTrainingJob' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@CreateTrainingJob' {} a -> s {debugRuleConfigurations = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens._Coerce

-- | An array of @Channel@ objects. Each channel is a named input source.
-- @InputDataConfig@ describes the input data and its location.
--
-- Algorithms can accept input data from one or more channels. For example,
-- an algorithm might have two channels of input data, @training_data@ and
-- @validation_data@. The configuration for each channel provides the S3,
-- EFS, or FSx location where the input data is stored. It also provides
-- information about the stored data: the MIME type, compression method,
-- and whether the data is wrapped in RecordIO format.
--
-- Depending on the input mode that the algorithm supports, Amazon
-- SageMaker either copies input data files from an S3 bucket to a local
-- directory in the Docker container, or makes it available as input
-- streams. For example, if you specify an EFS location, input data files
-- will be made available as input streams. They do not need to be
-- downloaded.
createTrainingJob_inputDataConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe (Prelude.NonEmpty Channel))
createTrainingJob_inputDataConfig = Lens.lens (\CreateTrainingJob' {inputDataConfig} -> inputDataConfig) (\s@CreateTrainingJob' {} a -> s {inputDataConfig = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens._Coerce

-- | Algorithm-specific parameters that influence the quality of the model.
-- You set hyperparameters before you start the learning process. For a
-- list of hyperparameters for each training algorithm provided by Amazon
-- SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is
-- a key-value pair. Each key and value is limited to 256 characters, as
-- specified by the @Length Constraint@.
createTrainingJob_hyperParameters :: Lens.Lens' CreateTrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTrainingJob_hyperParameters = Lens.lens (\CreateTrainingJob' {hyperParameters} -> hyperParameters) (\s@CreateTrainingJob' {} a -> s {hyperParameters = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens._Coerce

-- | To train models using managed spot training, choose @True@. Managed spot
-- training provides a fully managed and scalable infrastructure for
-- training machine learning models. this option is useful when training
-- jobs can be interrupted and when there is flexibility when the training
-- job is run.
--
-- The complete and intermediate results of jobs are stored in an Amazon S3
-- bucket, and can be used as a starting point to train models
-- incrementally. Amazon SageMaker provides metrics and logs in CloudWatch.
-- They can be used to see when managed spot training jobs are running,
-- interrupted, resumed, or completed.
createTrainingJob_enableManagedSpotTraining :: Lens.Lens' CreateTrainingJob (Prelude.Maybe Prelude.Bool)
createTrainingJob_enableManagedSpotTraining = Lens.lens (\CreateTrainingJob' {enableManagedSpotTraining} -> enableManagedSpotTraining) (\s@CreateTrainingJob' {} a -> s {enableManagedSpotTraining = a} :: CreateTrainingJob)

-- | Undocumented member.
createTrainingJob_profilerConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe ProfilerConfig)
createTrainingJob_profilerConfig = Lens.lens (\CreateTrainingJob' {profilerConfig} -> profilerConfig) (\s@CreateTrainingJob' {} a -> s {profilerConfig = a} :: CreateTrainingJob)

-- | Undocumented member.
createTrainingJob_experimentConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe ExperimentConfig)
createTrainingJob_experimentConfig = Lens.lens (\CreateTrainingJob' {experimentConfig} -> experimentConfig) (\s@CreateTrainingJob' {} a -> s {experimentConfig = a} :: CreateTrainingJob)

-- | Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If you enable network isolation for training
-- jobs that are configured to use a VPC, Amazon SageMaker downloads and
-- uploads customer data and model artifacts through the specified VPC, but
-- the training container does not have network access.
createTrainingJob_enableNetworkIsolation :: Lens.Lens' CreateTrainingJob (Prelude.Maybe Prelude.Bool)
createTrainingJob_enableNetworkIsolation = Lens.lens (\CreateTrainingJob' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@CreateTrainingJob' {} a -> s {enableNetworkIsolation = a} :: CreateTrainingJob)

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job>.
createTrainingJob_enableInterContainerTrafficEncryption :: Lens.Lens' CreateTrainingJob (Prelude.Maybe Prelude.Bool)
createTrainingJob_enableInterContainerTrafficEncryption = Lens.lens (\CreateTrainingJob' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@CreateTrainingJob' {} a -> s {enableInterContainerTrafficEncryption = a} :: CreateTrainingJob)

-- | Contains information about the output location for managed spot training
-- checkpoint data.
createTrainingJob_checkpointConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe CheckpointConfig)
createTrainingJob_checkpointConfig = Lens.lens (\CreateTrainingJob' {checkpointConfig} -> checkpointConfig) (\s@CreateTrainingJob' {} a -> s {checkpointConfig = a} :: CreateTrainingJob)

-- | Configuration information for Debugger rules for profiling system and
-- framework metrics.
createTrainingJob_profilerRuleConfigurations :: Lens.Lens' CreateTrainingJob (Prelude.Maybe [ProfilerRuleConfiguration])
createTrainingJob_profilerRuleConfigurations = Lens.lens (\CreateTrainingJob' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@CreateTrainingJob' {} a -> s {profilerRuleConfigurations = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens._Coerce

-- | An array of key-value pairs. You can use tags to categorize your AWS
-- resources in different ways, for example, by purpose, owner, or
-- environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources>.
createTrainingJob_tags :: Lens.Lens' CreateTrainingJob (Prelude.Maybe [Tag])
createTrainingJob_tags = Lens.lens (\CreateTrainingJob' {tags} -> tags) (\s@CreateTrainingJob' {} a -> s {tags = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
createTrainingJob_tensorBoardOutputConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe TensorBoardOutputConfig)
createTrainingJob_tensorBoardOutputConfig = Lens.lens (\CreateTrainingJob' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@CreateTrainingJob' {} a -> s {tensorBoardOutputConfig = a} :: CreateTrainingJob)

-- | Undocumented member.
createTrainingJob_debugHookConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe DebugHookConfig)
createTrainingJob_debugHookConfig = Lens.lens (\CreateTrainingJob' {debugHookConfig} -> debugHookConfig) (\s@CreateTrainingJob' {} a -> s {debugHookConfig = a} :: CreateTrainingJob)

-- | The name of the training job. The name must be unique within an AWS
-- Region in an AWS account.
createTrainingJob_trainingJobName :: Lens.Lens' CreateTrainingJob Prelude.Text
createTrainingJob_trainingJobName = Lens.lens (\CreateTrainingJob' {trainingJobName} -> trainingJobName) (\s@CreateTrainingJob' {} a -> s {trainingJobName = a} :: CreateTrainingJob)

-- | The registry path of the Docker image that contains the training
-- algorithm and algorithm-specific metadata, including the input mode. For
-- more information about algorithms provided by Amazon SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- For information about providing your own algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
createTrainingJob_algorithmSpecification :: Lens.Lens' CreateTrainingJob AlgorithmSpecification
createTrainingJob_algorithmSpecification = Lens.lens (\CreateTrainingJob' {algorithmSpecification} -> algorithmSpecification) (\s@CreateTrainingJob' {} a -> s {algorithmSpecification = a} :: CreateTrainingJob)

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can
-- assume to perform tasks on your behalf.
--
-- During model training, Amazon SageMaker needs your permission to read
-- input data from an S3 bucket, download a Docker image that contains
-- training code, write model artifacts to an S3 bucket, write logs to
-- Amazon CloudWatch Logs, and publish metrics to Amazon CloudWatch. You
-- grant permissions for all of these tasks to an IAM role. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
createTrainingJob_roleArn :: Lens.Lens' CreateTrainingJob Prelude.Text
createTrainingJob_roleArn = Lens.lens (\CreateTrainingJob' {roleArn} -> roleArn) (\s@CreateTrainingJob' {} a -> s {roleArn = a} :: CreateTrainingJob)

-- | Specifies the path to the S3 location where you want to store model
-- artifacts. Amazon SageMaker creates subfolders for the artifacts.
createTrainingJob_outputDataConfig :: Lens.Lens' CreateTrainingJob OutputDataConfig
createTrainingJob_outputDataConfig = Lens.lens (\CreateTrainingJob' {outputDataConfig} -> outputDataConfig) (\s@CreateTrainingJob' {} a -> s {outputDataConfig = a} :: CreateTrainingJob)

-- | The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use ML storage volumes for scratch space.
-- If you want Amazon SageMaker to use the ML storage volume to store the
-- training data, choose @File@ as the @TrainingInputMode@ in the algorithm
-- specification. For distributed training algorithms, specify an instance
-- count greater than 1.
createTrainingJob_resourceConfig :: Lens.Lens' CreateTrainingJob ResourceConfig
createTrainingJob_resourceConfig = Lens.lens (\CreateTrainingJob' {resourceConfig} -> resourceConfig) (\s@CreateTrainingJob' {} a -> s {resourceConfig = a} :: CreateTrainingJob)

-- | Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the @SIGTERM@
-- signal, which delays job termination for 120 seconds. Algorithms can use
-- this 120-second window to save the model artifacts, so the results of
-- training are not lost.
createTrainingJob_stoppingCondition :: Lens.Lens' CreateTrainingJob StoppingCondition
createTrainingJob_stoppingCondition = Lens.lens (\CreateTrainingJob' {stoppingCondition} -> stoppingCondition) (\s@CreateTrainingJob' {} a -> s {stoppingCondition = a} :: CreateTrainingJob)

instance Core.AWSRequest CreateTrainingJob where
  type
    AWSResponse CreateTrainingJob =
      CreateTrainingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrainingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "TrainingJobArn")
      )

instance Prelude.Hashable CreateTrainingJob

instance Prelude.NFData CreateTrainingJob

instance Core.ToHeaders CreateTrainingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateTrainingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTrainingJob where
  toJSON CreateTrainingJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("DebugRuleConfigurations" Core..=)
              Prelude.<$> debugRuleConfigurations,
            ("InputDataConfig" Core..=)
              Prelude.<$> inputDataConfig,
            ("HyperParameters" Core..=)
              Prelude.<$> hyperParameters,
            ("EnableManagedSpotTraining" Core..=)
              Prelude.<$> enableManagedSpotTraining,
            ("ProfilerConfig" Core..=)
              Prelude.<$> profilerConfig,
            ("ExperimentConfig" Core..=)
              Prelude.<$> experimentConfig,
            ("EnableNetworkIsolation" Core..=)
              Prelude.<$> enableNetworkIsolation,
            ("EnableInterContainerTrafficEncryption" Core..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("CheckpointConfig" Core..=)
              Prelude.<$> checkpointConfig,
            ("ProfilerRuleConfigurations" Core..=)
              Prelude.<$> profilerRuleConfigurations,
            ("Tags" Core..=) Prelude.<$> tags,
            ("TensorBoardOutputConfig" Core..=)
              Prelude.<$> tensorBoardOutputConfig,
            ("DebugHookConfig" Core..=)
              Prelude.<$> debugHookConfig,
            Prelude.Just
              ("TrainingJobName" Core..= trainingJobName),
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

instance Core.ToPath CreateTrainingJob where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTrainingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrainingJobResponse' smart constructor.
data CreateTrainingJobResponse = CreateTrainingJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrainingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTrainingJobResponse_httpStatus' - The response's http status code.
--
-- 'trainingJobArn', 'createTrainingJobResponse_trainingJobArn' - The Amazon Resource Name (ARN) of the training job.
newCreateTrainingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'trainingJobArn'
  Prelude.Text ->
  CreateTrainingJobResponse
newCreateTrainingJobResponse
  pHttpStatus_
  pTrainingJobArn_ =
    CreateTrainingJobResponse'
      { httpStatus =
          pHttpStatus_,
        trainingJobArn = pTrainingJobArn_
      }

-- | The response's http status code.
createTrainingJobResponse_httpStatus :: Lens.Lens' CreateTrainingJobResponse Prelude.Int
createTrainingJobResponse_httpStatus = Lens.lens (\CreateTrainingJobResponse' {httpStatus} -> httpStatus) (\s@CreateTrainingJobResponse' {} a -> s {httpStatus = a} :: CreateTrainingJobResponse)

-- | The Amazon Resource Name (ARN) of the training job.
createTrainingJobResponse_trainingJobArn :: Lens.Lens' CreateTrainingJobResponse Prelude.Text
createTrainingJobResponse_trainingJobArn = Lens.lens (\CreateTrainingJobResponse' {trainingJobArn} -> trainingJobArn) (\s@CreateTrainingJobResponse' {} a -> s {trainingJobArn = a} :: CreateTrainingJobResponse)

instance Prelude.NFData CreateTrainingJobResponse
