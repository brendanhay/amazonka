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
-- Module      : Amazonka.SageMaker.CreateTrainingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a model training job. After training completes, SageMaker saves
-- the resulting model artifacts to an Amazon S3 location that you specify.
--
-- If you choose to host your model using SageMaker hosting services, you
-- can use the resulting model artifacts as part of the model. You can also
-- use the artifacts in a machine learning service other than SageMaker,
-- provided that you know how to use them for inference.
--
-- In the request body, you provide the following:
--
-- -   @AlgorithmSpecification@ - Identifies the training algorithm to use.
--
-- -   @HyperParameters@ - Specify these algorithm-specific parameters to
--     enable the estimation of model parameters during training.
--     Hyperparameters can be tuned to optimize this learning process. For
--     a list of hyperparameters for each training algorithm provided by
--     SageMaker, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
--     Do not include any security-sensitive information including account
--     access IDs, secrets or tokens in any hyperparameter field. If the
--     use of security-sensitive credentials are detected, SageMaker will
--     reject your training job request and return an exception error.
--
-- -   @InputDataConfig@ - Describes the input required by the training job
--     and the Amazon S3, EFS, or FSx location where it is stored.
--
-- -   @OutputDataConfig@ - Identifies the Amazon S3 bucket where you want
--     SageMaker to save the results of model training.
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
-- -   @RoleArn@ - The Amazon Resource Name (ARN) that SageMaker assumes to
--     perform tasks on your behalf during model training. You must grant
--     this role the necessary permissions so that SageMaker can
--     successfully complete model training.
--
-- -   @StoppingCondition@ - To help cap training costs, use
--     @MaxRuntimeInSeconds@ to set a time limit for training. Use
--     @MaxWaitTimeInSeconds@ to specify how long a managed spot training
--     job has to complete.
--
-- -   @Environment@ - The environment variables to set in the Docker
--     container.
--
-- -   @RetryStrategy@ - The number of times to retry the job when the job
--     fails due to an @InternalServerError@.
--
-- For more information about SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works>.
module Amazonka.SageMaker.CreateTrainingJob
  ( -- * Creating a Request
    CreateTrainingJob (..),
    newCreateTrainingJob,

    -- * Request Lenses
    createTrainingJob_checkpointConfig,
    createTrainingJob_debugHookConfig,
    createTrainingJob_debugRuleConfigurations,
    createTrainingJob_enableInterContainerTrafficEncryption,
    createTrainingJob_enableManagedSpotTraining,
    createTrainingJob_enableNetworkIsolation,
    createTrainingJob_environment,
    createTrainingJob_experimentConfig,
    createTrainingJob_hyperParameters,
    createTrainingJob_inputDataConfig,
    createTrainingJob_profilerConfig,
    createTrainingJob_profilerRuleConfigurations,
    createTrainingJob_retryStrategy,
    createTrainingJob_tags,
    createTrainingJob_tensorBoardOutputConfig,
    createTrainingJob_vpcConfig,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateTrainingJob' smart constructor.
data CreateTrainingJob = CreateTrainingJob'
  { -- | Contains information about the output location for managed spot training
    -- checkpoint data.
    checkpointConfig :: Prelude.Maybe CheckpointConfig,
    debugHookConfig :: Prelude.Maybe DebugHookConfig,
    -- | Configuration information for Amazon SageMaker Debugger rules for
    -- debugging output tensors.
    debugRuleConfigurations :: Prelude.Maybe [DebugRuleConfiguration],
    -- | To encrypt all communications between ML compute instances in
    -- distributed training, choose @True@. Encryption provides greater
    -- security for distributed training, but training might take longer. How
    -- long it takes depends on the amount of communication between compute
    -- instances, especially if you use a deep learning algorithm in
    -- distributed training. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job>.
    enableInterContainerTrafficEncryption :: Prelude.Maybe Prelude.Bool,
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
    -- | Isolates the training container. No inbound or outbound network calls
    -- can be made, except for calls between peers within a training cluster
    -- for distributed training. If you enable network isolation for training
    -- jobs that are configured to use a VPC, SageMaker downloads and uploads
    -- customer data and model artifacts through the specified VPC, but the
    -- training container does not have network access.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | The environment variables to set in the Docker container.
    environment :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    experimentConfig :: Prelude.Maybe ExperimentConfig,
    -- | Algorithm-specific parameters that influence the quality of the model.
    -- You set hyperparameters before you start the learning process. For a
    -- list of hyperparameters for each training algorithm provided by
    -- SageMaker, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    --
    -- You can specify a maximum of 100 hyperparameters. Each hyperparameter is
    -- a key-value pair. Each key and value is limited to 256 characters, as
    -- specified by the @Length Constraint@.
    --
    -- Do not include any security-sensitive information including account
    -- access IDs, secrets or tokens in any hyperparameter field. If the use of
    -- security-sensitive credentials are detected, SageMaker will reject your
    -- training job request and return an exception error.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- Depending on the input mode that the algorithm supports, SageMaker
    -- either copies input data files from an S3 bucket to a local directory in
    -- the Docker container, or makes it available as input streams. For
    -- example, if you specify an EFS location, input data files are available
    -- as input streams. They do not need to be downloaded.
    inputDataConfig :: Prelude.Maybe (Prelude.NonEmpty Channel),
    profilerConfig :: Prelude.Maybe ProfilerConfig,
    -- | Configuration information for Amazon SageMaker Debugger rules for
    -- profiling system and framework metrics.
    profilerRuleConfigurations :: Prelude.Maybe [ProfilerRuleConfiguration],
    -- | The number of times to retry the job when the job fails due to an
    -- @InternalServerError@.
    retryStrategy :: Prelude.Maybe RetryStrategy,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    tensorBoardOutputConfig :: Prelude.Maybe TensorBoardOutputConfig,
    -- | A VpcConfig object that specifies the VPC that you want your training
    -- job to connect to. Control access to and from your training container by
    -- configuring the VPC. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the training job. The name must be unique within an Amazon
    -- Web Services Region in an Amazon Web Services account.
    trainingJobName :: Prelude.Text,
    -- | The registry path of the Docker image that contains the training
    -- algorithm and algorithm-specific metadata, including the input mode. For
    -- more information about algorithms provided by SageMaker, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    -- For information about providing your own algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    algorithmSpecification :: AlgorithmSpecification,
    -- | The Amazon Resource Name (ARN) of an IAM role that SageMaker can assume
    -- to perform tasks on your behalf.
    --
    -- During model training, SageMaker needs your permission to read input
    -- data from an S3 bucket, download a Docker image that contains training
    -- code, write model artifacts to an S3 bucket, write logs to Amazon
    -- CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant
    -- permissions for all of these tasks to an IAM role. For more information,
    -- see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
    --
    -- To be able to pass this role to SageMaker, the caller of this API must
    -- have the @iam:PassRole@ permission.
    roleArn :: Prelude.Text,
    -- | Specifies the path to the S3 location where you want to store model
    -- artifacts. SageMaker creates subfolders for the artifacts.
    outputDataConfig :: OutputDataConfig,
    -- | The resources, including the ML compute instances and ML storage
    -- volumes, to use for model training.
    --
    -- ML storage volumes store model artifacts and incremental states.
    -- Training algorithms might also use ML storage volumes for scratch space.
    -- If you want SageMaker to use the ML storage volume to store the training
    -- data, choose @File@ as the @TrainingInputMode@ in the algorithm
    -- specification. For distributed training algorithms, specify an instance
    -- count greater than 1.
    resourceConfig :: ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. It also
    -- specifies how long a managed Spot training job has to complete. When the
    -- job reaches the time limit, SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, SageMaker sends the algorithm the @SIGTERM@ signal, which
    -- delays job termination for 120 seconds. Algorithms can use this
    -- 120-second window to save the model artifacts, so the results of
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
-- 'checkpointConfig', 'createTrainingJob_checkpointConfig' - Contains information about the output location for managed spot training
-- checkpoint data.
--
-- 'debugHookConfig', 'createTrainingJob_debugHookConfig' - Undocumented member.
--
-- 'debugRuleConfigurations', 'createTrainingJob_debugRuleConfigurations' - Configuration information for Amazon SageMaker Debugger rules for
-- debugging output tensors.
--
-- 'enableInterContainerTrafficEncryption', 'createTrainingJob_enableInterContainerTrafficEncryption' - To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job>.
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
-- 'enableNetworkIsolation', 'createTrainingJob_enableNetworkIsolation' - Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If you enable network isolation for training
-- jobs that are configured to use a VPC, SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
--
-- 'environment', 'createTrainingJob_environment' - The environment variables to set in the Docker container.
--
-- 'experimentConfig', 'createTrainingJob_experimentConfig' - Undocumented member.
--
-- 'hyperParameters', 'createTrainingJob_hyperParameters' - Algorithm-specific parameters that influence the quality of the model.
-- You set hyperparameters before you start the learning process. For a
-- list of hyperparameters for each training algorithm provided by
-- SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is
-- a key-value pair. Each key and value is limited to 256 characters, as
-- specified by the @Length Constraint@.
--
-- Do not include any security-sensitive information including account
-- access IDs, secrets or tokens in any hyperparameter field. If the use of
-- security-sensitive credentials are detected, SageMaker will reject your
-- training job request and return an exception error.
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
-- Depending on the input mode that the algorithm supports, SageMaker
-- either copies input data files from an S3 bucket to a local directory in
-- the Docker container, or makes it available as input streams. For
-- example, if you specify an EFS location, input data files are available
-- as input streams. They do not need to be downloaded.
--
-- 'profilerConfig', 'createTrainingJob_profilerConfig' - Undocumented member.
--
-- 'profilerRuleConfigurations', 'createTrainingJob_profilerRuleConfigurations' - Configuration information for Amazon SageMaker Debugger rules for
-- profiling system and framework metrics.
--
-- 'retryStrategy', 'createTrainingJob_retryStrategy' - The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
--
-- 'tags', 'createTrainingJob_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'tensorBoardOutputConfig', 'createTrainingJob_tensorBoardOutputConfig' - Undocumented member.
--
-- 'vpcConfig', 'createTrainingJob_vpcConfig' - A VpcConfig object that specifies the VPC that you want your training
-- job to connect to. Control access to and from your training container by
-- configuring the VPC. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
--
-- 'trainingJobName', 'createTrainingJob_trainingJobName' - The name of the training job. The name must be unique within an Amazon
-- Web Services Region in an Amazon Web Services account.
--
-- 'algorithmSpecification', 'createTrainingJob_algorithmSpecification' - The registry path of the Docker image that contains the training
-- algorithm and algorithm-specific metadata, including the input mode. For
-- more information about algorithms provided by SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- For information about providing your own algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- 'roleArn', 'createTrainingJob_roleArn' - The Amazon Resource Name (ARN) of an IAM role that SageMaker can assume
-- to perform tasks on your behalf.
--
-- During model training, SageMaker needs your permission to read input
-- data from an S3 bucket, download a Docker image that contains training
-- code, write model artifacts to an S3 bucket, write logs to Amazon
-- CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant
-- permissions for all of these tasks to an IAM role. For more information,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
--
-- To be able to pass this role to SageMaker, the caller of this API must
-- have the @iam:PassRole@ permission.
--
-- 'outputDataConfig', 'createTrainingJob_outputDataConfig' - Specifies the path to the S3 location where you want to store model
-- artifacts. SageMaker creates subfolders for the artifacts.
--
-- 'resourceConfig', 'createTrainingJob_resourceConfig' - The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use ML storage volumes for scratch space.
-- If you want SageMaker to use the ML storage volume to store the training
-- data, choose @File@ as the @TrainingInputMode@ in the algorithm
-- specification. For distributed training algorithms, specify an instance
-- count greater than 1.
--
-- 'stoppingCondition', 'createTrainingJob_stoppingCondition' - Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, SageMaker sends the algorithm the @SIGTERM@ signal, which
-- delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts, so the results of
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
      { checkpointConfig =
          Prelude.Nothing,
        debugHookConfig = Prelude.Nothing,
        debugRuleConfigurations = Prelude.Nothing,
        enableInterContainerTrafficEncryption =
          Prelude.Nothing,
        enableManagedSpotTraining = Prelude.Nothing,
        enableNetworkIsolation = Prelude.Nothing,
        environment = Prelude.Nothing,
        experimentConfig = Prelude.Nothing,
        hyperParameters = Prelude.Nothing,
        inputDataConfig = Prelude.Nothing,
        profilerConfig = Prelude.Nothing,
        profilerRuleConfigurations = Prelude.Nothing,
        retryStrategy = Prelude.Nothing,
        tags = Prelude.Nothing,
        tensorBoardOutputConfig = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        trainingJobName = pTrainingJobName_,
        algorithmSpecification = pAlgorithmSpecification_,
        roleArn = pRoleArn_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | Contains information about the output location for managed spot training
-- checkpoint data.
createTrainingJob_checkpointConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe CheckpointConfig)
createTrainingJob_checkpointConfig = Lens.lens (\CreateTrainingJob' {checkpointConfig} -> checkpointConfig) (\s@CreateTrainingJob' {} a -> s {checkpointConfig = a} :: CreateTrainingJob)

-- | Undocumented member.
createTrainingJob_debugHookConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe DebugHookConfig)
createTrainingJob_debugHookConfig = Lens.lens (\CreateTrainingJob' {debugHookConfig} -> debugHookConfig) (\s@CreateTrainingJob' {} a -> s {debugHookConfig = a} :: CreateTrainingJob)

-- | Configuration information for Amazon SageMaker Debugger rules for
-- debugging output tensors.
createTrainingJob_debugRuleConfigurations :: Lens.Lens' CreateTrainingJob (Prelude.Maybe [DebugRuleConfiguration])
createTrainingJob_debugRuleConfigurations = Lens.lens (\CreateTrainingJob' {debugRuleConfigurations} -> debugRuleConfigurations) (\s@CreateTrainingJob' {} a -> s {debugRuleConfigurations = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | To encrypt all communications between ML compute instances in
-- distributed training, choose @True@. Encryption provides greater
-- security for distributed training, but training might take longer. How
-- long it takes depends on the amount of communication between compute
-- instances, especially if you use a deep learning algorithm in
-- distributed training. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-encrypt.html Protect Communications Between ML Compute Instances in a Distributed Training Job>.
createTrainingJob_enableInterContainerTrafficEncryption :: Lens.Lens' CreateTrainingJob (Prelude.Maybe Prelude.Bool)
createTrainingJob_enableInterContainerTrafficEncryption = Lens.lens (\CreateTrainingJob' {enableInterContainerTrafficEncryption} -> enableInterContainerTrafficEncryption) (\s@CreateTrainingJob' {} a -> s {enableInterContainerTrafficEncryption = a} :: CreateTrainingJob)

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

-- | Isolates the training container. No inbound or outbound network calls
-- can be made, except for calls between peers within a training cluster
-- for distributed training. If you enable network isolation for training
-- jobs that are configured to use a VPC, SageMaker downloads and uploads
-- customer data and model artifacts through the specified VPC, but the
-- training container does not have network access.
createTrainingJob_enableNetworkIsolation :: Lens.Lens' CreateTrainingJob (Prelude.Maybe Prelude.Bool)
createTrainingJob_enableNetworkIsolation = Lens.lens (\CreateTrainingJob' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@CreateTrainingJob' {} a -> s {enableNetworkIsolation = a} :: CreateTrainingJob)

-- | The environment variables to set in the Docker container.
createTrainingJob_environment :: Lens.Lens' CreateTrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTrainingJob_environment = Lens.lens (\CreateTrainingJob' {environment} -> environment) (\s@CreateTrainingJob' {} a -> s {environment = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createTrainingJob_experimentConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe ExperimentConfig)
createTrainingJob_experimentConfig = Lens.lens (\CreateTrainingJob' {experimentConfig} -> experimentConfig) (\s@CreateTrainingJob' {} a -> s {experimentConfig = a} :: CreateTrainingJob)

-- | Algorithm-specific parameters that influence the quality of the model.
-- You set hyperparameters before you start the learning process. For a
-- list of hyperparameters for each training algorithm provided by
-- SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- You can specify a maximum of 100 hyperparameters. Each hyperparameter is
-- a key-value pair. Each key and value is limited to 256 characters, as
-- specified by the @Length Constraint@.
--
-- Do not include any security-sensitive information including account
-- access IDs, secrets or tokens in any hyperparameter field. If the use of
-- security-sensitive credentials are detected, SageMaker will reject your
-- training job request and return an exception error.
createTrainingJob_hyperParameters :: Lens.Lens' CreateTrainingJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTrainingJob_hyperParameters = Lens.lens (\CreateTrainingJob' {hyperParameters} -> hyperParameters) (\s@CreateTrainingJob' {} a -> s {hyperParameters = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens.coerced

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
-- Depending on the input mode that the algorithm supports, SageMaker
-- either copies input data files from an S3 bucket to a local directory in
-- the Docker container, or makes it available as input streams. For
-- example, if you specify an EFS location, input data files are available
-- as input streams. They do not need to be downloaded.
createTrainingJob_inputDataConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe (Prelude.NonEmpty Channel))
createTrainingJob_inputDataConfig = Lens.lens (\CreateTrainingJob' {inputDataConfig} -> inputDataConfig) (\s@CreateTrainingJob' {} a -> s {inputDataConfig = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createTrainingJob_profilerConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe ProfilerConfig)
createTrainingJob_profilerConfig = Lens.lens (\CreateTrainingJob' {profilerConfig} -> profilerConfig) (\s@CreateTrainingJob' {} a -> s {profilerConfig = a} :: CreateTrainingJob)

-- | Configuration information for Amazon SageMaker Debugger rules for
-- profiling system and framework metrics.
createTrainingJob_profilerRuleConfigurations :: Lens.Lens' CreateTrainingJob (Prelude.Maybe [ProfilerRuleConfiguration])
createTrainingJob_profilerRuleConfigurations = Lens.lens (\CreateTrainingJob' {profilerRuleConfigurations} -> profilerRuleConfigurations) (\s@CreateTrainingJob' {} a -> s {profilerRuleConfigurations = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | The number of times to retry the job when the job fails due to an
-- @InternalServerError@.
createTrainingJob_retryStrategy :: Lens.Lens' CreateTrainingJob (Prelude.Maybe RetryStrategy)
createTrainingJob_retryStrategy = Lens.lens (\CreateTrainingJob' {retryStrategy} -> retryStrategy) (\s@CreateTrainingJob' {} a -> s {retryStrategy = a} :: CreateTrainingJob)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createTrainingJob_tags :: Lens.Lens' CreateTrainingJob (Prelude.Maybe [Tag])
createTrainingJob_tags = Lens.lens (\CreateTrainingJob' {tags} -> tags) (\s@CreateTrainingJob' {} a -> s {tags = a} :: CreateTrainingJob) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createTrainingJob_tensorBoardOutputConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe TensorBoardOutputConfig)
createTrainingJob_tensorBoardOutputConfig = Lens.lens (\CreateTrainingJob' {tensorBoardOutputConfig} -> tensorBoardOutputConfig) (\s@CreateTrainingJob' {} a -> s {tensorBoardOutputConfig = a} :: CreateTrainingJob)

-- | A VpcConfig object that specifies the VPC that you want your training
-- job to connect to. Control access to and from your training container by
-- configuring the VPC. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/train-vpc.html Protect Training Jobs by Using an Amazon Virtual Private Cloud>.
createTrainingJob_vpcConfig :: Lens.Lens' CreateTrainingJob (Prelude.Maybe VpcConfig)
createTrainingJob_vpcConfig = Lens.lens (\CreateTrainingJob' {vpcConfig} -> vpcConfig) (\s@CreateTrainingJob' {} a -> s {vpcConfig = a} :: CreateTrainingJob)

-- | The name of the training job. The name must be unique within an Amazon
-- Web Services Region in an Amazon Web Services account.
createTrainingJob_trainingJobName :: Lens.Lens' CreateTrainingJob Prelude.Text
createTrainingJob_trainingJobName = Lens.lens (\CreateTrainingJob' {trainingJobName} -> trainingJobName) (\s@CreateTrainingJob' {} a -> s {trainingJobName = a} :: CreateTrainingJob)

-- | The registry path of the Docker image that contains the training
-- algorithm and algorithm-specific metadata, including the input mode. For
-- more information about algorithms provided by SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- For information about providing your own algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
createTrainingJob_algorithmSpecification :: Lens.Lens' CreateTrainingJob AlgorithmSpecification
createTrainingJob_algorithmSpecification = Lens.lens (\CreateTrainingJob' {algorithmSpecification} -> algorithmSpecification) (\s@CreateTrainingJob' {} a -> s {algorithmSpecification = a} :: CreateTrainingJob)

-- | The Amazon Resource Name (ARN) of an IAM role that SageMaker can assume
-- to perform tasks on your behalf.
--
-- During model training, SageMaker needs your permission to read input
-- data from an S3 bucket, download a Docker image that contains training
-- code, write model artifacts to an S3 bucket, write logs to Amazon
-- CloudWatch Logs, and publish metrics to Amazon CloudWatch. You grant
-- permissions for all of these tasks to an IAM role. For more information,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
--
-- To be able to pass this role to SageMaker, the caller of this API must
-- have the @iam:PassRole@ permission.
createTrainingJob_roleArn :: Lens.Lens' CreateTrainingJob Prelude.Text
createTrainingJob_roleArn = Lens.lens (\CreateTrainingJob' {roleArn} -> roleArn) (\s@CreateTrainingJob' {} a -> s {roleArn = a} :: CreateTrainingJob)

-- | Specifies the path to the S3 location where you want to store model
-- artifacts. SageMaker creates subfolders for the artifacts.
createTrainingJob_outputDataConfig :: Lens.Lens' CreateTrainingJob OutputDataConfig
createTrainingJob_outputDataConfig = Lens.lens (\CreateTrainingJob' {outputDataConfig} -> outputDataConfig) (\s@CreateTrainingJob' {} a -> s {outputDataConfig = a} :: CreateTrainingJob)

-- | The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
--
-- ML storage volumes store model artifacts and incremental states.
-- Training algorithms might also use ML storage volumes for scratch space.
-- If you want SageMaker to use the ML storage volume to store the training
-- data, choose @File@ as the @TrainingInputMode@ in the algorithm
-- specification. For distributed training algorithms, specify an instance
-- count greater than 1.
createTrainingJob_resourceConfig :: Lens.Lens' CreateTrainingJob ResourceConfig
createTrainingJob_resourceConfig = Lens.lens (\CreateTrainingJob' {resourceConfig} -> resourceConfig) (\s@CreateTrainingJob' {} a -> s {resourceConfig = a} :: CreateTrainingJob)

-- | Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, SageMaker sends the algorithm the @SIGTERM@ signal, which
-- delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts, so the results of
-- training are not lost.
createTrainingJob_stoppingCondition :: Lens.Lens' CreateTrainingJob StoppingCondition
createTrainingJob_stoppingCondition = Lens.lens (\CreateTrainingJob' {stoppingCondition} -> stoppingCondition) (\s@CreateTrainingJob' {} a -> s {stoppingCondition = a} :: CreateTrainingJob)

instance Core.AWSRequest CreateTrainingJob where
  type
    AWSResponse CreateTrainingJob =
      CreateTrainingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrainingJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "TrainingJobArn")
      )

instance Prelude.Hashable CreateTrainingJob where
  hashWithSalt _salt CreateTrainingJob' {..} =
    _salt `Prelude.hashWithSalt` checkpointConfig
      `Prelude.hashWithSalt` debugHookConfig
      `Prelude.hashWithSalt` debugRuleConfigurations
      `Prelude.hashWithSalt` enableInterContainerTrafficEncryption
      `Prelude.hashWithSalt` enableManagedSpotTraining
      `Prelude.hashWithSalt` enableNetworkIsolation
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` experimentConfig
      `Prelude.hashWithSalt` hyperParameters
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` profilerConfig
      `Prelude.hashWithSalt` profilerRuleConfigurations
      `Prelude.hashWithSalt` retryStrategy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tensorBoardOutputConfig
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` trainingJobName
      `Prelude.hashWithSalt` algorithmSpecification
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` stoppingCondition

instance Prelude.NFData CreateTrainingJob where
  rnf CreateTrainingJob' {..} =
    Prelude.rnf checkpointConfig
      `Prelude.seq` Prelude.rnf debugHookConfig
      `Prelude.seq` Prelude.rnf debugRuleConfigurations
      `Prelude.seq` Prelude.rnf enableInterContainerTrafficEncryption
      `Prelude.seq` Prelude.rnf enableManagedSpotTraining
      `Prelude.seq` Prelude.rnf enableNetworkIsolation
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf experimentConfig
      `Prelude.seq` Prelude.rnf hyperParameters
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf profilerConfig
      `Prelude.seq` Prelude.rnf profilerRuleConfigurations
      `Prelude.seq` Prelude.rnf retryStrategy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tensorBoardOutputConfig
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf trainingJobName
      `Prelude.seq` Prelude.rnf algorithmSpecification
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf
        stoppingCondition

instance Data.ToHeaders CreateTrainingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateTrainingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTrainingJob where
  toJSON CreateTrainingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CheckpointConfig" Data..=)
              Prelude.<$> checkpointConfig,
            ("DebugHookConfig" Data..=)
              Prelude.<$> debugHookConfig,
            ("DebugRuleConfigurations" Data..=)
              Prelude.<$> debugRuleConfigurations,
            ("EnableInterContainerTrafficEncryption" Data..=)
              Prelude.<$> enableInterContainerTrafficEncryption,
            ("EnableManagedSpotTraining" Data..=)
              Prelude.<$> enableManagedSpotTraining,
            ("EnableNetworkIsolation" Data..=)
              Prelude.<$> enableNetworkIsolation,
            ("Environment" Data..=) Prelude.<$> environment,
            ("ExperimentConfig" Data..=)
              Prelude.<$> experimentConfig,
            ("HyperParameters" Data..=)
              Prelude.<$> hyperParameters,
            ("InputDataConfig" Data..=)
              Prelude.<$> inputDataConfig,
            ("ProfilerConfig" Data..=)
              Prelude.<$> profilerConfig,
            ("ProfilerRuleConfigurations" Data..=)
              Prelude.<$> profilerRuleConfigurations,
            ("RetryStrategy" Data..=) Prelude.<$> retryStrategy,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TensorBoardOutputConfig" Data..=)
              Prelude.<$> tensorBoardOutputConfig,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just
              ("TrainingJobName" Data..= trainingJobName),
            Prelude.Just
              ( "AlgorithmSpecification"
                  Data..= algorithmSpecification
              ),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("ResourceConfig" Data..= resourceConfig),
            Prelude.Just
              ("StoppingCondition" Data..= stoppingCondition)
          ]
      )

instance Data.ToPath CreateTrainingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTrainingJob where
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

instance Prelude.NFData CreateTrainingJobResponse where
  rnf CreateTrainingJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trainingJobArn
