{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pipes.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalException,
    _NotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AssignPublicIp
    AssignPublicIp (..),

    -- * BatchJobDependencyType
    BatchJobDependencyType (..),

    -- * BatchResourceRequirementType
    BatchResourceRequirementType (..),

    -- * DynamoDBStreamStartPosition
    DynamoDBStreamStartPosition (..),

    -- * EcsEnvironmentFileType
    EcsEnvironmentFileType (..),

    -- * EcsResourceRequirementType
    EcsResourceRequirementType (..),

    -- * KinesisStreamStartPosition
    KinesisStreamStartPosition (..),

    -- * LaunchType
    LaunchType (..),

    -- * MSKStartPosition
    MSKStartPosition (..),

    -- * OnPartialBatchItemFailureStreams
    OnPartialBatchItemFailureStreams (..),

    -- * PipeState
    PipeState (..),

    -- * PipeTargetInvocationType
    PipeTargetInvocationType (..),

    -- * PlacementConstraintType
    PlacementConstraintType (..),

    -- * PlacementStrategyType
    PlacementStrategyType (..),

    -- * PropagateTags
    PropagateTags (..),

    -- * RequestedPipeState
    RequestedPipeState (..),

    -- * RequestedPipeStateDescribeResponse
    RequestedPipeStateDescribeResponse (..),

    -- * SelfManagedKafkaStartPosition
    SelfManagedKafkaStartPosition (..),

    -- * AwsVpcConfiguration
    AwsVpcConfiguration (..),
    newAwsVpcConfiguration,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- * BatchArrayProperties
    BatchArrayProperties (..),
    newBatchArrayProperties,
    batchArrayProperties_size,

    -- * BatchContainerOverrides
    BatchContainerOverrides (..),
    newBatchContainerOverrides,
    batchContainerOverrides_command,
    batchContainerOverrides_environment,
    batchContainerOverrides_instanceType,
    batchContainerOverrides_resourceRequirements,

    -- * BatchEnvironmentVariable
    BatchEnvironmentVariable (..),
    newBatchEnvironmentVariable,
    batchEnvironmentVariable_name,
    batchEnvironmentVariable_value,

    -- * BatchJobDependency
    BatchJobDependency (..),
    newBatchJobDependency,
    batchJobDependency_jobId,
    batchJobDependency_type,

    -- * BatchResourceRequirement
    BatchResourceRequirement (..),
    newBatchResourceRequirement,
    batchResourceRequirement_type,
    batchResourceRequirement_value,

    -- * BatchRetryStrategy
    BatchRetryStrategy (..),
    newBatchRetryStrategy,
    batchRetryStrategy_attempts,

    -- * CapacityProviderStrategyItem
    CapacityProviderStrategyItem (..),
    newCapacityProviderStrategyItem,
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- * DeadLetterConfig
    DeadLetterConfig (..),
    newDeadLetterConfig,
    deadLetterConfig_arn,

    -- * EcsContainerOverride
    EcsContainerOverride (..),
    newEcsContainerOverride,
    ecsContainerOverride_command,
    ecsContainerOverride_cpu,
    ecsContainerOverride_environment,
    ecsContainerOverride_environmentFiles,
    ecsContainerOverride_memory,
    ecsContainerOverride_memoryReservation,
    ecsContainerOverride_name,
    ecsContainerOverride_resourceRequirements,

    -- * EcsEnvironmentFile
    EcsEnvironmentFile (..),
    newEcsEnvironmentFile,
    ecsEnvironmentFile_type,
    ecsEnvironmentFile_value,

    -- * EcsEnvironmentVariable
    EcsEnvironmentVariable (..),
    newEcsEnvironmentVariable,
    ecsEnvironmentVariable_name,
    ecsEnvironmentVariable_value,

    -- * EcsEphemeralStorage
    EcsEphemeralStorage (..),
    newEcsEphemeralStorage,
    ecsEphemeralStorage_sizeInGiB,

    -- * EcsInferenceAcceleratorOverride
    EcsInferenceAcceleratorOverride (..),
    newEcsInferenceAcceleratorOverride,
    ecsInferenceAcceleratorOverride_deviceName,
    ecsInferenceAcceleratorOverride_deviceType,

    -- * EcsResourceRequirement
    EcsResourceRequirement (..),
    newEcsResourceRequirement,
    ecsResourceRequirement_type,
    ecsResourceRequirement_value,

    -- * EcsTaskOverride
    EcsTaskOverride (..),
    newEcsTaskOverride,
    ecsTaskOverride_containerOverrides,
    ecsTaskOverride_cpu,
    ecsTaskOverride_ephemeralStorage,
    ecsTaskOverride_executionRoleArn,
    ecsTaskOverride_inferenceAcceleratorOverrides,
    ecsTaskOverride_memory,
    ecsTaskOverride_taskRoleArn,

    -- * Filter
    Filter (..),
    newFilter,
    filter_pattern,

    -- * FilterCriteria
    FilterCriteria (..),
    newFilterCriteria,
    filterCriteria_filters,

    -- * MQBrokerAccessCredentials
    MQBrokerAccessCredentials (..),
    newMQBrokerAccessCredentials,
    mQBrokerAccessCredentials_basicAuth,

    -- * MSKAccessCredentials
    MSKAccessCredentials (..),
    newMSKAccessCredentials,
    mSKAccessCredentials_clientCertificateTlsAuth,
    mSKAccessCredentials_saslScram512Auth,

    -- * NetworkConfiguration
    NetworkConfiguration (..),
    newNetworkConfiguration,
    networkConfiguration_awsvpcConfiguration,

    -- * Pipe
    Pipe (..),
    newPipe,
    pipe_arn,
    pipe_creationTime,
    pipe_currentState,
    pipe_desiredState,
    pipe_enrichment,
    pipe_lastModifiedTime,
    pipe_name,
    pipe_source,
    pipe_stateReason,
    pipe_target,

    -- * PipeEnrichmentHttpParameters
    PipeEnrichmentHttpParameters (..),
    newPipeEnrichmentHttpParameters,
    pipeEnrichmentHttpParameters_headerParameters,
    pipeEnrichmentHttpParameters_pathParameterValues,
    pipeEnrichmentHttpParameters_queryStringParameters,

    -- * PipeEnrichmentParameters
    PipeEnrichmentParameters (..),
    newPipeEnrichmentParameters,
    pipeEnrichmentParameters_httpParameters,
    pipeEnrichmentParameters_inputTemplate,

    -- * PipeSourceActiveMQBrokerParameters
    PipeSourceActiveMQBrokerParameters (..),
    newPipeSourceActiveMQBrokerParameters,
    pipeSourceActiveMQBrokerParameters_batchSize,
    pipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds,
    pipeSourceActiveMQBrokerParameters_credentials,
    pipeSourceActiveMQBrokerParameters_queueName,

    -- * PipeSourceDynamoDBStreamParameters
    PipeSourceDynamoDBStreamParameters (..),
    newPipeSourceDynamoDBStreamParameters,
    pipeSourceDynamoDBStreamParameters_batchSize,
    pipeSourceDynamoDBStreamParameters_deadLetterConfig,
    pipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds,
    pipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds,
    pipeSourceDynamoDBStreamParameters_maximumRetryAttempts,
    pipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure,
    pipeSourceDynamoDBStreamParameters_parallelizationFactor,
    pipeSourceDynamoDBStreamParameters_startingPosition,

    -- * PipeSourceKinesisStreamParameters
    PipeSourceKinesisStreamParameters (..),
    newPipeSourceKinesisStreamParameters,
    pipeSourceKinesisStreamParameters_batchSize,
    pipeSourceKinesisStreamParameters_deadLetterConfig,
    pipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds,
    pipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds,
    pipeSourceKinesisStreamParameters_maximumRetryAttempts,
    pipeSourceKinesisStreamParameters_onPartialBatchItemFailure,
    pipeSourceKinesisStreamParameters_parallelizationFactor,
    pipeSourceKinesisStreamParameters_startingPositionTimestamp,
    pipeSourceKinesisStreamParameters_startingPosition,

    -- * PipeSourceManagedStreamingKafkaParameters
    PipeSourceManagedStreamingKafkaParameters (..),
    newPipeSourceManagedStreamingKafkaParameters,
    pipeSourceManagedStreamingKafkaParameters_batchSize,
    pipeSourceManagedStreamingKafkaParameters_consumerGroupID,
    pipeSourceManagedStreamingKafkaParameters_credentials,
    pipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds,
    pipeSourceManagedStreamingKafkaParameters_startingPosition,
    pipeSourceManagedStreamingKafkaParameters_topicName,

    -- * PipeSourceParameters
    PipeSourceParameters (..),
    newPipeSourceParameters,
    pipeSourceParameters_activeMQBrokerParameters,
    pipeSourceParameters_dynamoDBStreamParameters,
    pipeSourceParameters_filterCriteria,
    pipeSourceParameters_kinesisStreamParameters,
    pipeSourceParameters_managedStreamingKafkaParameters,
    pipeSourceParameters_rabbitMQBrokerParameters,
    pipeSourceParameters_selfManagedKafkaParameters,
    pipeSourceParameters_sqsQueueParameters,

    -- * PipeSourceRabbitMQBrokerParameters
    PipeSourceRabbitMQBrokerParameters (..),
    newPipeSourceRabbitMQBrokerParameters,
    pipeSourceRabbitMQBrokerParameters_batchSize,
    pipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds,
    pipeSourceRabbitMQBrokerParameters_virtualHost,
    pipeSourceRabbitMQBrokerParameters_credentials,
    pipeSourceRabbitMQBrokerParameters_queueName,

    -- * PipeSourceSelfManagedKafkaParameters
    PipeSourceSelfManagedKafkaParameters (..),
    newPipeSourceSelfManagedKafkaParameters,
    pipeSourceSelfManagedKafkaParameters_additionalBootstrapServers,
    pipeSourceSelfManagedKafkaParameters_batchSize,
    pipeSourceSelfManagedKafkaParameters_consumerGroupID,
    pipeSourceSelfManagedKafkaParameters_credentials,
    pipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds,
    pipeSourceSelfManagedKafkaParameters_serverRootCaCertificate,
    pipeSourceSelfManagedKafkaParameters_startingPosition,
    pipeSourceSelfManagedKafkaParameters_vpc,
    pipeSourceSelfManagedKafkaParameters_topicName,

    -- * PipeSourceSqsQueueParameters
    PipeSourceSqsQueueParameters (..),
    newPipeSourceSqsQueueParameters,
    pipeSourceSqsQueueParameters_batchSize,
    pipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds,

    -- * PipeTargetBatchJobParameters
    PipeTargetBatchJobParameters (..),
    newPipeTargetBatchJobParameters,
    pipeTargetBatchJobParameters_arrayProperties,
    pipeTargetBatchJobParameters_containerOverrides,
    pipeTargetBatchJobParameters_dependsOn,
    pipeTargetBatchJobParameters_parameters,
    pipeTargetBatchJobParameters_retryStrategy,
    pipeTargetBatchJobParameters_jobDefinition,
    pipeTargetBatchJobParameters_jobName,

    -- * PipeTargetCloudWatchLogsParameters
    PipeTargetCloudWatchLogsParameters (..),
    newPipeTargetCloudWatchLogsParameters,
    pipeTargetCloudWatchLogsParameters_logStreamName,
    pipeTargetCloudWatchLogsParameters_timestamp,

    -- * PipeTargetEcsTaskParameters
    PipeTargetEcsTaskParameters (..),
    newPipeTargetEcsTaskParameters,
    pipeTargetEcsTaskParameters_capacityProviderStrategy,
    pipeTargetEcsTaskParameters_enableECSManagedTags,
    pipeTargetEcsTaskParameters_enableExecuteCommand,
    pipeTargetEcsTaskParameters_group,
    pipeTargetEcsTaskParameters_launchType,
    pipeTargetEcsTaskParameters_networkConfiguration,
    pipeTargetEcsTaskParameters_overrides,
    pipeTargetEcsTaskParameters_placementConstraints,
    pipeTargetEcsTaskParameters_placementStrategy,
    pipeTargetEcsTaskParameters_platformVersion,
    pipeTargetEcsTaskParameters_propagateTags,
    pipeTargetEcsTaskParameters_referenceId,
    pipeTargetEcsTaskParameters_tags,
    pipeTargetEcsTaskParameters_taskCount,
    pipeTargetEcsTaskParameters_taskDefinitionArn,

    -- * PipeTargetEventBridgeEventBusParameters
    PipeTargetEventBridgeEventBusParameters (..),
    newPipeTargetEventBridgeEventBusParameters,
    pipeTargetEventBridgeEventBusParameters_detailType,
    pipeTargetEventBridgeEventBusParameters_endpointId,
    pipeTargetEventBridgeEventBusParameters_resources,
    pipeTargetEventBridgeEventBusParameters_source,
    pipeTargetEventBridgeEventBusParameters_time,

    -- * PipeTargetHttpParameters
    PipeTargetHttpParameters (..),
    newPipeTargetHttpParameters,
    pipeTargetHttpParameters_headerParameters,
    pipeTargetHttpParameters_pathParameterValues,
    pipeTargetHttpParameters_queryStringParameters,

    -- * PipeTargetKinesisStreamParameters
    PipeTargetKinesisStreamParameters (..),
    newPipeTargetKinesisStreamParameters,
    pipeTargetKinesisStreamParameters_partitionKey,

    -- * PipeTargetLambdaFunctionParameters
    PipeTargetLambdaFunctionParameters (..),
    newPipeTargetLambdaFunctionParameters,
    pipeTargetLambdaFunctionParameters_invocationType,

    -- * PipeTargetParameters
    PipeTargetParameters (..),
    newPipeTargetParameters,
    pipeTargetParameters_batchJobParameters,
    pipeTargetParameters_cloudWatchLogsParameters,
    pipeTargetParameters_ecsTaskParameters,
    pipeTargetParameters_eventBridgeEventBusParameters,
    pipeTargetParameters_httpParameters,
    pipeTargetParameters_inputTemplate,
    pipeTargetParameters_kinesisStreamParameters,
    pipeTargetParameters_lambdaFunctionParameters,
    pipeTargetParameters_redshiftDataParameters,
    pipeTargetParameters_sageMakerPipelineParameters,
    pipeTargetParameters_sqsQueueParameters,
    pipeTargetParameters_stepFunctionStateMachineParameters,

    -- * PipeTargetRedshiftDataParameters
    PipeTargetRedshiftDataParameters (..),
    newPipeTargetRedshiftDataParameters,
    pipeTargetRedshiftDataParameters_dbUser,
    pipeTargetRedshiftDataParameters_secretManagerArn,
    pipeTargetRedshiftDataParameters_statementName,
    pipeTargetRedshiftDataParameters_withEvent,
    pipeTargetRedshiftDataParameters_database,
    pipeTargetRedshiftDataParameters_sqls,

    -- * PipeTargetSageMakerPipelineParameters
    PipeTargetSageMakerPipelineParameters (..),
    newPipeTargetSageMakerPipelineParameters,
    pipeTargetSageMakerPipelineParameters_pipelineParameterList,

    -- * PipeTargetSqsQueueParameters
    PipeTargetSqsQueueParameters (..),
    newPipeTargetSqsQueueParameters,
    pipeTargetSqsQueueParameters_messageDeduplicationId,
    pipeTargetSqsQueueParameters_messageGroupId,

    -- * PipeTargetStateMachineParameters
    PipeTargetStateMachineParameters (..),
    newPipeTargetStateMachineParameters,
    pipeTargetStateMachineParameters_invocationType,

    -- * PlacementConstraint
    PlacementConstraint (..),
    newPlacementConstraint,
    placementConstraint_expression,
    placementConstraint_type,

    -- * PlacementStrategy
    PlacementStrategy (..),
    newPlacementStrategy,
    placementStrategy_field,
    placementStrategy_type,

    -- * SageMakerPipelineParameter
    SageMakerPipelineParameter (..),
    newSageMakerPipelineParameter,
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- * SelfManagedKafkaAccessConfigurationCredentials
    SelfManagedKafkaAccessConfigurationCredentials (..),
    newSelfManagedKafkaAccessConfigurationCredentials,
    selfManagedKafkaAccessConfigurationCredentials_basicAuth,
    selfManagedKafkaAccessConfigurationCredentials_clientCertificateTlsAuth,
    selfManagedKafkaAccessConfigurationCredentials_saslScram256Auth,
    selfManagedKafkaAccessConfigurationCredentials_saslScram512Auth,

    -- * SelfManagedKafkaAccessConfigurationVpc
    SelfManagedKafkaAccessConfigurationVpc (..),
    newSelfManagedKafkaAccessConfigurationVpc,
    selfManagedKafkaAccessConfigurationVpc_securityGroup,
    selfManagedKafkaAccessConfigurationVpc_subnets,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpdatePipeSourceActiveMQBrokerParameters
    UpdatePipeSourceActiveMQBrokerParameters (..),
    newUpdatePipeSourceActiveMQBrokerParameters,
    updatePipeSourceActiveMQBrokerParameters_batchSize,
    updatePipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceActiveMQBrokerParameters_credentials,

    -- * UpdatePipeSourceDynamoDBStreamParameters
    UpdatePipeSourceDynamoDBStreamParameters (..),
    newUpdatePipeSourceDynamoDBStreamParameters,
    updatePipeSourceDynamoDBStreamParameters_batchSize,
    updatePipeSourceDynamoDBStreamParameters_deadLetterConfig,
    updatePipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds,
    updatePipeSourceDynamoDBStreamParameters_maximumRetryAttempts,
    updatePipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure,
    updatePipeSourceDynamoDBStreamParameters_parallelizationFactor,

    -- * UpdatePipeSourceKinesisStreamParameters
    UpdatePipeSourceKinesisStreamParameters (..),
    newUpdatePipeSourceKinesisStreamParameters,
    updatePipeSourceKinesisStreamParameters_batchSize,
    updatePipeSourceKinesisStreamParameters_deadLetterConfig,
    updatePipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds,
    updatePipeSourceKinesisStreamParameters_maximumRetryAttempts,
    updatePipeSourceKinesisStreamParameters_onPartialBatchItemFailure,
    updatePipeSourceKinesisStreamParameters_parallelizationFactor,

    -- * UpdatePipeSourceManagedStreamingKafkaParameters
    UpdatePipeSourceManagedStreamingKafkaParameters (..),
    newUpdatePipeSourceManagedStreamingKafkaParameters,
    updatePipeSourceManagedStreamingKafkaParameters_batchSize,
    updatePipeSourceManagedStreamingKafkaParameters_credentials,
    updatePipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds,

    -- * UpdatePipeSourceParameters
    UpdatePipeSourceParameters (..),
    newUpdatePipeSourceParameters,
    updatePipeSourceParameters_activeMQBrokerParameters,
    updatePipeSourceParameters_dynamoDBStreamParameters,
    updatePipeSourceParameters_filterCriteria,
    updatePipeSourceParameters_kinesisStreamParameters,
    updatePipeSourceParameters_managedStreamingKafkaParameters,
    updatePipeSourceParameters_rabbitMQBrokerParameters,
    updatePipeSourceParameters_selfManagedKafkaParameters,
    updatePipeSourceParameters_sqsQueueParameters,

    -- * UpdatePipeSourceRabbitMQBrokerParameters
    UpdatePipeSourceRabbitMQBrokerParameters (..),
    newUpdatePipeSourceRabbitMQBrokerParameters,
    updatePipeSourceRabbitMQBrokerParameters_batchSize,
    updatePipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceRabbitMQBrokerParameters_credentials,

    -- * UpdatePipeSourceSelfManagedKafkaParameters
    UpdatePipeSourceSelfManagedKafkaParameters (..),
    newUpdatePipeSourceSelfManagedKafkaParameters,
    updatePipeSourceSelfManagedKafkaParameters_batchSize,
    updatePipeSourceSelfManagedKafkaParameters_credentials,
    updatePipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceSelfManagedKafkaParameters_serverRootCaCertificate,
    updatePipeSourceSelfManagedKafkaParameters_vpc,

    -- * UpdatePipeSourceSqsQueueParameters
    UpdatePipeSourceSqsQueueParameters (..),
    newUpdatePipeSourceSqsQueueParameters,
    updatePipeSourceSqsQueueParameters_batchSize,
    updatePipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Pipes.Types.AssignPublicIp
import Amazonka.Pipes.Types.AwsVpcConfiguration
import Amazonka.Pipes.Types.BatchArrayProperties
import Amazonka.Pipes.Types.BatchContainerOverrides
import Amazonka.Pipes.Types.BatchEnvironmentVariable
import Amazonka.Pipes.Types.BatchJobDependency
import Amazonka.Pipes.Types.BatchJobDependencyType
import Amazonka.Pipes.Types.BatchResourceRequirement
import Amazonka.Pipes.Types.BatchResourceRequirementType
import Amazonka.Pipes.Types.BatchRetryStrategy
import Amazonka.Pipes.Types.CapacityProviderStrategyItem
import Amazonka.Pipes.Types.DeadLetterConfig
import Amazonka.Pipes.Types.DynamoDBStreamStartPosition
import Amazonka.Pipes.Types.EcsContainerOverride
import Amazonka.Pipes.Types.EcsEnvironmentFile
import Amazonka.Pipes.Types.EcsEnvironmentFileType
import Amazonka.Pipes.Types.EcsEnvironmentVariable
import Amazonka.Pipes.Types.EcsEphemeralStorage
import Amazonka.Pipes.Types.EcsInferenceAcceleratorOverride
import Amazonka.Pipes.Types.EcsResourceRequirement
import Amazonka.Pipes.Types.EcsResourceRequirementType
import Amazonka.Pipes.Types.EcsTaskOverride
import Amazonka.Pipes.Types.Filter
import Amazonka.Pipes.Types.FilterCriteria
import Amazonka.Pipes.Types.KinesisStreamStartPosition
import Amazonka.Pipes.Types.LaunchType
import Amazonka.Pipes.Types.MQBrokerAccessCredentials
import Amazonka.Pipes.Types.MSKAccessCredentials
import Amazonka.Pipes.Types.MSKStartPosition
import Amazonka.Pipes.Types.NetworkConfiguration
import Amazonka.Pipes.Types.OnPartialBatchItemFailureStreams
import Amazonka.Pipes.Types.Pipe
import Amazonka.Pipes.Types.PipeEnrichmentHttpParameters
import Amazonka.Pipes.Types.PipeEnrichmentParameters
import Amazonka.Pipes.Types.PipeSourceActiveMQBrokerParameters
import Amazonka.Pipes.Types.PipeSourceDynamoDBStreamParameters
import Amazonka.Pipes.Types.PipeSourceKinesisStreamParameters
import Amazonka.Pipes.Types.PipeSourceManagedStreamingKafkaParameters
import Amazonka.Pipes.Types.PipeSourceParameters
import Amazonka.Pipes.Types.PipeSourceRabbitMQBrokerParameters
import Amazonka.Pipes.Types.PipeSourceSelfManagedKafkaParameters
import Amazonka.Pipes.Types.PipeSourceSqsQueueParameters
import Amazonka.Pipes.Types.PipeState
import Amazonka.Pipes.Types.PipeTargetBatchJobParameters
import Amazonka.Pipes.Types.PipeTargetCloudWatchLogsParameters
import Amazonka.Pipes.Types.PipeTargetEcsTaskParameters
import Amazonka.Pipes.Types.PipeTargetEventBridgeEventBusParameters
import Amazonka.Pipes.Types.PipeTargetHttpParameters
import Amazonka.Pipes.Types.PipeTargetInvocationType
import Amazonka.Pipes.Types.PipeTargetKinesisStreamParameters
import Amazonka.Pipes.Types.PipeTargetLambdaFunctionParameters
import Amazonka.Pipes.Types.PipeTargetParameters
import Amazonka.Pipes.Types.PipeTargetRedshiftDataParameters
import Amazonka.Pipes.Types.PipeTargetSageMakerPipelineParameters
import Amazonka.Pipes.Types.PipeTargetSqsQueueParameters
import Amazonka.Pipes.Types.PipeTargetStateMachineParameters
import Amazonka.Pipes.Types.PlacementConstraint
import Amazonka.Pipes.Types.PlacementConstraintType
import Amazonka.Pipes.Types.PlacementStrategy
import Amazonka.Pipes.Types.PlacementStrategyType
import Amazonka.Pipes.Types.PropagateTags
import Amazonka.Pipes.Types.RequestedPipeState
import Amazonka.Pipes.Types.RequestedPipeStateDescribeResponse
import Amazonka.Pipes.Types.SageMakerPipelineParameter
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationCredentials
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationVpc
import Amazonka.Pipes.Types.SelfManagedKafkaStartPosition
import Amazonka.Pipes.Types.Tag
import Amazonka.Pipes.Types.UpdatePipeSourceActiveMQBrokerParameters
import Amazonka.Pipes.Types.UpdatePipeSourceDynamoDBStreamParameters
import Amazonka.Pipes.Types.UpdatePipeSourceKinesisStreamParameters
import Amazonka.Pipes.Types.UpdatePipeSourceManagedStreamingKafkaParameters
import Amazonka.Pipes.Types.UpdatePipeSourceParameters
import Amazonka.Pipes.Types.UpdatePipeSourceRabbitMQBrokerParameters
import Amazonka.Pipes.Types.UpdatePipeSourceSelfManagedKafkaParameters
import Amazonka.Pipes.Types.UpdatePipeSourceSqsQueueParameters
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-10-07@ of the Amazon EventBridge Pipes SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Pipes",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "pipes",
      Core.signingName = "pipes",
      Core.version = "2015-10-07",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Pipes",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An action you attempted resulted in an exception.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | This exception occurs due to unexpected causes.
_InternalException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | An entity that you specified does not exist.
_NotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | A quota has been exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | An action was throttled.
_ThrottlingException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Indicates that an error has occurred while performing a validate
-- operation.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
