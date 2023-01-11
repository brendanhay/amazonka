{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pipes.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Lens
  ( -- * Operations

    -- ** CreatePipe
    createPipe_description,
    createPipe_desiredState,
    createPipe_enrichment,
    createPipe_enrichmentParameters,
    createPipe_sourceParameters,
    createPipe_tags,
    createPipe_targetParameters,
    createPipe_name,
    createPipe_roleArn,
    createPipe_source,
    createPipe_target,
    createPipeResponse_arn,
    createPipeResponse_creationTime,
    createPipeResponse_currentState,
    createPipeResponse_desiredState,
    createPipeResponse_lastModifiedTime,
    createPipeResponse_name,
    createPipeResponse_httpStatus,

    -- ** DeletePipe
    deletePipe_name,
    deletePipeResponse_arn,
    deletePipeResponse_creationTime,
    deletePipeResponse_currentState,
    deletePipeResponse_desiredState,
    deletePipeResponse_lastModifiedTime,
    deletePipeResponse_name,
    deletePipeResponse_httpStatus,

    -- ** DescribePipe
    describePipe_name,
    describePipeResponse_arn,
    describePipeResponse_creationTime,
    describePipeResponse_currentState,
    describePipeResponse_description,
    describePipeResponse_desiredState,
    describePipeResponse_enrichment,
    describePipeResponse_enrichmentParameters,
    describePipeResponse_lastModifiedTime,
    describePipeResponse_name,
    describePipeResponse_roleArn,
    describePipeResponse_source,
    describePipeResponse_sourceParameters,
    describePipeResponse_stateReason,
    describePipeResponse_tags,
    describePipeResponse_target,
    describePipeResponse_targetParameters,
    describePipeResponse_httpStatus,

    -- ** ListPipes
    listPipes_currentState,
    listPipes_desiredState,
    listPipes_limit,
    listPipes_namePrefix,
    listPipes_nextToken,
    listPipes_sourcePrefix,
    listPipes_targetPrefix,
    listPipesResponse_nextToken,
    listPipesResponse_pipes,
    listPipesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartPipe
    startPipe_name,
    startPipeResponse_arn,
    startPipeResponse_creationTime,
    startPipeResponse_currentState,
    startPipeResponse_desiredState,
    startPipeResponse_lastModifiedTime,
    startPipeResponse_name,
    startPipeResponse_httpStatus,

    -- ** StopPipe
    stopPipe_name,
    stopPipeResponse_arn,
    stopPipeResponse_creationTime,
    stopPipeResponse_currentState,
    stopPipeResponse_desiredState,
    stopPipeResponse_lastModifiedTime,
    stopPipeResponse_name,
    stopPipeResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdatePipe
    updatePipe_description,
    updatePipe_desiredState,
    updatePipe_enrichment,
    updatePipe_enrichmentParameters,
    updatePipe_sourceParameters,
    updatePipe_target,
    updatePipe_targetParameters,
    updatePipe_name,
    updatePipe_roleArn,
    updatePipeResponse_arn,
    updatePipeResponse_creationTime,
    updatePipeResponse_currentState,
    updatePipeResponse_desiredState,
    updatePipeResponse_lastModifiedTime,
    updatePipeResponse_name,
    updatePipeResponse_httpStatus,

    -- * Types

    -- ** AwsVpcConfiguration
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- ** BatchArrayProperties
    batchArrayProperties_size,

    -- ** BatchContainerOverrides
    batchContainerOverrides_command,
    batchContainerOverrides_environment,
    batchContainerOverrides_instanceType,
    batchContainerOverrides_resourceRequirements,

    -- ** BatchEnvironmentVariable
    batchEnvironmentVariable_name,
    batchEnvironmentVariable_value,

    -- ** BatchJobDependency
    batchJobDependency_jobId,
    batchJobDependency_type,

    -- ** BatchResourceRequirement
    batchResourceRequirement_type,
    batchResourceRequirement_value,

    -- ** BatchRetryStrategy
    batchRetryStrategy_attempts,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** DeadLetterConfig
    deadLetterConfig_arn,

    -- ** EcsContainerOverride
    ecsContainerOverride_command,
    ecsContainerOverride_cpu,
    ecsContainerOverride_environment,
    ecsContainerOverride_environmentFiles,
    ecsContainerOverride_memory,
    ecsContainerOverride_memoryReservation,
    ecsContainerOverride_name,
    ecsContainerOverride_resourceRequirements,

    -- ** EcsEnvironmentFile
    ecsEnvironmentFile_type,
    ecsEnvironmentFile_value,

    -- ** EcsEnvironmentVariable
    ecsEnvironmentVariable_name,
    ecsEnvironmentVariable_value,

    -- ** EcsEphemeralStorage
    ecsEphemeralStorage_sizeInGiB,

    -- ** EcsInferenceAcceleratorOverride
    ecsInferenceAcceleratorOverride_deviceName,
    ecsInferenceAcceleratorOverride_deviceType,

    -- ** EcsResourceRequirement
    ecsResourceRequirement_type,
    ecsResourceRequirement_value,

    -- ** EcsTaskOverride
    ecsTaskOverride_containerOverrides,
    ecsTaskOverride_cpu,
    ecsTaskOverride_ephemeralStorage,
    ecsTaskOverride_executionRoleArn,
    ecsTaskOverride_inferenceAcceleratorOverrides,
    ecsTaskOverride_memory,
    ecsTaskOverride_taskRoleArn,

    -- ** Filter
    filter_pattern,

    -- ** FilterCriteria
    filterCriteria_filters,

    -- ** MQBrokerAccessCredentials
    mQBrokerAccessCredentials_basicAuth,

    -- ** MSKAccessCredentials
    mSKAccessCredentials_clientCertificateTlsAuth,
    mSKAccessCredentials_saslScram512Auth,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** Pipe
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

    -- ** PipeEnrichmentHttpParameters
    pipeEnrichmentHttpParameters_headerParameters,
    pipeEnrichmentHttpParameters_pathParameterValues,
    pipeEnrichmentHttpParameters_queryStringParameters,

    -- ** PipeEnrichmentParameters
    pipeEnrichmentParameters_httpParameters,
    pipeEnrichmentParameters_inputTemplate,

    -- ** PipeSourceActiveMQBrokerParameters
    pipeSourceActiveMQBrokerParameters_batchSize,
    pipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds,
    pipeSourceActiveMQBrokerParameters_credentials,
    pipeSourceActiveMQBrokerParameters_queueName,

    -- ** PipeSourceDynamoDBStreamParameters
    pipeSourceDynamoDBStreamParameters_batchSize,
    pipeSourceDynamoDBStreamParameters_deadLetterConfig,
    pipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds,
    pipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds,
    pipeSourceDynamoDBStreamParameters_maximumRetryAttempts,
    pipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure,
    pipeSourceDynamoDBStreamParameters_parallelizationFactor,
    pipeSourceDynamoDBStreamParameters_startingPosition,

    -- ** PipeSourceKinesisStreamParameters
    pipeSourceKinesisStreamParameters_batchSize,
    pipeSourceKinesisStreamParameters_deadLetterConfig,
    pipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds,
    pipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds,
    pipeSourceKinesisStreamParameters_maximumRetryAttempts,
    pipeSourceKinesisStreamParameters_onPartialBatchItemFailure,
    pipeSourceKinesisStreamParameters_parallelizationFactor,
    pipeSourceKinesisStreamParameters_startingPositionTimestamp,
    pipeSourceKinesisStreamParameters_startingPosition,

    -- ** PipeSourceManagedStreamingKafkaParameters
    pipeSourceManagedStreamingKafkaParameters_batchSize,
    pipeSourceManagedStreamingKafkaParameters_consumerGroupID,
    pipeSourceManagedStreamingKafkaParameters_credentials,
    pipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds,
    pipeSourceManagedStreamingKafkaParameters_startingPosition,
    pipeSourceManagedStreamingKafkaParameters_topicName,

    -- ** PipeSourceParameters
    pipeSourceParameters_activeMQBrokerParameters,
    pipeSourceParameters_dynamoDBStreamParameters,
    pipeSourceParameters_filterCriteria,
    pipeSourceParameters_kinesisStreamParameters,
    pipeSourceParameters_managedStreamingKafkaParameters,
    pipeSourceParameters_rabbitMQBrokerParameters,
    pipeSourceParameters_selfManagedKafkaParameters,
    pipeSourceParameters_sqsQueueParameters,

    -- ** PipeSourceRabbitMQBrokerParameters
    pipeSourceRabbitMQBrokerParameters_batchSize,
    pipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds,
    pipeSourceRabbitMQBrokerParameters_virtualHost,
    pipeSourceRabbitMQBrokerParameters_credentials,
    pipeSourceRabbitMQBrokerParameters_queueName,

    -- ** PipeSourceSelfManagedKafkaParameters
    pipeSourceSelfManagedKafkaParameters_additionalBootstrapServers,
    pipeSourceSelfManagedKafkaParameters_batchSize,
    pipeSourceSelfManagedKafkaParameters_consumerGroupID,
    pipeSourceSelfManagedKafkaParameters_credentials,
    pipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds,
    pipeSourceSelfManagedKafkaParameters_serverRootCaCertificate,
    pipeSourceSelfManagedKafkaParameters_startingPosition,
    pipeSourceSelfManagedKafkaParameters_vpc,
    pipeSourceSelfManagedKafkaParameters_topicName,

    -- ** PipeSourceSqsQueueParameters
    pipeSourceSqsQueueParameters_batchSize,
    pipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds,

    -- ** PipeTargetBatchJobParameters
    pipeTargetBatchJobParameters_arrayProperties,
    pipeTargetBatchJobParameters_containerOverrides,
    pipeTargetBatchJobParameters_dependsOn,
    pipeTargetBatchJobParameters_parameters,
    pipeTargetBatchJobParameters_retryStrategy,
    pipeTargetBatchJobParameters_jobDefinition,
    pipeTargetBatchJobParameters_jobName,

    -- ** PipeTargetCloudWatchLogsParameters
    pipeTargetCloudWatchLogsParameters_logStreamName,
    pipeTargetCloudWatchLogsParameters_timestamp,

    -- ** PipeTargetEcsTaskParameters
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

    -- ** PipeTargetEventBridgeEventBusParameters
    pipeTargetEventBridgeEventBusParameters_detailType,
    pipeTargetEventBridgeEventBusParameters_endpointId,
    pipeTargetEventBridgeEventBusParameters_resources,
    pipeTargetEventBridgeEventBusParameters_source,
    pipeTargetEventBridgeEventBusParameters_time,

    -- ** PipeTargetHttpParameters
    pipeTargetHttpParameters_headerParameters,
    pipeTargetHttpParameters_pathParameterValues,
    pipeTargetHttpParameters_queryStringParameters,

    -- ** PipeTargetKinesisStreamParameters
    pipeTargetKinesisStreamParameters_partitionKey,

    -- ** PipeTargetLambdaFunctionParameters
    pipeTargetLambdaFunctionParameters_invocationType,

    -- ** PipeTargetParameters
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

    -- ** PipeTargetRedshiftDataParameters
    pipeTargetRedshiftDataParameters_dbUser,
    pipeTargetRedshiftDataParameters_secretManagerArn,
    pipeTargetRedshiftDataParameters_statementName,
    pipeTargetRedshiftDataParameters_withEvent,
    pipeTargetRedshiftDataParameters_database,
    pipeTargetRedshiftDataParameters_sqls,

    -- ** PipeTargetSageMakerPipelineParameters
    pipeTargetSageMakerPipelineParameters_pipelineParameterList,

    -- ** PipeTargetSqsQueueParameters
    pipeTargetSqsQueueParameters_messageDeduplicationId,
    pipeTargetSqsQueueParameters_messageGroupId,

    -- ** PipeTargetStateMachineParameters
    pipeTargetStateMachineParameters_invocationType,

    -- ** PlacementConstraint
    placementConstraint_expression,
    placementConstraint_type,

    -- ** PlacementStrategy
    placementStrategy_field,
    placementStrategy_type,

    -- ** SageMakerPipelineParameter
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- ** SelfManagedKafkaAccessConfigurationCredentials
    selfManagedKafkaAccessConfigurationCredentials_basicAuth,
    selfManagedKafkaAccessConfigurationCredentials_clientCertificateTlsAuth,
    selfManagedKafkaAccessConfigurationCredentials_saslScram256Auth,
    selfManagedKafkaAccessConfigurationCredentials_saslScram512Auth,

    -- ** SelfManagedKafkaAccessConfigurationVpc
    selfManagedKafkaAccessConfigurationVpc_securityGroup,
    selfManagedKafkaAccessConfigurationVpc_subnets,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UpdatePipeSourceActiveMQBrokerParameters
    updatePipeSourceActiveMQBrokerParameters_batchSize,
    updatePipeSourceActiveMQBrokerParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceActiveMQBrokerParameters_credentials,

    -- ** UpdatePipeSourceDynamoDBStreamParameters
    updatePipeSourceDynamoDBStreamParameters_batchSize,
    updatePipeSourceDynamoDBStreamParameters_deadLetterConfig,
    updatePipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds,
    updatePipeSourceDynamoDBStreamParameters_maximumRetryAttempts,
    updatePipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure,
    updatePipeSourceDynamoDBStreamParameters_parallelizationFactor,

    -- ** UpdatePipeSourceKinesisStreamParameters
    updatePipeSourceKinesisStreamParameters_batchSize,
    updatePipeSourceKinesisStreamParameters_deadLetterConfig,
    updatePipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds,
    updatePipeSourceKinesisStreamParameters_maximumRetryAttempts,
    updatePipeSourceKinesisStreamParameters_onPartialBatchItemFailure,
    updatePipeSourceKinesisStreamParameters_parallelizationFactor,

    -- ** UpdatePipeSourceManagedStreamingKafkaParameters
    updatePipeSourceManagedStreamingKafkaParameters_batchSize,
    updatePipeSourceManagedStreamingKafkaParameters_credentials,
    updatePipeSourceManagedStreamingKafkaParameters_maximumBatchingWindowInSeconds,

    -- ** UpdatePipeSourceParameters
    updatePipeSourceParameters_activeMQBrokerParameters,
    updatePipeSourceParameters_dynamoDBStreamParameters,
    updatePipeSourceParameters_filterCriteria,
    updatePipeSourceParameters_kinesisStreamParameters,
    updatePipeSourceParameters_managedStreamingKafkaParameters,
    updatePipeSourceParameters_rabbitMQBrokerParameters,
    updatePipeSourceParameters_selfManagedKafkaParameters,
    updatePipeSourceParameters_sqsQueueParameters,

    -- ** UpdatePipeSourceRabbitMQBrokerParameters
    updatePipeSourceRabbitMQBrokerParameters_batchSize,
    updatePipeSourceRabbitMQBrokerParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceRabbitMQBrokerParameters_credentials,

    -- ** UpdatePipeSourceSelfManagedKafkaParameters
    updatePipeSourceSelfManagedKafkaParameters_batchSize,
    updatePipeSourceSelfManagedKafkaParameters_credentials,
    updatePipeSourceSelfManagedKafkaParameters_maximumBatchingWindowInSeconds,
    updatePipeSourceSelfManagedKafkaParameters_serverRootCaCertificate,
    updatePipeSourceSelfManagedKafkaParameters_vpc,

    -- ** UpdatePipeSourceSqsQueueParameters
    updatePipeSourceSqsQueueParameters_batchSize,
    updatePipeSourceSqsQueueParameters_maximumBatchingWindowInSeconds,
  )
where

import Amazonka.Pipes.CreatePipe
import Amazonka.Pipes.DeletePipe
import Amazonka.Pipes.DescribePipe
import Amazonka.Pipes.ListPipes
import Amazonka.Pipes.ListTagsForResource
import Amazonka.Pipes.StartPipe
import Amazonka.Pipes.StopPipe
import Amazonka.Pipes.TagResource
import Amazonka.Pipes.Types.AwsVpcConfiguration
import Amazonka.Pipes.Types.BatchArrayProperties
import Amazonka.Pipes.Types.BatchContainerOverrides
import Amazonka.Pipes.Types.BatchEnvironmentVariable
import Amazonka.Pipes.Types.BatchJobDependency
import Amazonka.Pipes.Types.BatchResourceRequirement
import Amazonka.Pipes.Types.BatchRetryStrategy
import Amazonka.Pipes.Types.CapacityProviderStrategyItem
import Amazonka.Pipes.Types.DeadLetterConfig
import Amazonka.Pipes.Types.EcsContainerOverride
import Amazonka.Pipes.Types.EcsEnvironmentFile
import Amazonka.Pipes.Types.EcsEnvironmentVariable
import Amazonka.Pipes.Types.EcsEphemeralStorage
import Amazonka.Pipes.Types.EcsInferenceAcceleratorOverride
import Amazonka.Pipes.Types.EcsResourceRequirement
import Amazonka.Pipes.Types.EcsTaskOverride
import Amazonka.Pipes.Types.Filter
import Amazonka.Pipes.Types.FilterCriteria
import Amazonka.Pipes.Types.MQBrokerAccessCredentials
import Amazonka.Pipes.Types.MSKAccessCredentials
import Amazonka.Pipes.Types.NetworkConfiguration
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
import Amazonka.Pipes.Types.PipeTargetBatchJobParameters
import Amazonka.Pipes.Types.PipeTargetCloudWatchLogsParameters
import Amazonka.Pipes.Types.PipeTargetEcsTaskParameters
import Amazonka.Pipes.Types.PipeTargetEventBridgeEventBusParameters
import Amazonka.Pipes.Types.PipeTargetHttpParameters
import Amazonka.Pipes.Types.PipeTargetKinesisStreamParameters
import Amazonka.Pipes.Types.PipeTargetLambdaFunctionParameters
import Amazonka.Pipes.Types.PipeTargetParameters
import Amazonka.Pipes.Types.PipeTargetRedshiftDataParameters
import Amazonka.Pipes.Types.PipeTargetSageMakerPipelineParameters
import Amazonka.Pipes.Types.PipeTargetSqsQueueParameters
import Amazonka.Pipes.Types.PipeTargetStateMachineParameters
import Amazonka.Pipes.Types.PlacementConstraint
import Amazonka.Pipes.Types.PlacementStrategy
import Amazonka.Pipes.Types.SageMakerPipelineParameter
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationCredentials
import Amazonka.Pipes.Types.SelfManagedKafkaAccessConfigurationVpc
import Amazonka.Pipes.Types.Tag
import Amazonka.Pipes.Types.UpdatePipeSourceActiveMQBrokerParameters
import Amazonka.Pipes.Types.UpdatePipeSourceDynamoDBStreamParameters
import Amazonka.Pipes.Types.UpdatePipeSourceKinesisStreamParameters
import Amazonka.Pipes.Types.UpdatePipeSourceManagedStreamingKafkaParameters
import Amazonka.Pipes.Types.UpdatePipeSourceParameters
import Amazonka.Pipes.Types.UpdatePipeSourceRabbitMQBrokerParameters
import Amazonka.Pipes.Types.UpdatePipeSourceSelfManagedKafkaParameters
import Amazonka.Pipes.Types.UpdatePipeSourceSqsQueueParameters
import Amazonka.Pipes.UntagResource
import Amazonka.Pipes.UpdatePipe
