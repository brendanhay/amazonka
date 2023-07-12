{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Pipes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-10-07@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EventBridge Pipes connects event sources to targets. Pipes
-- reduces the need for specialized knowledge and integration code when
-- developing event driven architectures. This helps ensures consistency
-- across your company’s applications. With Pipes, the target can be any
-- available EventBridge target. To set up a pipe, you select the event
-- source, add optional event filtering, define optional enrichment, and
-- select the target for the event data.
module Amazonka.Pipes
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** InternalException
    _InternalException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreatePipe
    CreatePipe (CreatePipe'),
    newCreatePipe,
    CreatePipeResponse (CreatePipeResponse'),
    newCreatePipeResponse,

    -- ** DeletePipe
    DeletePipe (DeletePipe'),
    newDeletePipe,
    DeletePipeResponse (DeletePipeResponse'),
    newDeletePipeResponse,

    -- ** DescribePipe
    DescribePipe (DescribePipe'),
    newDescribePipe,
    DescribePipeResponse (DescribePipeResponse'),
    newDescribePipeResponse,

    -- ** ListPipes (Paginated)
    ListPipes (ListPipes'),
    newListPipes,
    ListPipesResponse (ListPipesResponse'),
    newListPipesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartPipe
    StartPipe (StartPipe'),
    newStartPipe,
    StartPipeResponse (StartPipeResponse'),
    newStartPipeResponse,

    -- ** StopPipe
    StopPipe (StopPipe'),
    newStopPipe,
    StopPipeResponse (StopPipeResponse'),
    newStopPipeResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdatePipe
    UpdatePipe (UpdatePipe'),
    newUpdatePipe,
    UpdatePipeResponse (UpdatePipeResponse'),
    newUpdatePipeResponse,

    -- * Types

    -- ** AssignPublicIp
    AssignPublicIp (..),

    -- ** BatchJobDependencyType
    BatchJobDependencyType (..),

    -- ** BatchResourceRequirementType
    BatchResourceRequirementType (..),

    -- ** DynamoDBStreamStartPosition
    DynamoDBStreamStartPosition (..),

    -- ** EcsEnvironmentFileType
    EcsEnvironmentFileType (..),

    -- ** EcsResourceRequirementType
    EcsResourceRequirementType (..),

    -- ** KinesisStreamStartPosition
    KinesisStreamStartPosition (..),

    -- ** LaunchType
    LaunchType (..),

    -- ** MSKStartPosition
    MSKStartPosition (..),

    -- ** OnPartialBatchItemFailureStreams
    OnPartialBatchItemFailureStreams (..),

    -- ** PipeState
    PipeState (..),

    -- ** PipeTargetInvocationType
    PipeTargetInvocationType (..),

    -- ** PlacementConstraintType
    PlacementConstraintType (..),

    -- ** PlacementStrategyType
    PlacementStrategyType (..),

    -- ** PropagateTags
    PropagateTags (..),

    -- ** RequestedPipeState
    RequestedPipeState (..),

    -- ** RequestedPipeStateDescribeResponse
    RequestedPipeStateDescribeResponse (..),

    -- ** SelfManagedKafkaStartPosition
    SelfManagedKafkaStartPosition (..),

    -- ** AwsVpcConfiguration
    AwsVpcConfiguration (AwsVpcConfiguration'),
    newAwsVpcConfiguration,

    -- ** BatchArrayProperties
    BatchArrayProperties (BatchArrayProperties'),
    newBatchArrayProperties,

    -- ** BatchContainerOverrides
    BatchContainerOverrides (BatchContainerOverrides'),
    newBatchContainerOverrides,

    -- ** BatchEnvironmentVariable
    BatchEnvironmentVariable (BatchEnvironmentVariable'),
    newBatchEnvironmentVariable,

    -- ** BatchJobDependency
    BatchJobDependency (BatchJobDependency'),
    newBatchJobDependency,

    -- ** BatchResourceRequirement
    BatchResourceRequirement (BatchResourceRequirement'),
    newBatchResourceRequirement,

    -- ** BatchRetryStrategy
    BatchRetryStrategy (BatchRetryStrategy'),
    newBatchRetryStrategy,

    -- ** CapacityProviderStrategyItem
    CapacityProviderStrategyItem (CapacityProviderStrategyItem'),
    newCapacityProviderStrategyItem,

    -- ** DeadLetterConfig
    DeadLetterConfig (DeadLetterConfig'),
    newDeadLetterConfig,

    -- ** EcsContainerOverride
    EcsContainerOverride (EcsContainerOverride'),
    newEcsContainerOverride,

    -- ** EcsEnvironmentFile
    EcsEnvironmentFile (EcsEnvironmentFile'),
    newEcsEnvironmentFile,

    -- ** EcsEnvironmentVariable
    EcsEnvironmentVariable (EcsEnvironmentVariable'),
    newEcsEnvironmentVariable,

    -- ** EcsEphemeralStorage
    EcsEphemeralStorage (EcsEphemeralStorage'),
    newEcsEphemeralStorage,

    -- ** EcsInferenceAcceleratorOverride
    EcsInferenceAcceleratorOverride (EcsInferenceAcceleratorOverride'),
    newEcsInferenceAcceleratorOverride,

    -- ** EcsResourceRequirement
    EcsResourceRequirement (EcsResourceRequirement'),
    newEcsResourceRequirement,

    -- ** EcsTaskOverride
    EcsTaskOverride (EcsTaskOverride'),
    newEcsTaskOverride,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FilterCriteria
    FilterCriteria (FilterCriteria'),
    newFilterCriteria,

    -- ** MQBrokerAccessCredentials
    MQBrokerAccessCredentials (MQBrokerAccessCredentials'),
    newMQBrokerAccessCredentials,

    -- ** MSKAccessCredentials
    MSKAccessCredentials (MSKAccessCredentials'),
    newMSKAccessCredentials,

    -- ** NetworkConfiguration
    NetworkConfiguration (NetworkConfiguration'),
    newNetworkConfiguration,

    -- ** Pipe
    Pipe (Pipe'),
    newPipe,

    -- ** PipeEnrichmentHttpParameters
    PipeEnrichmentHttpParameters (PipeEnrichmentHttpParameters'),
    newPipeEnrichmentHttpParameters,

    -- ** PipeEnrichmentParameters
    PipeEnrichmentParameters (PipeEnrichmentParameters'),
    newPipeEnrichmentParameters,

    -- ** PipeSourceActiveMQBrokerParameters
    PipeSourceActiveMQBrokerParameters (PipeSourceActiveMQBrokerParameters'),
    newPipeSourceActiveMQBrokerParameters,

    -- ** PipeSourceDynamoDBStreamParameters
    PipeSourceDynamoDBStreamParameters (PipeSourceDynamoDBStreamParameters'),
    newPipeSourceDynamoDBStreamParameters,

    -- ** PipeSourceKinesisStreamParameters
    PipeSourceKinesisStreamParameters (PipeSourceKinesisStreamParameters'),
    newPipeSourceKinesisStreamParameters,

    -- ** PipeSourceManagedStreamingKafkaParameters
    PipeSourceManagedStreamingKafkaParameters (PipeSourceManagedStreamingKafkaParameters'),
    newPipeSourceManagedStreamingKafkaParameters,

    -- ** PipeSourceParameters
    PipeSourceParameters (PipeSourceParameters'),
    newPipeSourceParameters,

    -- ** PipeSourceRabbitMQBrokerParameters
    PipeSourceRabbitMQBrokerParameters (PipeSourceRabbitMQBrokerParameters'),
    newPipeSourceRabbitMQBrokerParameters,

    -- ** PipeSourceSelfManagedKafkaParameters
    PipeSourceSelfManagedKafkaParameters (PipeSourceSelfManagedKafkaParameters'),
    newPipeSourceSelfManagedKafkaParameters,

    -- ** PipeSourceSqsQueueParameters
    PipeSourceSqsQueueParameters (PipeSourceSqsQueueParameters'),
    newPipeSourceSqsQueueParameters,

    -- ** PipeTargetBatchJobParameters
    PipeTargetBatchJobParameters (PipeTargetBatchJobParameters'),
    newPipeTargetBatchJobParameters,

    -- ** PipeTargetCloudWatchLogsParameters
    PipeTargetCloudWatchLogsParameters (PipeTargetCloudWatchLogsParameters'),
    newPipeTargetCloudWatchLogsParameters,

    -- ** PipeTargetEcsTaskParameters
    PipeTargetEcsTaskParameters (PipeTargetEcsTaskParameters'),
    newPipeTargetEcsTaskParameters,

    -- ** PipeTargetEventBridgeEventBusParameters
    PipeTargetEventBridgeEventBusParameters (PipeTargetEventBridgeEventBusParameters'),
    newPipeTargetEventBridgeEventBusParameters,

    -- ** PipeTargetHttpParameters
    PipeTargetHttpParameters (PipeTargetHttpParameters'),
    newPipeTargetHttpParameters,

    -- ** PipeTargetKinesisStreamParameters
    PipeTargetKinesisStreamParameters (PipeTargetKinesisStreamParameters'),
    newPipeTargetKinesisStreamParameters,

    -- ** PipeTargetLambdaFunctionParameters
    PipeTargetLambdaFunctionParameters (PipeTargetLambdaFunctionParameters'),
    newPipeTargetLambdaFunctionParameters,

    -- ** PipeTargetParameters
    PipeTargetParameters (PipeTargetParameters'),
    newPipeTargetParameters,

    -- ** PipeTargetRedshiftDataParameters
    PipeTargetRedshiftDataParameters (PipeTargetRedshiftDataParameters'),
    newPipeTargetRedshiftDataParameters,

    -- ** PipeTargetSageMakerPipelineParameters
    PipeTargetSageMakerPipelineParameters (PipeTargetSageMakerPipelineParameters'),
    newPipeTargetSageMakerPipelineParameters,

    -- ** PipeTargetSqsQueueParameters
    PipeTargetSqsQueueParameters (PipeTargetSqsQueueParameters'),
    newPipeTargetSqsQueueParameters,

    -- ** PipeTargetStateMachineParameters
    PipeTargetStateMachineParameters (PipeTargetStateMachineParameters'),
    newPipeTargetStateMachineParameters,

    -- ** PlacementConstraint
    PlacementConstraint (PlacementConstraint'),
    newPlacementConstraint,

    -- ** PlacementStrategy
    PlacementStrategy (PlacementStrategy'),
    newPlacementStrategy,

    -- ** SageMakerPipelineParameter
    SageMakerPipelineParameter (SageMakerPipelineParameter'),
    newSageMakerPipelineParameter,

    -- ** SelfManagedKafkaAccessConfigurationCredentials
    SelfManagedKafkaAccessConfigurationCredentials (SelfManagedKafkaAccessConfigurationCredentials'),
    newSelfManagedKafkaAccessConfigurationCredentials,

    -- ** SelfManagedKafkaAccessConfigurationVpc
    SelfManagedKafkaAccessConfigurationVpc (SelfManagedKafkaAccessConfigurationVpc'),
    newSelfManagedKafkaAccessConfigurationVpc,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UpdatePipeSourceActiveMQBrokerParameters
    UpdatePipeSourceActiveMQBrokerParameters (UpdatePipeSourceActiveMQBrokerParameters'),
    newUpdatePipeSourceActiveMQBrokerParameters,

    -- ** UpdatePipeSourceDynamoDBStreamParameters
    UpdatePipeSourceDynamoDBStreamParameters (UpdatePipeSourceDynamoDBStreamParameters'),
    newUpdatePipeSourceDynamoDBStreamParameters,

    -- ** UpdatePipeSourceKinesisStreamParameters
    UpdatePipeSourceKinesisStreamParameters (UpdatePipeSourceKinesisStreamParameters'),
    newUpdatePipeSourceKinesisStreamParameters,

    -- ** UpdatePipeSourceManagedStreamingKafkaParameters
    UpdatePipeSourceManagedStreamingKafkaParameters (UpdatePipeSourceManagedStreamingKafkaParameters'),
    newUpdatePipeSourceManagedStreamingKafkaParameters,

    -- ** UpdatePipeSourceParameters
    UpdatePipeSourceParameters (UpdatePipeSourceParameters'),
    newUpdatePipeSourceParameters,

    -- ** UpdatePipeSourceRabbitMQBrokerParameters
    UpdatePipeSourceRabbitMQBrokerParameters (UpdatePipeSourceRabbitMQBrokerParameters'),
    newUpdatePipeSourceRabbitMQBrokerParameters,

    -- ** UpdatePipeSourceSelfManagedKafkaParameters
    UpdatePipeSourceSelfManagedKafkaParameters (UpdatePipeSourceSelfManagedKafkaParameters'),
    newUpdatePipeSourceSelfManagedKafkaParameters,

    -- ** UpdatePipeSourceSqsQueueParameters
    UpdatePipeSourceSqsQueueParameters (UpdatePipeSourceSqsQueueParameters'),
    newUpdatePipeSourceSqsQueueParameters,
  )
where

import Amazonka.Pipes.CreatePipe
import Amazonka.Pipes.DeletePipe
import Amazonka.Pipes.DescribePipe
import Amazonka.Pipes.Lens
import Amazonka.Pipes.ListPipes
import Amazonka.Pipes.ListTagsForResource
import Amazonka.Pipes.StartPipe
import Amazonka.Pipes.StopPipe
import Amazonka.Pipes.TagResource
import Amazonka.Pipes.Types
import Amazonka.Pipes.UntagResource
import Amazonka.Pipes.UpdatePipe
import Amazonka.Pipes.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Pipes'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
