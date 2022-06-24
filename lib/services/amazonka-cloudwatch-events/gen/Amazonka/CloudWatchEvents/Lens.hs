{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Lens
  ( -- * Operations

    -- ** ActivateEventSource
    activateEventSource_name,

    -- ** CancelReplay
    cancelReplay_replayName,
    cancelReplayResponse_replayArn,
    cancelReplayResponse_state,
    cancelReplayResponse_stateReason,
    cancelReplayResponse_httpStatus,

    -- ** CreateApiDestination
    createApiDestination_invocationRateLimitPerSecond,
    createApiDestination_description,
    createApiDestination_name,
    createApiDestination_connectionArn,
    createApiDestination_invocationEndpoint,
    createApiDestination_httpMethod,
    createApiDestinationResponse_lastModifiedTime,
    createApiDestinationResponse_apiDestinationState,
    createApiDestinationResponse_creationTime,
    createApiDestinationResponse_apiDestinationArn,
    createApiDestinationResponse_httpStatus,

    -- ** CreateArchive
    createArchive_eventPattern,
    createArchive_retentionDays,
    createArchive_description,
    createArchive_archiveName,
    createArchive_eventSourceArn,
    createArchiveResponse_archiveArn,
    createArchiveResponse_state,
    createArchiveResponse_creationTime,
    createArchiveResponse_stateReason,
    createArchiveResponse_httpStatus,

    -- ** CreateConnection
    createConnection_description,
    createConnection_name,
    createConnection_authorizationType,
    createConnection_authParameters,
    createConnectionResponse_connectionState,
    createConnectionResponse_connectionArn,
    createConnectionResponse_lastModifiedTime,
    createConnectionResponse_creationTime,
    createConnectionResponse_httpStatus,

    -- ** CreateEventBus
    createEventBus_tags,
    createEventBus_eventSourceName,
    createEventBus_name,
    createEventBusResponse_eventBusArn,
    createEventBusResponse_httpStatus,

    -- ** CreatePartnerEventSource
    createPartnerEventSource_name,
    createPartnerEventSource_account,
    createPartnerEventSourceResponse_eventSourceArn,
    createPartnerEventSourceResponse_httpStatus,

    -- ** DeactivateEventSource
    deactivateEventSource_name,

    -- ** DeauthorizeConnection
    deauthorizeConnection_name,
    deauthorizeConnectionResponse_connectionState,
    deauthorizeConnectionResponse_connectionArn,
    deauthorizeConnectionResponse_lastModifiedTime,
    deauthorizeConnectionResponse_lastAuthorizedTime,
    deauthorizeConnectionResponse_creationTime,
    deauthorizeConnectionResponse_httpStatus,

    -- ** DeleteApiDestination
    deleteApiDestination_name,
    deleteApiDestinationResponse_httpStatus,

    -- ** DeleteArchive
    deleteArchive_archiveName,
    deleteArchiveResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_name,
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_creationTime,
    deleteConnectionResponse_httpStatus,

    -- ** DeleteEventBus
    deleteEventBus_name,

    -- ** DeletePartnerEventSource
    deletePartnerEventSource_name,
    deletePartnerEventSource_account,

    -- ** DeleteRule
    deleteRule_eventBusName,
    deleteRule_force,
    deleteRule_name,

    -- ** DescribeApiDestination
    describeApiDestination_name,
    describeApiDestinationResponse_name,
    describeApiDestinationResponse_invocationRateLimitPerSecond,
    describeApiDestinationResponse_invocationEndpoint,
    describeApiDestinationResponse_description,
    describeApiDestinationResponse_connectionArn,
    describeApiDestinationResponse_httpMethod,
    describeApiDestinationResponse_lastModifiedTime,
    describeApiDestinationResponse_apiDestinationState,
    describeApiDestinationResponse_creationTime,
    describeApiDestinationResponse_apiDestinationArn,
    describeApiDestinationResponse_httpStatus,

    -- ** DescribeArchive
    describeArchive_archiveName,
    describeArchiveResponse_eventPattern,
    describeArchiveResponse_retentionDays,
    describeArchiveResponse_archiveArn,
    describeArchiveResponse_sizeBytes,
    describeArchiveResponse_state,
    describeArchiveResponse_description,
    describeArchiveResponse_eventCount,
    describeArchiveResponse_archiveName,
    describeArchiveResponse_eventSourceArn,
    describeArchiveResponse_creationTime,
    describeArchiveResponse_stateReason,
    describeArchiveResponse_httpStatus,

    -- ** DescribeConnection
    describeConnection_name,
    describeConnectionResponse_name,
    describeConnectionResponse_authParameters,
    describeConnectionResponse_connectionState,
    describeConnectionResponse_description,
    describeConnectionResponse_connectionArn,
    describeConnectionResponse_lastModifiedTime,
    describeConnectionResponse_secretArn,
    describeConnectionResponse_lastAuthorizedTime,
    describeConnectionResponse_creationTime,
    describeConnectionResponse_authorizationType,
    describeConnectionResponse_stateReason,
    describeConnectionResponse_httpStatus,

    -- ** DescribeEventBus
    describeEventBus_name,
    describeEventBusResponse_policy,
    describeEventBusResponse_name,
    describeEventBusResponse_arn,
    describeEventBusResponse_httpStatus,

    -- ** DescribeEventSource
    describeEventSource_name,
    describeEventSourceResponse_name,
    describeEventSourceResponse_expirationTime,
    describeEventSourceResponse_arn,
    describeEventSourceResponse_state,
    describeEventSourceResponse_creationTime,
    describeEventSourceResponse_createdBy,
    describeEventSourceResponse_httpStatus,

    -- ** DescribePartnerEventSource
    describePartnerEventSource_name,
    describePartnerEventSourceResponse_name,
    describePartnerEventSourceResponse_arn,
    describePartnerEventSourceResponse_httpStatus,

    -- ** DescribeReplay
    describeReplay_replayName,
    describeReplayResponse_destination,
    describeReplayResponse_replayArn,
    describeReplayResponse_state,
    describeReplayResponse_eventLastReplayedTime,
    describeReplayResponse_eventEndTime,
    describeReplayResponse_description,
    describeReplayResponse_eventSourceArn,
    describeReplayResponse_replayEndTime,
    describeReplayResponse_replayName,
    describeReplayResponse_replayStartTime,
    describeReplayResponse_stateReason,
    describeReplayResponse_eventStartTime,
    describeReplayResponse_httpStatus,

    -- ** DescribeRule
    describeRule_eventBusName,
    describeRule_name,
    describeRuleResponse_name,
    describeRuleResponse_roleArn,
    describeRuleResponse_eventPattern,
    describeRuleResponse_eventBusName,
    describeRuleResponse_arn,
    describeRuleResponse_state,
    describeRuleResponse_description,
    describeRuleResponse_scheduleExpression,
    describeRuleResponse_createdBy,
    describeRuleResponse_managedBy,
    describeRuleResponse_httpStatus,

    -- ** DisableRule
    disableRule_eventBusName,
    disableRule_name,

    -- ** EnableRule
    enableRule_eventBusName,
    enableRule_name,

    -- ** ListApiDestinations
    listApiDestinations_nextToken,
    listApiDestinations_connectionArn,
    listApiDestinations_limit,
    listApiDestinations_namePrefix,
    listApiDestinationsResponse_nextToken,
    listApiDestinationsResponse_apiDestinations,
    listApiDestinationsResponse_httpStatus,

    -- ** ListArchives
    listArchives_nextToken,
    listArchives_state,
    listArchives_limit,
    listArchives_eventSourceArn,
    listArchives_namePrefix,
    listArchivesResponse_nextToken,
    listArchivesResponse_archives,
    listArchivesResponse_httpStatus,

    -- ** ListConnections
    listConnections_nextToken,
    listConnections_connectionState,
    listConnections_limit,
    listConnections_namePrefix,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_connections,
    listConnectionsResponse_httpStatus,

    -- ** ListEventBuses
    listEventBuses_nextToken,
    listEventBuses_limit,
    listEventBuses_namePrefix,
    listEventBusesResponse_nextToken,
    listEventBusesResponse_eventBuses,
    listEventBusesResponse_httpStatus,

    -- ** ListEventSources
    listEventSources_nextToken,
    listEventSources_limit,
    listEventSources_namePrefix,
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_httpStatus,

    -- ** ListPartnerEventSourceAccounts
    listPartnerEventSourceAccounts_nextToken,
    listPartnerEventSourceAccounts_limit,
    listPartnerEventSourceAccounts_eventSourceName,
    listPartnerEventSourceAccountsResponse_nextToken,
    listPartnerEventSourceAccountsResponse_partnerEventSourceAccounts,
    listPartnerEventSourceAccountsResponse_httpStatus,

    -- ** ListPartnerEventSources
    listPartnerEventSources_nextToken,
    listPartnerEventSources_limit,
    listPartnerEventSources_namePrefix,
    listPartnerEventSourcesResponse_nextToken,
    listPartnerEventSourcesResponse_partnerEventSources,
    listPartnerEventSourcesResponse_httpStatus,

    -- ** ListReplays
    listReplays_nextToken,
    listReplays_state,
    listReplays_limit,
    listReplays_eventSourceArn,
    listReplays_namePrefix,
    listReplaysResponse_nextToken,
    listReplaysResponse_replays,
    listReplaysResponse_httpStatus,

    -- ** ListRuleNamesByTarget
    listRuleNamesByTarget_nextToken,
    listRuleNamesByTarget_eventBusName,
    listRuleNamesByTarget_limit,
    listRuleNamesByTarget_targetArn,
    listRuleNamesByTargetResponse_nextToken,
    listRuleNamesByTargetResponse_ruleNames,
    listRuleNamesByTargetResponse_httpStatus,

    -- ** ListRules
    listRules_nextToken,
    listRules_eventBusName,
    listRules_limit,
    listRules_namePrefix,
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetsByRule
    listTargetsByRule_nextToken,
    listTargetsByRule_eventBusName,
    listTargetsByRule_limit,
    listTargetsByRule_rule,
    listTargetsByRuleResponse_nextToken,
    listTargetsByRuleResponse_targets,
    listTargetsByRuleResponse_httpStatus,

    -- ** PutEvents
    putEvents_entries,
    putEventsResponse_entries,
    putEventsResponse_failedEntryCount,
    putEventsResponse_httpStatus,

    -- ** PutPartnerEvents
    putPartnerEvents_entries,
    putPartnerEventsResponse_entries,
    putPartnerEventsResponse_failedEntryCount,
    putPartnerEventsResponse_httpStatus,

    -- ** PutPermission
    putPermission_principal,
    putPermission_policy,
    putPermission_eventBusName,
    putPermission_statementId,
    putPermission_condition,
    putPermission_action,

    -- ** PutRule
    putRule_tags,
    putRule_roleArn,
    putRule_eventPattern,
    putRule_eventBusName,
    putRule_state,
    putRule_description,
    putRule_scheduleExpression,
    putRule_name,
    putRuleResponse_ruleArn,
    putRuleResponse_httpStatus,

    -- ** PutTargets
    putTargets_eventBusName,
    putTargets_rule,
    putTargets_targets,
    putTargetsResponse_failedEntries,
    putTargetsResponse_failedEntryCount,
    putTargetsResponse_httpStatus,

    -- ** RemovePermission
    removePermission_eventBusName,
    removePermission_statementId,
    removePermission_removeAllPermissions,

    -- ** RemoveTargets
    removeTargets_eventBusName,
    removeTargets_force,
    removeTargets_rule,
    removeTargets_ids,
    removeTargetsResponse_failedEntries,
    removeTargetsResponse_failedEntryCount,
    removeTargetsResponse_httpStatus,

    -- ** StartReplay
    startReplay_description,
    startReplay_replayName,
    startReplay_eventSourceArn,
    startReplay_eventStartTime,
    startReplay_eventEndTime,
    startReplay_destination,
    startReplayResponse_replayArn,
    startReplayResponse_state,
    startReplayResponse_replayStartTime,
    startReplayResponse_stateReason,
    startReplayResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestEventPattern
    testEventPattern_eventPattern,
    testEventPattern_event,
    testEventPatternResponse_result,
    testEventPatternResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApiDestination
    updateApiDestination_invocationRateLimitPerSecond,
    updateApiDestination_invocationEndpoint,
    updateApiDestination_description,
    updateApiDestination_connectionArn,
    updateApiDestination_httpMethod,
    updateApiDestination_name,
    updateApiDestinationResponse_lastModifiedTime,
    updateApiDestinationResponse_apiDestinationState,
    updateApiDestinationResponse_creationTime,
    updateApiDestinationResponse_apiDestinationArn,
    updateApiDestinationResponse_httpStatus,

    -- ** UpdateArchive
    updateArchive_eventPattern,
    updateArchive_retentionDays,
    updateArchive_description,
    updateArchive_archiveName,
    updateArchiveResponse_archiveArn,
    updateArchiveResponse_state,
    updateArchiveResponse_creationTime,
    updateArchiveResponse_stateReason,
    updateArchiveResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_authParameters,
    updateConnection_description,
    updateConnection_authorizationType,
    updateConnection_name,
    updateConnectionResponse_connectionState,
    updateConnectionResponse_connectionArn,
    updateConnectionResponse_lastModifiedTime,
    updateConnectionResponse_lastAuthorizedTime,
    updateConnectionResponse_creationTime,
    updateConnectionResponse_httpStatus,

    -- * Types

    -- ** ApiDestination
    apiDestination_name,
    apiDestination_invocationRateLimitPerSecond,
    apiDestination_invocationEndpoint,
    apiDestination_connectionArn,
    apiDestination_httpMethod,
    apiDestination_lastModifiedTime,
    apiDestination_apiDestinationState,
    apiDestination_creationTime,
    apiDestination_apiDestinationArn,

    -- ** Archive
    archive_retentionDays,
    archive_sizeBytes,
    archive_state,
    archive_eventCount,
    archive_archiveName,
    archive_eventSourceArn,
    archive_creationTime,
    archive_stateReason,

    -- ** AwsVpcConfiguration
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_subnets,

    -- ** BatchArrayProperties
    batchArrayProperties_size,

    -- ** BatchParameters
    batchParameters_retryStrategy,
    batchParameters_arrayProperties,
    batchParameters_jobDefinition,
    batchParameters_jobName,

    -- ** BatchRetryStrategy
    batchRetryStrategy_attempts,

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** Condition
    condition_type,
    condition_key,
    condition_value,

    -- ** Connection
    connection_name,
    connection_connectionState,
    connection_connectionArn,
    connection_lastModifiedTime,
    connection_lastAuthorizedTime,
    connection_creationTime,
    connection_authorizationType,
    connection_stateReason,

    -- ** ConnectionApiKeyAuthResponseParameters
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- ** ConnectionAuthResponseParameters
    connectionAuthResponseParameters_oAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,
    connectionAuthResponseParameters_basicAuthParameters,
    connectionAuthResponseParameters_apiKeyAuthParameters,

    -- ** ConnectionBasicAuthResponseParameters
    connectionBasicAuthResponseParameters_username,

    -- ** ConnectionBodyParameter
    connectionBodyParameter_key,
    connectionBodyParameter_isValueSecret,
    connectionBodyParameter_value,

    -- ** ConnectionHeaderParameter
    connectionHeaderParameter_key,
    connectionHeaderParameter_isValueSecret,
    connectionHeaderParameter_value,

    -- ** ConnectionHttpParameters
    connectionHttpParameters_queryStringParameters,
    connectionHttpParameters_headerParameters,
    connectionHttpParameters_bodyParameters,

    -- ** ConnectionOAuthClientResponseParameters
    connectionOAuthClientResponseParameters_clientID,

    -- ** ConnectionOAuthResponseParameters
    connectionOAuthResponseParameters_authorizationEndpoint,
    connectionOAuthResponseParameters_oAuthHttpParameters,
    connectionOAuthResponseParameters_clientParameters,
    connectionOAuthResponseParameters_httpMethod,

    -- ** ConnectionQueryStringParameter
    connectionQueryStringParameter_key,
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_value,

    -- ** CreateConnectionApiKeyAuthRequestParameters
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- ** CreateConnectionAuthRequestParameters
    createConnectionAuthRequestParameters_oAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,
    createConnectionAuthRequestParameters_basicAuthParameters,
    createConnectionAuthRequestParameters_apiKeyAuthParameters,

    -- ** CreateConnectionBasicAuthRequestParameters
    createConnectionBasicAuthRequestParameters_username,
    createConnectionBasicAuthRequestParameters_password,

    -- ** CreateConnectionOAuthClientRequestParameters
    createConnectionOAuthClientRequestParameters_clientID,
    createConnectionOAuthClientRequestParameters_clientSecret,

    -- ** CreateConnectionOAuthRequestParameters
    createConnectionOAuthRequestParameters_oAuthHttpParameters,
    createConnectionOAuthRequestParameters_clientParameters,
    createConnectionOAuthRequestParameters_authorizationEndpoint,
    createConnectionOAuthRequestParameters_httpMethod,

    -- ** DeadLetterConfig
    deadLetterConfig_arn,

    -- ** EcsParameters
    ecsParameters_tags,
    ecsParameters_placementStrategy,
    ecsParameters_networkConfiguration,
    ecsParameters_enableExecuteCommand,
    ecsParameters_capacityProviderStrategy,
    ecsParameters_placementConstraints,
    ecsParameters_propagateTags,
    ecsParameters_referenceId,
    ecsParameters_launchType,
    ecsParameters_platformVersion,
    ecsParameters_enableECSManagedTags,
    ecsParameters_group,
    ecsParameters_taskCount,
    ecsParameters_taskDefinitionArn,

    -- ** EventBus
    eventBus_policy,
    eventBus_name,
    eventBus_arn,

    -- ** EventSource
    eventSource_name,
    eventSource_expirationTime,
    eventSource_arn,
    eventSource_state,
    eventSource_creationTime,
    eventSource_createdBy,

    -- ** HttpParameters
    httpParameters_queryStringParameters,
    httpParameters_headerParameters,
    httpParameters_pathParameterValues,

    -- ** InputTransformer
    inputTransformer_inputPathsMap,
    inputTransformer_inputTemplate,

    -- ** KinesisParameters
    kinesisParameters_partitionKeyPath,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** PartnerEventSource
    partnerEventSource_name,
    partnerEventSource_arn,

    -- ** PartnerEventSourceAccount
    partnerEventSourceAccount_expirationTime,
    partnerEventSourceAccount_account,
    partnerEventSourceAccount_state,
    partnerEventSourceAccount_creationTime,

    -- ** PlacementConstraint
    placementConstraint_type,
    placementConstraint_expression,

    -- ** PlacementStrategy
    placementStrategy_type,
    placementStrategy_field,

    -- ** PutEventsRequestEntry
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_time,
    putEventsRequestEntry_source,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_traceHeader,
    putEventsRequestEntry_detail,

    -- ** PutEventsResultEntry
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_eventId,
    putEventsResultEntry_errorCode,

    -- ** PutPartnerEventsRequestEntry
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_time,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_detail,

    -- ** PutPartnerEventsResultEntry
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_eventId,
    putPartnerEventsResultEntry_errorCode,

    -- ** PutTargetsResultEntry
    putTargetsResultEntry_targetId,
    putTargetsResultEntry_errorMessage,
    putTargetsResultEntry_errorCode,

    -- ** RedshiftDataParameters
    redshiftDataParameters_secretManagerArn,
    redshiftDataParameters_statementName,
    redshiftDataParameters_withEvent,
    redshiftDataParameters_dbUser,
    redshiftDataParameters_database,
    redshiftDataParameters_sql,

    -- ** RemoveTargetsResultEntry
    removeTargetsResultEntry_targetId,
    removeTargetsResultEntry_errorMessage,
    removeTargetsResultEntry_errorCode,

    -- ** Replay
    replay_state,
    replay_eventLastReplayedTime,
    replay_eventEndTime,
    replay_eventSourceArn,
    replay_replayEndTime,
    replay_replayName,
    replay_replayStartTime,
    replay_stateReason,
    replay_eventStartTime,

    -- ** ReplayDestination
    replayDestination_filterArns,
    replayDestination_arn,

    -- ** RetryPolicy
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- ** Rule
    rule_name,
    rule_roleArn,
    rule_eventPattern,
    rule_eventBusName,
    rule_arn,
    rule_state,
    rule_description,
    rule_scheduleExpression,
    rule_managedBy,

    -- ** RunCommandParameters
    runCommandParameters_runCommandTargets,

    -- ** RunCommandTarget
    runCommandTarget_key,
    runCommandTarget_values,

    -- ** SageMakerPipelineParameter
    sageMakerPipelineParameter_name,
    sageMakerPipelineParameter_value,

    -- ** SageMakerPipelineParameters
    sageMakerPipelineParameters_pipelineParameterList,

    -- ** SqsParameters
    sqsParameters_messageGroupId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_kinesisParameters,
    target_httpParameters,
    target_roleArn,
    target_inputPath,
    target_sageMakerPipelineParameters,
    target_runCommandParameters,
    target_input,
    target_redshiftDataParameters,
    target_sqsParameters,
    target_inputTransformer,
    target_batchParameters,
    target_ecsParameters,
    target_retryPolicy,
    target_deadLetterConfig,
    target_id,
    target_arn,

    -- ** UpdateConnectionApiKeyAuthRequestParameters
    updateConnectionApiKeyAuthRequestParameters_apiKeyValue,
    updateConnectionApiKeyAuthRequestParameters_apiKeyName,

    -- ** UpdateConnectionAuthRequestParameters
    updateConnectionAuthRequestParameters_oAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,
    updateConnectionAuthRequestParameters_basicAuthParameters,
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,

    -- ** UpdateConnectionBasicAuthRequestParameters
    updateConnectionBasicAuthRequestParameters_password,
    updateConnectionBasicAuthRequestParameters_username,

    -- ** UpdateConnectionOAuthClientRequestParameters
    updateConnectionOAuthClientRequestParameters_clientSecret,
    updateConnectionOAuthClientRequestParameters_clientID,

    -- ** UpdateConnectionOAuthRequestParameters
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_httpMethod,
  )
where

import Amazonka.CloudWatchEvents.ActivateEventSource
import Amazonka.CloudWatchEvents.CancelReplay
import Amazonka.CloudWatchEvents.CreateApiDestination
import Amazonka.CloudWatchEvents.CreateArchive
import Amazonka.CloudWatchEvents.CreateConnection
import Amazonka.CloudWatchEvents.CreateEventBus
import Amazonka.CloudWatchEvents.CreatePartnerEventSource
import Amazonka.CloudWatchEvents.DeactivateEventSource
import Amazonka.CloudWatchEvents.DeauthorizeConnection
import Amazonka.CloudWatchEvents.DeleteApiDestination
import Amazonka.CloudWatchEvents.DeleteArchive
import Amazonka.CloudWatchEvents.DeleteConnection
import Amazonka.CloudWatchEvents.DeleteEventBus
import Amazonka.CloudWatchEvents.DeletePartnerEventSource
import Amazonka.CloudWatchEvents.DeleteRule
import Amazonka.CloudWatchEvents.DescribeApiDestination
import Amazonka.CloudWatchEvents.DescribeArchive
import Amazonka.CloudWatchEvents.DescribeConnection
import Amazonka.CloudWatchEvents.DescribeEventBus
import Amazonka.CloudWatchEvents.DescribeEventSource
import Amazonka.CloudWatchEvents.DescribePartnerEventSource
import Amazonka.CloudWatchEvents.DescribeReplay
import Amazonka.CloudWatchEvents.DescribeRule
import Amazonka.CloudWatchEvents.DisableRule
import Amazonka.CloudWatchEvents.EnableRule
import Amazonka.CloudWatchEvents.ListApiDestinations
import Amazonka.CloudWatchEvents.ListArchives
import Amazonka.CloudWatchEvents.ListConnections
import Amazonka.CloudWatchEvents.ListEventBuses
import Amazonka.CloudWatchEvents.ListEventSources
import Amazonka.CloudWatchEvents.ListPartnerEventSourceAccounts
import Amazonka.CloudWatchEvents.ListPartnerEventSources
import Amazonka.CloudWatchEvents.ListReplays
import Amazonka.CloudWatchEvents.ListRuleNamesByTarget
import Amazonka.CloudWatchEvents.ListRules
import Amazonka.CloudWatchEvents.ListTagsForResource
import Amazonka.CloudWatchEvents.ListTargetsByRule
import Amazonka.CloudWatchEvents.PutEvents
import Amazonka.CloudWatchEvents.PutPartnerEvents
import Amazonka.CloudWatchEvents.PutPermission
import Amazonka.CloudWatchEvents.PutRule
import Amazonka.CloudWatchEvents.PutTargets
import Amazonka.CloudWatchEvents.RemovePermission
import Amazonka.CloudWatchEvents.RemoveTargets
import Amazonka.CloudWatchEvents.StartReplay
import Amazonka.CloudWatchEvents.TagResource
import Amazonka.CloudWatchEvents.TestEventPattern
import Amazonka.CloudWatchEvents.Types.ApiDestination
import Amazonka.CloudWatchEvents.Types.Archive
import Amazonka.CloudWatchEvents.Types.AwsVpcConfiguration
import Amazonka.CloudWatchEvents.Types.BatchArrayProperties
import Amazonka.CloudWatchEvents.Types.BatchParameters
import Amazonka.CloudWatchEvents.Types.BatchRetryStrategy
import Amazonka.CloudWatchEvents.Types.CapacityProviderStrategyItem
import Amazonka.CloudWatchEvents.Types.Condition
import Amazonka.CloudWatchEvents.Types.Connection
import Amazonka.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionBodyParameter
import Amazonka.CloudWatchEvents.Types.ConnectionHeaderParameter
import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionQueryStringParameter
import Amazonka.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
import Amazonka.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.DeadLetterConfig
import Amazonka.CloudWatchEvents.Types.EcsParameters
import Amazonka.CloudWatchEvents.Types.EventBus
import Amazonka.CloudWatchEvents.Types.EventSource
import Amazonka.CloudWatchEvents.Types.HttpParameters
import Amazonka.CloudWatchEvents.Types.InputTransformer
import Amazonka.CloudWatchEvents.Types.KinesisParameters
import Amazonka.CloudWatchEvents.Types.NetworkConfiguration
import Amazonka.CloudWatchEvents.Types.PartnerEventSource
import Amazonka.CloudWatchEvents.Types.PartnerEventSourceAccount
import Amazonka.CloudWatchEvents.Types.PlacementConstraint
import Amazonka.CloudWatchEvents.Types.PlacementStrategy
import Amazonka.CloudWatchEvents.Types.PutEventsRequestEntry
import Amazonka.CloudWatchEvents.Types.PutEventsResultEntry
import Amazonka.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Amazonka.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Amazonka.CloudWatchEvents.Types.PutTargetsResultEntry
import Amazonka.CloudWatchEvents.Types.RedshiftDataParameters
import Amazonka.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Amazonka.CloudWatchEvents.Types.Replay
import Amazonka.CloudWatchEvents.Types.ReplayDestination
import Amazonka.CloudWatchEvents.Types.RetryPolicy
import Amazonka.CloudWatchEvents.Types.Rule
import Amazonka.CloudWatchEvents.Types.RunCommandParameters
import Amazonka.CloudWatchEvents.Types.RunCommandTarget
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameter
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters
import Amazonka.CloudWatchEvents.Types.SqsParameters
import Amazonka.CloudWatchEvents.Types.Tag
import Amazonka.CloudWatchEvents.Types.Target
import Amazonka.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import Amazonka.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import Amazonka.CloudWatchEvents.UntagResource
import Amazonka.CloudWatchEvents.UpdateApiDestination
import Amazonka.CloudWatchEvents.UpdateArchive
import Amazonka.CloudWatchEvents.UpdateConnection
