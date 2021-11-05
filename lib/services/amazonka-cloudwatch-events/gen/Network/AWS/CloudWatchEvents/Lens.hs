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

    -- ** DeauthorizeConnection
    deauthorizeConnection_name,
    deauthorizeConnectionResponse_creationTime,
    deauthorizeConnectionResponse_lastModifiedTime,
    deauthorizeConnectionResponse_lastAuthorizedTime,
    deauthorizeConnectionResponse_connectionArn,
    deauthorizeConnectionResponse_connectionState,
    deauthorizeConnectionResponse_httpStatus,

    -- ** RemoveTargets
    removeTargets_force,
    removeTargets_eventBusName,
    removeTargets_rule,
    removeTargets_ids,
    removeTargetsResponse_failedEntryCount,
    removeTargetsResponse_failedEntries,
    removeTargetsResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_force,
    deleteRule_eventBusName,
    deleteRule_name,

    -- ** ListPartnerEventSourceAccounts
    listPartnerEventSourceAccounts_nextToken,
    listPartnerEventSourceAccounts_limit,
    listPartnerEventSourceAccounts_eventSourceName,
    listPartnerEventSourceAccountsResponse_partnerEventSourceAccounts,
    listPartnerEventSourceAccountsResponse_nextToken,
    listPartnerEventSourceAccountsResponse_httpStatus,

    -- ** ListConnections
    listConnections_nextToken,
    listConnections_namePrefix,
    listConnections_limit,
    listConnections_connectionState,
    listConnectionsResponse_connections,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_name,
    deleteConnectionResponse_creationTime,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_authParameters,
    updateConnection_authorizationType,
    updateConnection_description,
    updateConnection_name,
    updateConnectionResponse_creationTime,
    updateConnectionResponse_lastModifiedTime,
    updateConnectionResponse_lastAuthorizedTime,
    updateConnectionResponse_connectionArn,
    updateConnectionResponse_connectionState,
    updateConnectionResponse_httpStatus,

    -- ** ListRules
    listRules_nextToken,
    listRules_eventBusName,
    listRules_namePrefix,
    listRules_limit,
    listRulesResponse_rules,
    listRulesResponse_nextToken,
    listRulesResponse_httpStatus,

    -- ** PutRule
    putRule_eventPattern,
    putRule_state,
    putRule_eventBusName,
    putRule_scheduleExpression,
    putRule_description,
    putRule_tags,
    putRule_roleArn,
    putRule_name,
    putRuleResponse_ruleArn,
    putRuleResponse_httpStatus,

    -- ** DisableRule
    disableRule_eventBusName,
    disableRule_name,

    -- ** PutPermission
    putPermission_action,
    putPermission_eventBusName,
    putPermission_principal,
    putPermission_policy,
    putPermission_statementId,
    putPermission_condition,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListReplays
    listReplays_eventSourceArn,
    listReplays_state,
    listReplays_nextToken,
    listReplays_namePrefix,
    listReplays_limit,
    listReplaysResponse_replays,
    listReplaysResponse_nextToken,
    listReplaysResponse_httpStatus,

    -- ** CreateConnection
    createConnection_description,
    createConnection_name,
    createConnection_authorizationType,
    createConnection_authParameters,
    createConnectionResponse_creationTime,
    createConnectionResponse_lastModifiedTime,
    createConnectionResponse_connectionArn,
    createConnectionResponse_connectionState,
    createConnectionResponse_httpStatus,

    -- ** CancelReplay
    cancelReplay_replayName,
    cancelReplayResponse_state,
    cancelReplayResponse_replayArn,
    cancelReplayResponse_stateReason,
    cancelReplayResponse_httpStatus,

    -- ** ListTargetsByRule
    listTargetsByRule_nextToken,
    listTargetsByRule_eventBusName,
    listTargetsByRule_limit,
    listTargetsByRule_rule,
    listTargetsByRuleResponse_nextToken,
    listTargetsByRuleResponse_targets,
    listTargetsByRuleResponse_httpStatus,

    -- ** RemovePermission
    removePermission_eventBusName,
    removePermission_removeAllPermissions,
    removePermission_statementId,

    -- ** ListApiDestinations
    listApiDestinations_nextToken,
    listApiDestinations_namePrefix,
    listApiDestinations_limit,
    listApiDestinations_connectionArn,
    listApiDestinationsResponse_apiDestinations,
    listApiDestinationsResponse_nextToken,
    listApiDestinationsResponse_httpStatus,

    -- ** UpdateApiDestination
    updateApiDestination_httpMethod,
    updateApiDestination_invocationEndpoint,
    updateApiDestination_invocationRateLimitPerSecond,
    updateApiDestination_connectionArn,
    updateApiDestination_description,
    updateApiDestination_name,
    updateApiDestinationResponse_creationTime,
    updateApiDestinationResponse_lastModifiedTime,
    updateApiDestinationResponse_apiDestinationState,
    updateApiDestinationResponse_apiDestinationArn,
    updateApiDestinationResponse_httpStatus,

    -- ** DeleteApiDestination
    deleteApiDestination_name,
    deleteApiDestinationResponse_httpStatus,

    -- ** ActivateEventSource
    activateEventSource_name,

    -- ** CreateApiDestination
    createApiDestination_invocationRateLimitPerSecond,
    createApiDestination_description,
    createApiDestination_name,
    createApiDestination_connectionArn,
    createApiDestination_invocationEndpoint,
    createApiDestination_httpMethod,
    createApiDestinationResponse_creationTime,
    createApiDestinationResponse_lastModifiedTime,
    createApiDestinationResponse_apiDestinationState,
    createApiDestinationResponse_apiDestinationArn,
    createApiDestinationResponse_httpStatus,

    -- ** PutPartnerEvents
    putPartnerEvents_entries,
    putPartnerEventsResponse_failedEntryCount,
    putPartnerEventsResponse_entries,
    putPartnerEventsResponse_httpStatus,

    -- ** DescribeConnection
    describeConnection_name,
    describeConnectionResponse_creationTime,
    describeConnectionResponse_lastModifiedTime,
    describeConnectionResponse_name,
    describeConnectionResponse_authParameters,
    describeConnectionResponse_lastAuthorizedTime,
    describeConnectionResponse_authorizationType,
    describeConnectionResponse_connectionArn,
    describeConnectionResponse_stateReason,
    describeConnectionResponse_secretArn,
    describeConnectionResponse_description,
    describeConnectionResponse_connectionState,
    describeConnectionResponse_httpStatus,

    -- ** DescribeRule
    describeRule_eventBusName,
    describeRule_name,
    describeRuleResponse_eventPattern,
    describeRuleResponse_state,
    describeRuleResponse_arn,
    describeRuleResponse_createdBy,
    describeRuleResponse_eventBusName,
    describeRuleResponse_scheduleExpression,
    describeRuleResponse_name,
    describeRuleResponse_description,
    describeRuleResponse_managedBy,
    describeRuleResponse_roleArn,
    describeRuleResponse_httpStatus,

    -- ** ListArchives
    listArchives_eventSourceArn,
    listArchives_state,
    listArchives_nextToken,
    listArchives_namePrefix,
    listArchives_limit,
    listArchivesResponse_archives,
    listArchivesResponse_nextToken,
    listArchivesResponse_httpStatus,

    -- ** StartReplay
    startReplay_description,
    startReplay_replayName,
    startReplay_eventSourceArn,
    startReplay_eventStartTime,
    startReplay_eventEndTime,
    startReplay_destination,
    startReplayResponse_state,
    startReplayResponse_replayStartTime,
    startReplayResponse_replayArn,
    startReplayResponse_stateReason,
    startReplayResponse_httpStatus,

    -- ** DeletePartnerEventSource
    deletePartnerEventSource_name,
    deletePartnerEventSource_account,

    -- ** DescribeReplay
    describeReplay_replayName,
    describeReplayResponse_eventSourceArn,
    describeReplayResponse_destination,
    describeReplayResponse_state,
    describeReplayResponse_eventEndTime,
    describeReplayResponse_replayStartTime,
    describeReplayResponse_replayArn,
    describeReplayResponse_replayEndTime,
    describeReplayResponse_eventLastReplayedTime,
    describeReplayResponse_eventStartTime,
    describeReplayResponse_replayName,
    describeReplayResponse_stateReason,
    describeReplayResponse_description,
    describeReplayResponse_httpStatus,

    -- ** DescribeApiDestination
    describeApiDestination_name,
    describeApiDestinationResponse_creationTime,
    describeApiDestinationResponse_httpMethod,
    describeApiDestinationResponse_invocationEndpoint,
    describeApiDestinationResponse_lastModifiedTime,
    describeApiDestinationResponse_name,
    describeApiDestinationResponse_invocationRateLimitPerSecond,
    describeApiDestinationResponse_apiDestinationState,
    describeApiDestinationResponse_connectionArn,
    describeApiDestinationResponse_description,
    describeApiDestinationResponse_apiDestinationArn,
    describeApiDestinationResponse_httpStatus,

    -- ** ListEventBuses
    listEventBuses_nextToken,
    listEventBuses_namePrefix,
    listEventBuses_limit,
    listEventBusesResponse_eventBuses,
    listEventBusesResponse_nextToken,
    listEventBusesResponse_httpStatus,

    -- ** CreateEventBus
    createEventBus_eventSourceName,
    createEventBus_tags,
    createEventBus_name,
    createEventBusResponse_eventBusArn,
    createEventBusResponse_httpStatus,

    -- ** DescribeEventSource
    describeEventSource_name,
    describeEventSourceResponse_creationTime,
    describeEventSourceResponse_state,
    describeEventSourceResponse_arn,
    describeEventSourceResponse_createdBy,
    describeEventSourceResponse_name,
    describeEventSourceResponse_expirationTime,
    describeEventSourceResponse_httpStatus,

    -- ** DescribeArchive
    describeArchive_archiveName,
    describeArchiveResponse_creationTime,
    describeArchiveResponse_sizeBytes,
    describeArchiveResponse_eventSourceArn,
    describeArchiveResponse_eventPattern,
    describeArchiveResponse_state,
    describeArchiveResponse_eventCount,
    describeArchiveResponse_archiveName,
    describeArchiveResponse_retentionDays,
    describeArchiveResponse_archiveArn,
    describeArchiveResponse_stateReason,
    describeArchiveResponse_description,
    describeArchiveResponse_httpStatus,

    -- ** EnableRule
    enableRule_eventBusName,
    enableRule_name,

    -- ** ListRuleNamesByTarget
    listRuleNamesByTarget_nextToken,
    listRuleNamesByTarget_eventBusName,
    listRuleNamesByTarget_limit,
    listRuleNamesByTarget_targetArn,
    listRuleNamesByTargetResponse_ruleNames,
    listRuleNamesByTargetResponse_nextToken,
    listRuleNamesByTargetResponse_httpStatus,

    -- ** TestEventPattern
    testEventPattern_eventPattern,
    testEventPattern_event,
    testEventPatternResponse_result,
    testEventPatternResponse_httpStatus,

    -- ** DescribePartnerEventSource
    describePartnerEventSource_name,
    describePartnerEventSourceResponse_arn,
    describePartnerEventSourceResponse_name,
    describePartnerEventSourceResponse_httpStatus,

    -- ** DescribeEventBus
    describeEventBus_name,
    describeEventBusResponse_arn,
    describeEventBusResponse_name,
    describeEventBusResponse_policy,
    describeEventBusResponse_httpStatus,

    -- ** ListEventSources
    listEventSources_nextToken,
    listEventSources_namePrefix,
    listEventSources_limit,
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreatePartnerEventSource
    createPartnerEventSource_name,
    createPartnerEventSource_account,
    createPartnerEventSourceResponse_eventSourceArn,
    createPartnerEventSourceResponse_httpStatus,

    -- ** PutTargets
    putTargets_eventBusName,
    putTargets_rule,
    putTargets_targets,
    putTargetsResponse_failedEntryCount,
    putTargetsResponse_failedEntries,
    putTargetsResponse_httpStatus,

    -- ** UpdateArchive
    updateArchive_eventPattern,
    updateArchive_retentionDays,
    updateArchive_description,
    updateArchive_archiveName,
    updateArchiveResponse_creationTime,
    updateArchiveResponse_state,
    updateArchiveResponse_archiveArn,
    updateArchiveResponse_stateReason,
    updateArchiveResponse_httpStatus,

    -- ** DeleteArchive
    deleteArchive_archiveName,
    deleteArchiveResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** PutEvents
    putEvents_entries,
    putEventsResponse_failedEntryCount,
    putEventsResponse_entries,
    putEventsResponse_httpStatus,

    -- ** ListPartnerEventSources
    listPartnerEventSources_nextToken,
    listPartnerEventSources_limit,
    listPartnerEventSources_namePrefix,
    listPartnerEventSourcesResponse_partnerEventSources,
    listPartnerEventSourcesResponse_nextToken,
    listPartnerEventSourcesResponse_httpStatus,

    -- ** CreateArchive
    createArchive_eventPattern,
    createArchive_retentionDays,
    createArchive_description,
    createArchive_archiveName,
    createArchive_eventSourceArn,
    createArchiveResponse_creationTime,
    createArchiveResponse_state,
    createArchiveResponse_archiveArn,
    createArchiveResponse_stateReason,
    createArchiveResponse_httpStatus,

    -- ** DeactivateEventSource
    deactivateEventSource_name,

    -- ** DeleteEventBus
    deleteEventBus_name,

    -- * Types

    -- ** ApiDestination
    apiDestination_creationTime,
    apiDestination_httpMethod,
    apiDestination_invocationEndpoint,
    apiDestination_lastModifiedTime,
    apiDestination_name,
    apiDestination_invocationRateLimitPerSecond,
    apiDestination_apiDestinationState,
    apiDestination_connectionArn,
    apiDestination_apiDestinationArn,

    -- ** Archive
    archive_creationTime,
    archive_sizeBytes,
    archive_eventSourceArn,
    archive_state,
    archive_eventCount,
    archive_archiveName,
    archive_retentionDays,
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
    connection_creationTime,
    connection_lastModifiedTime,
    connection_name,
    connection_lastAuthorizedTime,
    connection_authorizationType,
    connection_connectionArn,
    connection_stateReason,
    connection_connectionState,

    -- ** ConnectionApiKeyAuthResponseParameters
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- ** ConnectionAuthResponseParameters
    connectionAuthResponseParameters_oAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,
    connectionAuthResponseParameters_apiKeyAuthParameters,
    connectionAuthResponseParameters_basicAuthParameters,

    -- ** ConnectionBasicAuthResponseParameters
    connectionBasicAuthResponseParameters_username,

    -- ** ConnectionBodyParameter
    connectionBodyParameter_isValueSecret,
    connectionBodyParameter_value,
    connectionBodyParameter_key,

    -- ** ConnectionHeaderParameter
    connectionHeaderParameter_isValueSecret,
    connectionHeaderParameter_value,
    connectionHeaderParameter_key,

    -- ** ConnectionHttpParameters
    connectionHttpParameters_queryStringParameters,
    connectionHttpParameters_headerParameters,
    connectionHttpParameters_bodyParameters,

    -- ** ConnectionOAuthClientResponseParameters
    connectionOAuthClientResponseParameters_clientID,

    -- ** ConnectionOAuthResponseParameters
    connectionOAuthResponseParameters_httpMethod,
    connectionOAuthResponseParameters_clientParameters,
    connectionOAuthResponseParameters_oAuthHttpParameters,
    connectionOAuthResponseParameters_authorizationEndpoint,

    -- ** ConnectionQueryStringParameter
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_value,
    connectionQueryStringParameter_key,

    -- ** CreateConnectionApiKeyAuthRequestParameters
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- ** CreateConnectionAuthRequestParameters
    createConnectionAuthRequestParameters_oAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,
    createConnectionAuthRequestParameters_apiKeyAuthParameters,
    createConnectionAuthRequestParameters_basicAuthParameters,

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
    ecsParameters_group,
    ecsParameters_propagateTags,
    ecsParameters_platformVersion,
    ecsParameters_enableECSManagedTags,
    ecsParameters_referenceId,
    ecsParameters_placementConstraints,
    ecsParameters_placementStrategy,
    ecsParameters_launchType,
    ecsParameters_capacityProviderStrategy,
    ecsParameters_taskCount,
    ecsParameters_networkConfiguration,
    ecsParameters_tags,
    ecsParameters_enableExecuteCommand,
    ecsParameters_taskDefinitionArn,

    -- ** EventBus
    eventBus_arn,
    eventBus_name,
    eventBus_policy,

    -- ** EventSource
    eventSource_creationTime,
    eventSource_state,
    eventSource_arn,
    eventSource_createdBy,
    eventSource_name,
    eventSource_expirationTime,

    -- ** HttpParameters
    httpParameters_pathParameterValues,
    httpParameters_queryStringParameters,
    httpParameters_headerParameters,

    -- ** InputTransformer
    inputTransformer_inputPathsMap,
    inputTransformer_inputTemplate,

    -- ** KinesisParameters
    kinesisParameters_partitionKeyPath,

    -- ** NetworkConfiguration
    networkConfiguration_awsvpcConfiguration,

    -- ** PartnerEventSource
    partnerEventSource_arn,
    partnerEventSource_name,

    -- ** PartnerEventSourceAccount
    partnerEventSourceAccount_creationTime,
    partnerEventSourceAccount_state,
    partnerEventSourceAccount_account,
    partnerEventSourceAccount_expirationTime,

    -- ** PlacementConstraint
    placementConstraint_expression,
    placementConstraint_type,

    -- ** PlacementStrategy
    placementStrategy_field,
    placementStrategy_type,

    -- ** PutEventsRequestEntry
    putEventsRequestEntry_time,
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_source,
    putEventsRequestEntry_traceHeader,
    putEventsRequestEntry_detail,

    -- ** PutEventsResultEntry
    putEventsResultEntry_errorCode,
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_eventId,

    -- ** PutPartnerEventsRequestEntry
    putPartnerEventsRequestEntry_time,
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_detail,

    -- ** PutPartnerEventsResultEntry
    putPartnerEventsResultEntry_errorCode,
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_eventId,

    -- ** PutTargetsResultEntry
    putTargetsResultEntry_targetId,
    putTargetsResultEntry_errorCode,
    putTargetsResultEntry_errorMessage,

    -- ** RedshiftDataParameters
    redshiftDataParameters_dbUser,
    redshiftDataParameters_secretManagerArn,
    redshiftDataParameters_statementName,
    redshiftDataParameters_withEvent,
    redshiftDataParameters_database,
    redshiftDataParameters_sql,

    -- ** RemoveTargetsResultEntry
    removeTargetsResultEntry_targetId,
    removeTargetsResultEntry_errorCode,
    removeTargetsResultEntry_errorMessage,

    -- ** Replay
    replay_eventSourceArn,
    replay_state,
    replay_eventEndTime,
    replay_replayStartTime,
    replay_replayEndTime,
    replay_eventLastReplayedTime,
    replay_eventStartTime,
    replay_replayName,
    replay_stateReason,

    -- ** ReplayDestination
    replayDestination_filterArns,
    replayDestination_arn,

    -- ** RetryPolicy
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- ** Rule
    rule_eventPattern,
    rule_state,
    rule_arn,
    rule_eventBusName,
    rule_scheduleExpression,
    rule_name,
    rule_description,
    rule_managedBy,
    rule_roleArn,

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
    target_runCommandParameters,
    target_httpParameters,
    target_kinesisParameters,
    target_inputTransformer,
    target_deadLetterConfig,
    target_sageMakerPipelineParameters,
    target_sqsParameters,
    target_input,
    target_batchParameters,
    target_redshiftDataParameters,
    target_ecsParameters,
    target_retryPolicy,
    target_inputPath,
    target_roleArn,
    target_id,
    target_arn,

    -- ** UpdateConnectionApiKeyAuthRequestParameters
    updateConnectionApiKeyAuthRequestParameters_apiKeyValue,
    updateConnectionApiKeyAuthRequestParameters_apiKeyName,

    -- ** UpdateConnectionAuthRequestParameters
    updateConnectionAuthRequestParameters_oAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,
    updateConnectionAuthRequestParameters_basicAuthParameters,

    -- ** UpdateConnectionBasicAuthRequestParameters
    updateConnectionBasicAuthRequestParameters_username,
    updateConnectionBasicAuthRequestParameters_password,

    -- ** UpdateConnectionOAuthClientRequestParameters
    updateConnectionOAuthClientRequestParameters_clientID,
    updateConnectionOAuthClientRequestParameters_clientSecret,

    -- ** UpdateConnectionOAuthRequestParameters
    updateConnectionOAuthRequestParameters_httpMethod,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
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
