{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Lens
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

import Network.AWS.CloudWatchEvents.ActivateEventSource
import Network.AWS.CloudWatchEvents.CancelReplay
import Network.AWS.CloudWatchEvents.CreateApiDestination
import Network.AWS.CloudWatchEvents.CreateArchive
import Network.AWS.CloudWatchEvents.CreateConnection
import Network.AWS.CloudWatchEvents.CreateEventBus
import Network.AWS.CloudWatchEvents.CreatePartnerEventSource
import Network.AWS.CloudWatchEvents.DeactivateEventSource
import Network.AWS.CloudWatchEvents.DeauthorizeConnection
import Network.AWS.CloudWatchEvents.DeleteApiDestination
import Network.AWS.CloudWatchEvents.DeleteArchive
import Network.AWS.CloudWatchEvents.DeleteConnection
import Network.AWS.CloudWatchEvents.DeleteEventBus
import Network.AWS.CloudWatchEvents.DeletePartnerEventSource
import Network.AWS.CloudWatchEvents.DeleteRule
import Network.AWS.CloudWatchEvents.DescribeApiDestination
import Network.AWS.CloudWatchEvents.DescribeArchive
import Network.AWS.CloudWatchEvents.DescribeConnection
import Network.AWS.CloudWatchEvents.DescribeEventBus
import Network.AWS.CloudWatchEvents.DescribeEventSource
import Network.AWS.CloudWatchEvents.DescribePartnerEventSource
import Network.AWS.CloudWatchEvents.DescribeReplay
import Network.AWS.CloudWatchEvents.DescribeRule
import Network.AWS.CloudWatchEvents.DisableRule
import Network.AWS.CloudWatchEvents.EnableRule
import Network.AWS.CloudWatchEvents.ListApiDestinations
import Network.AWS.CloudWatchEvents.ListArchives
import Network.AWS.CloudWatchEvents.ListConnections
import Network.AWS.CloudWatchEvents.ListEventBuses
import Network.AWS.CloudWatchEvents.ListEventSources
import Network.AWS.CloudWatchEvents.ListPartnerEventSourceAccounts
import Network.AWS.CloudWatchEvents.ListPartnerEventSources
import Network.AWS.CloudWatchEvents.ListReplays
import Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
import Network.AWS.CloudWatchEvents.ListRules
import Network.AWS.CloudWatchEvents.ListTagsForResource
import Network.AWS.CloudWatchEvents.ListTargetsByRule
import Network.AWS.CloudWatchEvents.PutEvents
import Network.AWS.CloudWatchEvents.PutPartnerEvents
import Network.AWS.CloudWatchEvents.PutPermission
import Network.AWS.CloudWatchEvents.PutRule
import Network.AWS.CloudWatchEvents.PutTargets
import Network.AWS.CloudWatchEvents.RemovePermission
import Network.AWS.CloudWatchEvents.RemoveTargets
import Network.AWS.CloudWatchEvents.StartReplay
import Network.AWS.CloudWatchEvents.TagResource
import Network.AWS.CloudWatchEvents.TestEventPattern
import Network.AWS.CloudWatchEvents.Types.ApiDestination
import Network.AWS.CloudWatchEvents.Types.Archive
import Network.AWS.CloudWatchEvents.Types.AwsVpcConfiguration
import Network.AWS.CloudWatchEvents.Types.BatchArrayProperties
import Network.AWS.CloudWatchEvents.Types.BatchParameters
import Network.AWS.CloudWatchEvents.Types.BatchRetryStrategy
import Network.AWS.CloudWatchEvents.Types.CapacityProviderStrategyItem
import Network.AWS.CloudWatchEvents.Types.Condition
import Network.AWS.CloudWatchEvents.Types.Connection
import Network.AWS.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionBodyParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionHeaderParameter
import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthClientResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthResponseParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionQueryStringParameter
import Network.AWS.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthClientRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
import Network.AWS.CloudWatchEvents.Types.EcsParameters
import Network.AWS.CloudWatchEvents.Types.EventBus
import Network.AWS.CloudWatchEvents.Types.EventSource
import Network.AWS.CloudWatchEvents.Types.HttpParameters
import Network.AWS.CloudWatchEvents.Types.InputTransformer
import Network.AWS.CloudWatchEvents.Types.KinesisParameters
import Network.AWS.CloudWatchEvents.Types.NetworkConfiguration
import Network.AWS.CloudWatchEvents.Types.PartnerEventSource
import Network.AWS.CloudWatchEvents.Types.PartnerEventSourceAccount
import Network.AWS.CloudWatchEvents.Types.PlacementConstraint
import Network.AWS.CloudWatchEvents.Types.PlacementStrategy
import Network.AWS.CloudWatchEvents.Types.PutEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.RedshiftDataParameters
import Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Network.AWS.CloudWatchEvents.Types.Replay
import Network.AWS.CloudWatchEvents.Types.ReplayDestination
import Network.AWS.CloudWatchEvents.Types.RetryPolicy
import Network.AWS.CloudWatchEvents.Types.Rule
import Network.AWS.CloudWatchEvents.Types.RunCommandParameters
import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import Network.AWS.CloudWatchEvents.Types.SageMakerPipelineParameter
import Network.AWS.CloudWatchEvents.Types.SageMakerPipelineParameters
import Network.AWS.CloudWatchEvents.Types.SqsParameters
import Network.AWS.CloudWatchEvents.Types.Tag
import Network.AWS.CloudWatchEvents.Types.Target
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import Network.AWS.CloudWatchEvents.UntagResource
import Network.AWS.CloudWatchEvents.UpdateApiDestination
import Network.AWS.CloudWatchEvents.UpdateArchive
import Network.AWS.CloudWatchEvents.UpdateConnection
