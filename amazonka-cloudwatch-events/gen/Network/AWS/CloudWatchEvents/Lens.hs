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

    -- ** ListPartnerEventSourceAccounts
    listPartnerEventSourceAccounts_nextToken,
    listPartnerEventSourceAccounts_limit,
    listPartnerEventSourceAccounts_eventSourceName,
    listPartnerEventSourceAccountsResponse_nextToken,
    listPartnerEventSourceAccountsResponse_partnerEventSourceAccounts,
    listPartnerEventSourceAccountsResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_name,
    deleteConnectionResponse_creationTime,
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_authorizationType,
    updateConnection_description,
    updateConnection_authParameters,
    updateConnection_name,
    updateConnectionResponse_creationTime,
    updateConnectionResponse_connectionState,
    updateConnectionResponse_connectionArn,
    updateConnectionResponse_lastModifiedTime,
    updateConnectionResponse_lastAuthorizedTime,
    updateConnectionResponse_httpStatus,

    -- ** DeleteRule
    deleteRule_force,
    deleteRule_eventBusName,
    deleteRule_name,

    -- ** DescribeArchive
    describeArchive_archiveName,
    describeArchiveResponse_eventCount,
    describeArchiveResponse_eventPattern,
    describeArchiveResponse_eventSourceArn,
    describeArchiveResponse_creationTime,
    describeArchiveResponse_stateReason,
    describeArchiveResponse_archiveName,
    describeArchiveResponse_archiveArn,
    describeArchiveResponse_state,
    describeArchiveResponse_sizeBytes,
    describeArchiveResponse_description,
    describeArchiveResponse_retentionDays,
    describeArchiveResponse_httpStatus,

    -- ** DescribeEventSource
    describeEventSource_name,
    describeEventSourceResponse_creationTime,
    describeEventSourceResponse_expirationTime,
    describeEventSourceResponse_arn,
    describeEventSourceResponse_state,
    describeEventSourceResponse_name,
    describeEventSourceResponse_createdBy,
    describeEventSourceResponse_httpStatus,

    -- ** DescribeApiDestination
    describeApiDestination_name,
    describeApiDestinationResponse_httpMethod,
    describeApiDestinationResponse_creationTime,
    describeApiDestinationResponse_apiDestinationArn,
    describeApiDestinationResponse_invocationEndpoint,
    describeApiDestinationResponse_apiDestinationState,
    describeApiDestinationResponse_connectionArn,
    describeApiDestinationResponse_name,
    describeApiDestinationResponse_lastModifiedTime,
    describeApiDestinationResponse_description,
    describeApiDestinationResponse_invocationRateLimitPerSecond,
    describeApiDestinationResponse_httpStatus,

    -- ** DeactivateEventSource
    deactivateEventSource_name,

    -- ** UpdateArchive
    updateArchive_eventPattern,
    updateArchive_description,
    updateArchive_retentionDays,
    updateArchive_archiveName,
    updateArchiveResponse_creationTime,
    updateArchiveResponse_stateReason,
    updateArchiveResponse_archiveArn,
    updateArchiveResponse_state,
    updateArchiveResponse_httpStatus,

    -- ** DescribeConnection
    describeConnection_name,
    describeConnectionResponse_creationTime,
    describeConnectionResponse_connectionState,
    describeConnectionResponse_secretArn,
    describeConnectionResponse_stateReason,
    describeConnectionResponse_authorizationType,
    describeConnectionResponse_connectionArn,
    describeConnectionResponse_name,
    describeConnectionResponse_lastModifiedTime,
    describeConnectionResponse_description,
    describeConnectionResponse_lastAuthorizedTime,
    describeConnectionResponse_authParameters,
    describeConnectionResponse_httpStatus,

    -- ** DeleteArchive
    deleteArchive_archiveName,
    deleteArchiveResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeRule
    describeRule_eventBusName,
    describeRule_name,
    describeRuleResponse_eventPattern,
    describeRuleResponse_roleArn,
    describeRuleResponse_arn,
    describeRuleResponse_eventBusName,
    describeRuleResponse_state,
    describeRuleResponse_scheduleExpression,
    describeRuleResponse_name,
    describeRuleResponse_managedBy,
    describeRuleResponse_description,
    describeRuleResponse_createdBy,
    describeRuleResponse_httpStatus,

    -- ** ListArchives
    listArchives_nextToken,
    listArchives_eventSourceArn,
    listArchives_state,
    listArchives_namePrefix,
    listArchives_limit,
    listArchivesResponse_nextToken,
    listArchivesResponse_archives,
    listArchivesResponse_httpStatus,

    -- ** PutPartnerEvents
    putPartnerEvents_entries,
    putPartnerEventsResponse_failedEntryCount,
    putPartnerEventsResponse_entries,
    putPartnerEventsResponse_httpStatus,

    -- ** CreateApiDestination
    createApiDestination_description,
    createApiDestination_invocationRateLimitPerSecond,
    createApiDestination_name,
    createApiDestination_connectionArn,
    createApiDestination_invocationEndpoint,
    createApiDestination_httpMethod,
    createApiDestinationResponse_creationTime,
    createApiDestinationResponse_apiDestinationArn,
    createApiDestinationResponse_apiDestinationState,
    createApiDestinationResponse_lastModifiedTime,
    createApiDestinationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListApiDestinations
    listApiDestinations_nextToken,
    listApiDestinations_connectionArn,
    listApiDestinations_namePrefix,
    listApiDestinations_limit,
    listApiDestinationsResponse_nextToken,
    listApiDestinationsResponse_apiDestinations,
    listApiDestinationsResponse_httpStatus,

    -- ** DescribeEventBus
    describeEventBus_name,
    describeEventBusResponse_arn,
    describeEventBusResponse_name,
    describeEventBusResponse_policy,
    describeEventBusResponse_httpStatus,

    -- ** ListTargetsByRule
    listTargetsByRule_nextToken,
    listTargetsByRule_eventBusName,
    listTargetsByRule_limit,
    listTargetsByRule_rule,
    listTargetsByRuleResponse_nextToken,
    listTargetsByRuleResponse_targets,
    listTargetsByRuleResponse_httpStatus,

    -- ** CreateConnection
    createConnection_description,
    createConnection_name,
    createConnection_authorizationType,
    createConnection_authParameters,
    createConnectionResponse_creationTime,
    createConnectionResponse_connectionState,
    createConnectionResponse_connectionArn,
    createConnectionResponse_lastModifiedTime,
    createConnectionResponse_httpStatus,

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
    listRules_namePrefix,
    listRules_limit,
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** PutRule
    putRule_eventPattern,
    putRule_roleArn,
    putRule_eventBusName,
    putRule_state,
    putRule_scheduleExpression,
    putRule_tags,
    putRule_description,
    putRule_name,
    putRuleResponse_ruleArn,
    putRuleResponse_httpStatus,

    -- ** EnableRule
    enableRule_eventBusName,
    enableRule_name,

    -- ** ListConnections
    listConnections_nextToken,
    listConnections_connectionState,
    listConnections_namePrefix,
    listConnections_limit,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_connections,
    listConnectionsResponse_httpStatus,

    -- ** DeauthorizeConnection
    deauthorizeConnection_name,
    deauthorizeConnectionResponse_creationTime,
    deauthorizeConnectionResponse_connectionState,
    deauthorizeConnectionResponse_connectionArn,
    deauthorizeConnectionResponse_lastModifiedTime,
    deauthorizeConnectionResponse_lastAuthorizedTime,
    deauthorizeConnectionResponse_httpStatus,

    -- ** CreateEventBus
    createEventBus_tags,
    createEventBus_eventSourceName,
    createEventBus_name,
    createEventBusResponse_eventBusArn,
    createEventBusResponse_httpStatus,

    -- ** RemoveTargets
    removeTargets_force,
    removeTargets_eventBusName,
    removeTargets_rule,
    removeTargets_ids,
    removeTargetsResponse_failedEntryCount,
    removeTargetsResponse_failedEntries,
    removeTargetsResponse_httpStatus,

    -- ** ListEventBuses
    listEventBuses_nextToken,
    listEventBuses_namePrefix,
    listEventBuses_limit,
    listEventBusesResponse_nextToken,
    listEventBusesResponse_eventBuses,
    listEventBusesResponse_httpStatus,

    -- ** DeleteEventBus
    deleteEventBus_name,

    -- ** PutEvents
    putEvents_entries,
    putEventsResponse_failedEntryCount,
    putEventsResponse_entries,
    putEventsResponse_httpStatus,

    -- ** CreateArchive
    createArchive_eventPattern,
    createArchive_description,
    createArchive_retentionDays,
    createArchive_archiveName,
    createArchive_eventSourceArn,
    createArchiveResponse_creationTime,
    createArchiveResponse_stateReason,
    createArchiveResponse_archiveArn,
    createArchiveResponse_state,
    createArchiveResponse_httpStatus,

    -- ** ListPartnerEventSources
    listPartnerEventSources_nextToken,
    listPartnerEventSources_limit,
    listPartnerEventSources_namePrefix,
    listPartnerEventSourcesResponse_nextToken,
    listPartnerEventSourcesResponse_partnerEventSources,
    listPartnerEventSourcesResponse_httpStatus,

    -- ** DescribeReplay
    describeReplay_replayName,
    describeReplayResponse_eventSourceArn,
    describeReplayResponse_eventStartTime,
    describeReplayResponse_replayStartTime,
    describeReplayResponse_replayArn,
    describeReplayResponse_stateReason,
    describeReplayResponse_state,
    describeReplayResponse_destination,
    describeReplayResponse_replayName,
    describeReplayResponse_eventLastReplayedTime,
    describeReplayResponse_replayEndTime,
    describeReplayResponse_eventEndTime,
    describeReplayResponse_description,
    describeReplayResponse_httpStatus,

    -- ** DeletePartnerEventSource
    deletePartnerEventSource_name,
    deletePartnerEventSource_account,

    -- ** CreatePartnerEventSource
    createPartnerEventSource_name,
    createPartnerEventSource_account,
    createPartnerEventSourceResponse_eventSourceArn,
    createPartnerEventSourceResponse_httpStatus,

    -- ** StartReplay
    startReplay_description,
    startReplay_replayName,
    startReplay_eventSourceArn,
    startReplay_eventStartTime,
    startReplay_eventEndTime,
    startReplay_destination,
    startReplayResponse_replayStartTime,
    startReplayResponse_replayArn,
    startReplayResponse_stateReason,
    startReplayResponse_state,
    startReplayResponse_httpStatus,

    -- ** PutTargets
    putTargets_eventBusName,
    putTargets_rule,
    putTargets_targets,
    putTargetsResponse_failedEntryCount,
    putTargetsResponse_failedEntries,
    putTargetsResponse_httpStatus,

    -- ** ListEventSources
    listEventSources_nextToken,
    listEventSources_namePrefix,
    listEventSources_limit,
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_httpStatus,

    -- ** ActivateEventSource
    activateEventSource_name,

    -- ** DeleteApiDestination
    deleteApiDestination_name,
    deleteApiDestinationResponse_httpStatus,

    -- ** CancelReplay
    cancelReplay_replayName,
    cancelReplayResponse_replayArn,
    cancelReplayResponse_stateReason,
    cancelReplayResponse_state,
    cancelReplayResponse_httpStatus,

    -- ** UpdateApiDestination
    updateApiDestination_httpMethod,
    updateApiDestination_invocationEndpoint,
    updateApiDestination_connectionArn,
    updateApiDestination_description,
    updateApiDestination_invocationRateLimitPerSecond,
    updateApiDestination_name,
    updateApiDestinationResponse_creationTime,
    updateApiDestinationResponse_apiDestinationArn,
    updateApiDestinationResponse_apiDestinationState,
    updateApiDestinationResponse_lastModifiedTime,
    updateApiDestinationResponse_httpStatus,

    -- ** RemovePermission
    removePermission_statementId,
    removePermission_eventBusName,
    removePermission_removeAllPermissions,

    -- ** TestEventPattern
    testEventPattern_eventPattern,
    testEventPattern_event,
    testEventPatternResponse_result,
    testEventPatternResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DisableRule
    disableRule_eventBusName,
    disableRule_name,

    -- ** ListReplays
    listReplays_nextToken,
    listReplays_eventSourceArn,
    listReplays_state,
    listReplays_namePrefix,
    listReplays_limit,
    listReplaysResponse_nextToken,
    listReplaysResponse_replays,
    listReplaysResponse_httpStatus,

    -- ** DescribePartnerEventSource
    describePartnerEventSource_name,
    describePartnerEventSourceResponse_arn,
    describePartnerEventSourceResponse_name,
    describePartnerEventSourceResponse_httpStatus,

    -- ** PutPermission
    putPermission_condition,
    putPermission_statementId,
    putPermission_principal,
    putPermission_eventBusName,
    putPermission_action,
    putPermission_policy,

    -- * Types

    -- ** ApiDestination
    apiDestination_httpMethod,
    apiDestination_creationTime,
    apiDestination_apiDestinationArn,
    apiDestination_invocationEndpoint,
    apiDestination_apiDestinationState,
    apiDestination_connectionArn,
    apiDestination_name,
    apiDestination_lastModifiedTime,
    apiDestination_invocationRateLimitPerSecond,

    -- ** Archive
    archive_eventCount,
    archive_eventSourceArn,
    archive_creationTime,
    archive_stateReason,
    archive_archiveName,
    archive_state,
    archive_sizeBytes,
    archive_retentionDays,

    -- ** AwsVpcConfiguration
    awsVpcConfiguration_assignPublicIp,
    awsVpcConfiguration_securityGroups,
    awsVpcConfiguration_subnets,

    -- ** BatchArrayProperties
    batchArrayProperties_size,

    -- ** BatchParameters
    batchParameters_arrayProperties,
    batchParameters_retryStrategy,
    batchParameters_jobDefinition,
    batchParameters_jobName,

    -- ** BatchRetryStrategy
    batchRetryStrategy_attempts,

    -- ** Condition
    condition_type,
    condition_key,
    condition_value,

    -- ** Connection
    connection_creationTime,
    connection_connectionState,
    connection_stateReason,
    connection_authorizationType,
    connection_connectionArn,
    connection_name,
    connection_lastModifiedTime,
    connection_lastAuthorizedTime,

    -- ** ConnectionApiKeyAuthResponseParameters
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- ** ConnectionAuthResponseParameters
    connectionAuthResponseParameters_basicAuthParameters,
    connectionAuthResponseParameters_oAuthParameters,
    connectionAuthResponseParameters_apiKeyAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,

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
    connectionOAuthResponseParameters_httpMethod,
    connectionOAuthResponseParameters_clientParameters,
    connectionOAuthResponseParameters_authorizationEndpoint,
    connectionOAuthResponseParameters_oAuthHttpParameters,

    -- ** ConnectionQueryStringParameter
    connectionQueryStringParameter_key,
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_value,

    -- ** CreateConnectionApiKeyAuthRequestParameters
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- ** CreateConnectionAuthRequestParameters
    createConnectionAuthRequestParameters_basicAuthParameters,
    createConnectionAuthRequestParameters_oAuthParameters,
    createConnectionAuthRequestParameters_apiKeyAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,

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
    ecsParameters_networkConfiguration,
    ecsParameters_platformVersion,
    ecsParameters_launchType,
    ecsParameters_group,
    ecsParameters_taskCount,
    ecsParameters_taskDefinitionArn,

    -- ** EventBus
    eventBus_arn,
    eventBus_name,
    eventBus_policy,

    -- ** EventSource
    eventSource_creationTime,
    eventSource_expirationTime,
    eventSource_arn,
    eventSource_state,
    eventSource_name,
    eventSource_createdBy,

    -- ** HttpParameters
    httpParameters_queryStringParameters,
    httpParameters_pathParameterValues,
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
    partnerEventSourceAccount_expirationTime,
    partnerEventSourceAccount_state,
    partnerEventSourceAccount_account,

    -- ** PutEventsRequestEntry
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_source,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_detail,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_traceHeader,
    putEventsRequestEntry_time,

    -- ** PutEventsResultEntry
    putEventsResultEntry_eventId,
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_errorCode,

    -- ** PutPartnerEventsRequestEntry
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_detail,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_time,

    -- ** PutPartnerEventsResultEntry
    putPartnerEventsResultEntry_eventId,
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_errorCode,

    -- ** PutTargetsResultEntry
    putTargetsResultEntry_targetId,
    putTargetsResultEntry_errorMessage,
    putTargetsResultEntry_errorCode,

    -- ** RedshiftDataParameters
    redshiftDataParameters_dbUser,
    redshiftDataParameters_secretManagerArn,
    redshiftDataParameters_statementName,
    redshiftDataParameters_withEvent,
    redshiftDataParameters_database,
    redshiftDataParameters_sql,

    -- ** RemoveTargetsResultEntry
    removeTargetsResultEntry_targetId,
    removeTargetsResultEntry_errorMessage,
    removeTargetsResultEntry_errorCode,

    -- ** Replay
    replay_eventSourceArn,
    replay_eventStartTime,
    replay_replayStartTime,
    replay_stateReason,
    replay_state,
    replay_replayName,
    replay_eventLastReplayedTime,
    replay_replayEndTime,
    replay_eventEndTime,

    -- ** ReplayDestination
    replayDestination_filterArns,
    replayDestination_arn,

    -- ** RetryPolicy
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- ** Rule
    rule_eventPattern,
    rule_roleArn,
    rule_arn,
    rule_eventBusName,
    rule_state,
    rule_scheduleExpression,
    rule_name,
    rule_managedBy,
    rule_description,

    -- ** RunCommandParameters
    runCommandParameters_runCommandTargets,

    -- ** RunCommandTarget
    runCommandTarget_key,
    runCommandTarget_values,

    -- ** SqsParameters
    sqsParameters_messageGroupId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_ecsParameters,
    target_runCommandParameters,
    target_roleArn,
    target_redshiftDataParameters,
    target_batchParameters,
    target_input,
    target_inputPath,
    target_deadLetterConfig,
    target_retryPolicy,
    target_httpParameters,
    target_sqsParameters,
    target_inputTransformer,
    target_kinesisParameters,
    target_id,
    target_arn,

    -- ** UpdateConnectionApiKeyAuthRequestParameters
    updateConnectionApiKeyAuthRequestParameters_apiKeyValue,
    updateConnectionApiKeyAuthRequestParameters_apiKeyName,

    -- ** UpdateConnectionAuthRequestParameters
    updateConnectionAuthRequestParameters_basicAuthParameters,
    updateConnectionAuthRequestParameters_oAuthParameters,
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,

    -- ** UpdateConnectionBasicAuthRequestParameters
    updateConnectionBasicAuthRequestParameters_password,
    updateConnectionBasicAuthRequestParameters_username,

    -- ** UpdateConnectionOAuthClientRequestParameters
    updateConnectionOAuthClientRequestParameters_clientSecret,
    updateConnectionOAuthClientRequestParameters_clientID,

    -- ** UpdateConnectionOAuthRequestParameters
    updateConnectionOAuthRequestParameters_httpMethod,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
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
