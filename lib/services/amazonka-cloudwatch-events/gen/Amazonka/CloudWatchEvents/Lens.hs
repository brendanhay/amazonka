{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createApiDestination_description,
    createApiDestination_invocationRateLimitPerSecond,
    createApiDestination_name,
    createApiDestination_connectionArn,
    createApiDestination_invocationEndpoint,
    createApiDestination_httpMethod,
    createApiDestinationResponse_apiDestinationArn,
    createApiDestinationResponse_apiDestinationState,
    createApiDestinationResponse_creationTime,
    createApiDestinationResponse_lastModifiedTime,
    createApiDestinationResponse_httpStatus,

    -- ** CreateArchive
    createArchive_description,
    createArchive_eventPattern,
    createArchive_retentionDays,
    createArchive_archiveName,
    createArchive_eventSourceArn,
    createArchiveResponse_archiveArn,
    createArchiveResponse_creationTime,
    createArchiveResponse_state,
    createArchiveResponse_stateReason,
    createArchiveResponse_httpStatus,

    -- ** CreateConnection
    createConnection_description,
    createConnection_name,
    createConnection_authorizationType,
    createConnection_authParameters,
    createConnectionResponse_connectionArn,
    createConnectionResponse_connectionState,
    createConnectionResponse_creationTime,
    createConnectionResponse_lastModifiedTime,
    createConnectionResponse_httpStatus,

    -- ** CreateEndpoint
    createEndpoint_description,
    createEndpoint_replicationConfig,
    createEndpoint_roleArn,
    createEndpoint_name,
    createEndpoint_routingConfig,
    createEndpoint_eventBuses,
    createEndpointResponse_arn,
    createEndpointResponse_eventBuses,
    createEndpointResponse_name,
    createEndpointResponse_replicationConfig,
    createEndpointResponse_roleArn,
    createEndpointResponse_routingConfig,
    createEndpointResponse_state,
    createEndpointResponse_httpStatus,

    -- ** CreateEventBus
    createEventBus_eventSourceName,
    createEventBus_tags,
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
    deauthorizeConnectionResponse_connectionArn,
    deauthorizeConnectionResponse_connectionState,
    deauthorizeConnectionResponse_creationTime,
    deauthorizeConnectionResponse_lastAuthorizedTime,
    deauthorizeConnectionResponse_lastModifiedTime,
    deauthorizeConnectionResponse_httpStatus,

    -- ** DeleteApiDestination
    deleteApiDestination_name,
    deleteApiDestinationResponse_httpStatus,

    -- ** DeleteArchive
    deleteArchive_archiveName,
    deleteArchiveResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_name,
    deleteConnectionResponse_connectionArn,
    deleteConnectionResponse_connectionState,
    deleteConnectionResponse_creationTime,
    deleteConnectionResponse_lastAuthorizedTime,
    deleteConnectionResponse_lastModifiedTime,
    deleteConnectionResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_name,
    deleteEndpointResponse_httpStatus,

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
    describeApiDestinationResponse_apiDestinationArn,
    describeApiDestinationResponse_apiDestinationState,
    describeApiDestinationResponse_connectionArn,
    describeApiDestinationResponse_creationTime,
    describeApiDestinationResponse_description,
    describeApiDestinationResponse_httpMethod,
    describeApiDestinationResponse_invocationEndpoint,
    describeApiDestinationResponse_invocationRateLimitPerSecond,
    describeApiDestinationResponse_lastModifiedTime,
    describeApiDestinationResponse_name,
    describeApiDestinationResponse_httpStatus,

    -- ** DescribeArchive
    describeArchive_archiveName,
    describeArchiveResponse_archiveArn,
    describeArchiveResponse_archiveName,
    describeArchiveResponse_creationTime,
    describeArchiveResponse_description,
    describeArchiveResponse_eventCount,
    describeArchiveResponse_eventPattern,
    describeArchiveResponse_eventSourceArn,
    describeArchiveResponse_retentionDays,
    describeArchiveResponse_sizeBytes,
    describeArchiveResponse_state,
    describeArchiveResponse_stateReason,
    describeArchiveResponse_httpStatus,

    -- ** DescribeConnection
    describeConnection_name,
    describeConnectionResponse_authParameters,
    describeConnectionResponse_authorizationType,
    describeConnectionResponse_connectionArn,
    describeConnectionResponse_connectionState,
    describeConnectionResponse_creationTime,
    describeConnectionResponse_description,
    describeConnectionResponse_lastAuthorizedTime,
    describeConnectionResponse_lastModifiedTime,
    describeConnectionResponse_name,
    describeConnectionResponse_secretArn,
    describeConnectionResponse_stateReason,
    describeConnectionResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_homeRegion,
    describeEndpoint_name,
    describeEndpointResponse_arn,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_description,
    describeEndpointResponse_endpointId,
    describeEndpointResponse_endpointUrl,
    describeEndpointResponse_eventBuses,
    describeEndpointResponse_lastModifiedTime,
    describeEndpointResponse_name,
    describeEndpointResponse_replicationConfig,
    describeEndpointResponse_roleArn,
    describeEndpointResponse_routingConfig,
    describeEndpointResponse_state,
    describeEndpointResponse_stateReason,
    describeEndpointResponse_httpStatus,

    -- ** DescribeEventBus
    describeEventBus_name,
    describeEventBusResponse_arn,
    describeEventBusResponse_name,
    describeEventBusResponse_policy,
    describeEventBusResponse_httpStatus,

    -- ** DescribeEventSource
    describeEventSource_name,
    describeEventSourceResponse_arn,
    describeEventSourceResponse_createdBy,
    describeEventSourceResponse_creationTime,
    describeEventSourceResponse_expirationTime,
    describeEventSourceResponse_name,
    describeEventSourceResponse_state,
    describeEventSourceResponse_httpStatus,

    -- ** DescribePartnerEventSource
    describePartnerEventSource_name,
    describePartnerEventSourceResponse_arn,
    describePartnerEventSourceResponse_name,
    describePartnerEventSourceResponse_httpStatus,

    -- ** DescribeReplay
    describeReplay_replayName,
    describeReplayResponse_description,
    describeReplayResponse_destination,
    describeReplayResponse_eventEndTime,
    describeReplayResponse_eventLastReplayedTime,
    describeReplayResponse_eventSourceArn,
    describeReplayResponse_eventStartTime,
    describeReplayResponse_replayArn,
    describeReplayResponse_replayEndTime,
    describeReplayResponse_replayName,
    describeReplayResponse_replayStartTime,
    describeReplayResponse_state,
    describeReplayResponse_stateReason,
    describeReplayResponse_httpStatus,

    -- ** DescribeRule
    describeRule_eventBusName,
    describeRule_name,
    describeRuleResponse_arn,
    describeRuleResponse_createdBy,
    describeRuleResponse_description,
    describeRuleResponse_eventBusName,
    describeRuleResponse_eventPattern,
    describeRuleResponse_managedBy,
    describeRuleResponse_name,
    describeRuleResponse_roleArn,
    describeRuleResponse_scheduleExpression,
    describeRuleResponse_state,
    describeRuleResponse_httpStatus,

    -- ** DisableRule
    disableRule_eventBusName,
    disableRule_name,

    -- ** EnableRule
    enableRule_eventBusName,
    enableRule_name,

    -- ** ListApiDestinations
    listApiDestinations_connectionArn,
    listApiDestinations_limit,
    listApiDestinations_namePrefix,
    listApiDestinations_nextToken,
    listApiDestinationsResponse_apiDestinations,
    listApiDestinationsResponse_nextToken,
    listApiDestinationsResponse_httpStatus,

    -- ** ListArchives
    listArchives_eventSourceArn,
    listArchives_limit,
    listArchives_namePrefix,
    listArchives_nextToken,
    listArchives_state,
    listArchivesResponse_archives,
    listArchivesResponse_nextToken,
    listArchivesResponse_httpStatus,

    -- ** ListConnections
    listConnections_connectionState,
    listConnections_limit,
    listConnections_namePrefix,
    listConnections_nextToken,
    listConnectionsResponse_connections,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_httpStatus,

    -- ** ListEndpoints
    listEndpoints_homeRegion,
    listEndpoints_maxResults,
    listEndpoints_namePrefix,
    listEndpoints_nextToken,
    listEndpointsResponse_endpoints,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,

    -- ** ListEventBuses
    listEventBuses_limit,
    listEventBuses_namePrefix,
    listEventBuses_nextToken,
    listEventBusesResponse_eventBuses,
    listEventBusesResponse_nextToken,
    listEventBusesResponse_httpStatus,

    -- ** ListEventSources
    listEventSources_limit,
    listEventSources_namePrefix,
    listEventSources_nextToken,
    listEventSourcesResponse_eventSources,
    listEventSourcesResponse_nextToken,
    listEventSourcesResponse_httpStatus,

    -- ** ListPartnerEventSourceAccounts
    listPartnerEventSourceAccounts_limit,
    listPartnerEventSourceAccounts_nextToken,
    listPartnerEventSourceAccounts_eventSourceName,
    listPartnerEventSourceAccountsResponse_nextToken,
    listPartnerEventSourceAccountsResponse_partnerEventSourceAccounts,
    listPartnerEventSourceAccountsResponse_httpStatus,

    -- ** ListPartnerEventSources
    listPartnerEventSources_limit,
    listPartnerEventSources_nextToken,
    listPartnerEventSources_namePrefix,
    listPartnerEventSourcesResponse_nextToken,
    listPartnerEventSourcesResponse_partnerEventSources,
    listPartnerEventSourcesResponse_httpStatus,

    -- ** ListReplays
    listReplays_eventSourceArn,
    listReplays_limit,
    listReplays_namePrefix,
    listReplays_nextToken,
    listReplays_state,
    listReplaysResponse_nextToken,
    listReplaysResponse_replays,
    listReplaysResponse_httpStatus,

    -- ** ListRuleNamesByTarget
    listRuleNamesByTarget_eventBusName,
    listRuleNamesByTarget_limit,
    listRuleNamesByTarget_nextToken,
    listRuleNamesByTarget_targetArn,
    listRuleNamesByTargetResponse_nextToken,
    listRuleNamesByTargetResponse_ruleNames,
    listRuleNamesByTargetResponse_httpStatus,

    -- ** ListRules
    listRules_eventBusName,
    listRules_limit,
    listRules_namePrefix,
    listRules_nextToken,
    listRulesResponse_nextToken,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTargetsByRule
    listTargetsByRule_eventBusName,
    listTargetsByRule_limit,
    listTargetsByRule_nextToken,
    listTargetsByRule_rule,
    listTargetsByRuleResponse_nextToken,
    listTargetsByRuleResponse_targets,
    listTargetsByRuleResponse_httpStatus,

    -- ** PutEvents
    putEvents_endpointId,
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
    putPermission_action,
    putPermission_condition,
    putPermission_eventBusName,
    putPermission_policy,
    putPermission_principal,
    putPermission_statementId,

    -- ** PutRule
    putRule_description,
    putRule_eventBusName,
    putRule_eventPattern,
    putRule_roleArn,
    putRule_scheduleExpression,
    putRule_state,
    putRule_tags,
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
    removePermission_removeAllPermissions,
    removePermission_statementId,

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
    startReplayResponse_replayStartTime,
    startReplayResponse_state,
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
    updateApiDestination_connectionArn,
    updateApiDestination_description,
    updateApiDestination_httpMethod,
    updateApiDestination_invocationEndpoint,
    updateApiDestination_invocationRateLimitPerSecond,
    updateApiDestination_name,
    updateApiDestinationResponse_apiDestinationArn,
    updateApiDestinationResponse_apiDestinationState,
    updateApiDestinationResponse_creationTime,
    updateApiDestinationResponse_lastModifiedTime,
    updateApiDestinationResponse_httpStatus,

    -- ** UpdateArchive
    updateArchive_description,
    updateArchive_eventPattern,
    updateArchive_retentionDays,
    updateArchive_archiveName,
    updateArchiveResponse_archiveArn,
    updateArchiveResponse_creationTime,
    updateArchiveResponse_state,
    updateArchiveResponse_stateReason,
    updateArchiveResponse_httpStatus,

    -- ** UpdateConnection
    updateConnection_authParameters,
    updateConnection_authorizationType,
    updateConnection_description,
    updateConnection_name,
    updateConnectionResponse_connectionArn,
    updateConnectionResponse_connectionState,
    updateConnectionResponse_creationTime,
    updateConnectionResponse_lastAuthorizedTime,
    updateConnectionResponse_lastModifiedTime,
    updateConnectionResponse_httpStatus,

    -- ** UpdateEndpoint
    updateEndpoint_description,
    updateEndpoint_eventBuses,
    updateEndpoint_replicationConfig,
    updateEndpoint_roleArn,
    updateEndpoint_routingConfig,
    updateEndpoint_name,
    updateEndpointResponse_arn,
    updateEndpointResponse_endpointId,
    updateEndpointResponse_endpointUrl,
    updateEndpointResponse_eventBuses,
    updateEndpointResponse_name,
    updateEndpointResponse_replicationConfig,
    updateEndpointResponse_roleArn,
    updateEndpointResponse_routingConfig,
    updateEndpointResponse_state,
    updateEndpointResponse_httpStatus,

    -- * Types

    -- ** ApiDestination
    apiDestination_apiDestinationArn,
    apiDestination_apiDestinationState,
    apiDestination_connectionArn,
    apiDestination_creationTime,
    apiDestination_httpMethod,
    apiDestination_invocationEndpoint,
    apiDestination_invocationRateLimitPerSecond,
    apiDestination_lastModifiedTime,
    apiDestination_name,

    -- ** Archive
    archive_archiveName,
    archive_creationTime,
    archive_eventCount,
    archive_eventSourceArn,
    archive_retentionDays,
    archive_sizeBytes,
    archive_state,
    archive_stateReason,

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

    -- ** CapacityProviderStrategyItem
    capacityProviderStrategyItem_base,
    capacityProviderStrategyItem_weight,
    capacityProviderStrategyItem_capacityProvider,

    -- ** Condition
    condition_type,
    condition_key,
    condition_value,

    -- ** Connection
    connection_authorizationType,
    connection_connectionArn,
    connection_connectionState,
    connection_creationTime,
    connection_lastAuthorizedTime,
    connection_lastModifiedTime,
    connection_name,
    connection_stateReason,

    -- ** ConnectionApiKeyAuthResponseParameters
    connectionApiKeyAuthResponseParameters_apiKeyName,

    -- ** ConnectionAuthResponseParameters
    connectionAuthResponseParameters_apiKeyAuthParameters,
    connectionAuthResponseParameters_basicAuthParameters,
    connectionAuthResponseParameters_invocationHttpParameters,
    connectionAuthResponseParameters_oAuthParameters,

    -- ** ConnectionBasicAuthResponseParameters
    connectionBasicAuthResponseParameters_username,

    -- ** ConnectionBodyParameter
    connectionBodyParameter_isValueSecret,
    connectionBodyParameter_key,
    connectionBodyParameter_value,

    -- ** ConnectionHeaderParameter
    connectionHeaderParameter_isValueSecret,
    connectionHeaderParameter_key,
    connectionHeaderParameter_value,

    -- ** ConnectionHttpParameters
    connectionHttpParameters_bodyParameters,
    connectionHttpParameters_headerParameters,
    connectionHttpParameters_queryStringParameters,

    -- ** ConnectionOAuthClientResponseParameters
    connectionOAuthClientResponseParameters_clientID,

    -- ** ConnectionOAuthResponseParameters
    connectionOAuthResponseParameters_authorizationEndpoint,
    connectionOAuthResponseParameters_clientParameters,
    connectionOAuthResponseParameters_httpMethod,
    connectionOAuthResponseParameters_oAuthHttpParameters,

    -- ** ConnectionQueryStringParameter
    connectionQueryStringParameter_isValueSecret,
    connectionQueryStringParameter_key,
    connectionQueryStringParameter_value,

    -- ** CreateConnectionApiKeyAuthRequestParameters
    createConnectionApiKeyAuthRequestParameters_apiKeyName,
    createConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- ** CreateConnectionAuthRequestParameters
    createConnectionAuthRequestParameters_apiKeyAuthParameters,
    createConnectionAuthRequestParameters_basicAuthParameters,
    createConnectionAuthRequestParameters_invocationHttpParameters,
    createConnectionAuthRequestParameters_oAuthParameters,

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
    ecsParameters_capacityProviderStrategy,
    ecsParameters_enableECSManagedTags,
    ecsParameters_enableExecuteCommand,
    ecsParameters_group,
    ecsParameters_launchType,
    ecsParameters_networkConfiguration,
    ecsParameters_placementConstraints,
    ecsParameters_placementStrategy,
    ecsParameters_platformVersion,
    ecsParameters_propagateTags,
    ecsParameters_referenceId,
    ecsParameters_tags,
    ecsParameters_taskCount,
    ecsParameters_taskDefinitionArn,

    -- ** Endpoint
    endpoint_arn,
    endpoint_creationTime,
    endpoint_description,
    endpoint_endpointId,
    endpoint_endpointUrl,
    endpoint_eventBuses,
    endpoint_lastModifiedTime,
    endpoint_name,
    endpoint_replicationConfig,
    endpoint_roleArn,
    endpoint_routingConfig,
    endpoint_state,
    endpoint_stateReason,

    -- ** EndpointEventBus
    endpointEventBus_eventBusArn,

    -- ** EventBus
    eventBus_arn,
    eventBus_name,
    eventBus_policy,

    -- ** EventSource
    eventSource_arn,
    eventSource_createdBy,
    eventSource_creationTime,
    eventSource_expirationTime,
    eventSource_name,
    eventSource_state,

    -- ** FailoverConfig
    failoverConfig_primary,
    failoverConfig_secondary,

    -- ** HttpParameters
    httpParameters_headerParameters,
    httpParameters_pathParameterValues,
    httpParameters_queryStringParameters,

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
    partnerEventSourceAccount_account,
    partnerEventSourceAccount_creationTime,
    partnerEventSourceAccount_expirationTime,
    partnerEventSourceAccount_state,

    -- ** PlacementConstraint
    placementConstraint_expression,
    placementConstraint_type,

    -- ** PlacementStrategy
    placementStrategy_field,
    placementStrategy_type,

    -- ** Primary
    primary_healthCheck,

    -- ** PutEventsRequestEntry
    putEventsRequestEntry_detail,
    putEventsRequestEntry_detailType,
    putEventsRequestEntry_eventBusName,
    putEventsRequestEntry_resources,
    putEventsRequestEntry_source,
    putEventsRequestEntry_time,
    putEventsRequestEntry_traceHeader,

    -- ** PutEventsResultEntry
    putEventsResultEntry_errorCode,
    putEventsResultEntry_errorMessage,
    putEventsResultEntry_eventId,

    -- ** PutPartnerEventsRequestEntry
    putPartnerEventsRequestEntry_detail,
    putPartnerEventsRequestEntry_detailType,
    putPartnerEventsRequestEntry_resources,
    putPartnerEventsRequestEntry_source,
    putPartnerEventsRequestEntry_time,

    -- ** PutPartnerEventsResultEntry
    putPartnerEventsResultEntry_errorCode,
    putPartnerEventsResultEntry_errorMessage,
    putPartnerEventsResultEntry_eventId,

    -- ** PutTargetsResultEntry
    putTargetsResultEntry_errorCode,
    putTargetsResultEntry_errorMessage,
    putTargetsResultEntry_targetId,

    -- ** RedshiftDataParameters
    redshiftDataParameters_dbUser,
    redshiftDataParameters_secretManagerArn,
    redshiftDataParameters_statementName,
    redshiftDataParameters_withEvent,
    redshiftDataParameters_database,
    redshiftDataParameters_sql,

    -- ** RemoveTargetsResultEntry
    removeTargetsResultEntry_errorCode,
    removeTargetsResultEntry_errorMessage,
    removeTargetsResultEntry_targetId,

    -- ** Replay
    replay_eventEndTime,
    replay_eventLastReplayedTime,
    replay_eventSourceArn,
    replay_eventStartTime,
    replay_replayEndTime,
    replay_replayName,
    replay_replayStartTime,
    replay_state,
    replay_stateReason,

    -- ** ReplayDestination
    replayDestination_filterArns,
    replayDestination_arn,

    -- ** ReplicationConfig
    replicationConfig_state,

    -- ** RetryPolicy
    retryPolicy_maximumEventAgeInSeconds,
    retryPolicy_maximumRetryAttempts,

    -- ** RoutingConfig
    routingConfig_failoverConfig,

    -- ** Rule
    rule_arn,
    rule_description,
    rule_eventBusName,
    rule_eventPattern,
    rule_managedBy,
    rule_name,
    rule_roleArn,
    rule_scheduleExpression,
    rule_state,

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

    -- ** Secondary
    secondary_route,

    -- ** SqsParameters
    sqsParameters_messageGroupId,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Target
    target_batchParameters,
    target_deadLetterConfig,
    target_ecsParameters,
    target_httpParameters,
    target_input,
    target_inputPath,
    target_inputTransformer,
    target_kinesisParameters,
    target_redshiftDataParameters,
    target_retryPolicy,
    target_roleArn,
    target_runCommandParameters,
    target_sageMakerPipelineParameters,
    target_sqsParameters,
    target_id,
    target_arn,

    -- ** UpdateConnectionApiKeyAuthRequestParameters
    updateConnectionApiKeyAuthRequestParameters_apiKeyName,
    updateConnectionApiKeyAuthRequestParameters_apiKeyValue,

    -- ** UpdateConnectionAuthRequestParameters
    updateConnectionAuthRequestParameters_apiKeyAuthParameters,
    updateConnectionAuthRequestParameters_basicAuthParameters,
    updateConnectionAuthRequestParameters_invocationHttpParameters,
    updateConnectionAuthRequestParameters_oAuthParameters,

    -- ** UpdateConnectionBasicAuthRequestParameters
    updateConnectionBasicAuthRequestParameters_password,
    updateConnectionBasicAuthRequestParameters_username,

    -- ** UpdateConnectionOAuthClientRequestParameters
    updateConnectionOAuthClientRequestParameters_clientID,
    updateConnectionOAuthClientRequestParameters_clientSecret,

    -- ** UpdateConnectionOAuthRequestParameters
    updateConnectionOAuthRequestParameters_authorizationEndpoint,
    updateConnectionOAuthRequestParameters_clientParameters,
    updateConnectionOAuthRequestParameters_httpMethod,
    updateConnectionOAuthRequestParameters_oAuthHttpParameters,
  )
where

import Amazonka.CloudWatchEvents.ActivateEventSource
import Amazonka.CloudWatchEvents.CancelReplay
import Amazonka.CloudWatchEvents.CreateApiDestination
import Amazonka.CloudWatchEvents.CreateArchive
import Amazonka.CloudWatchEvents.CreateConnection
import Amazonka.CloudWatchEvents.CreateEndpoint
import Amazonka.CloudWatchEvents.CreateEventBus
import Amazonka.CloudWatchEvents.CreatePartnerEventSource
import Amazonka.CloudWatchEvents.DeactivateEventSource
import Amazonka.CloudWatchEvents.DeauthorizeConnection
import Amazonka.CloudWatchEvents.DeleteApiDestination
import Amazonka.CloudWatchEvents.DeleteArchive
import Amazonka.CloudWatchEvents.DeleteConnection
import Amazonka.CloudWatchEvents.DeleteEndpoint
import Amazonka.CloudWatchEvents.DeleteEventBus
import Amazonka.CloudWatchEvents.DeletePartnerEventSource
import Amazonka.CloudWatchEvents.DeleteRule
import Amazonka.CloudWatchEvents.DescribeApiDestination
import Amazonka.CloudWatchEvents.DescribeArchive
import Amazonka.CloudWatchEvents.DescribeConnection
import Amazonka.CloudWatchEvents.DescribeEndpoint
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
import Amazonka.CloudWatchEvents.ListEndpoints
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
import Amazonka.CloudWatchEvents.Types.Endpoint
import Amazonka.CloudWatchEvents.Types.EndpointEventBus
import Amazonka.CloudWatchEvents.Types.EventBus
import Amazonka.CloudWatchEvents.Types.EventSource
import Amazonka.CloudWatchEvents.Types.FailoverConfig
import Amazonka.CloudWatchEvents.Types.HttpParameters
import Amazonka.CloudWatchEvents.Types.InputTransformer
import Amazonka.CloudWatchEvents.Types.KinesisParameters
import Amazonka.CloudWatchEvents.Types.NetworkConfiguration
import Amazonka.CloudWatchEvents.Types.PartnerEventSource
import Amazonka.CloudWatchEvents.Types.PartnerEventSourceAccount
import Amazonka.CloudWatchEvents.Types.PlacementConstraint
import Amazonka.CloudWatchEvents.Types.PlacementStrategy
import Amazonka.CloudWatchEvents.Types.Primary
import Amazonka.CloudWatchEvents.Types.PutEventsRequestEntry
import Amazonka.CloudWatchEvents.Types.PutEventsResultEntry
import Amazonka.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
import Amazonka.CloudWatchEvents.Types.PutPartnerEventsResultEntry
import Amazonka.CloudWatchEvents.Types.PutTargetsResultEntry
import Amazonka.CloudWatchEvents.Types.RedshiftDataParameters
import Amazonka.CloudWatchEvents.Types.RemoveTargetsResultEntry
import Amazonka.CloudWatchEvents.Types.Replay
import Amazonka.CloudWatchEvents.Types.ReplayDestination
import Amazonka.CloudWatchEvents.Types.ReplicationConfig
import Amazonka.CloudWatchEvents.Types.RetryPolicy
import Amazonka.CloudWatchEvents.Types.RoutingConfig
import Amazonka.CloudWatchEvents.Types.Rule
import Amazonka.CloudWatchEvents.Types.RunCommandParameters
import Amazonka.CloudWatchEvents.Types.RunCommandTarget
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameter
import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters
import Amazonka.CloudWatchEvents.Types.Secondary
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
import Amazonka.CloudWatchEvents.UpdateEndpoint
