{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppFlow.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Lens
  ( -- * Operations

    -- ** ListConnectorEntities
    listConnectorEntities_connectorProfileName,
    listConnectorEntities_entitiesPath,
    listConnectorEntities_connectorType,
    listConnectorEntitiesResponse_httpStatus,
    listConnectorEntitiesResponse_connectorEntityMap,

    -- ** CreateConnectorProfile
    createConnectorProfile_kmsArn,
    createConnectorProfile_connectorProfileName,
    createConnectorProfile_connectorType,
    createConnectorProfile_connectionMode,
    createConnectorProfile_connectorProfileConfig,
    createConnectorProfileResponse_connectorProfileArn,
    createConnectorProfileResponse_httpStatus,

    -- ** StartFlow
    startFlow_flowName,
    startFlowResponse_executionId,
    startFlowResponse_flowArn,
    startFlowResponse_flowStatus,
    startFlowResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateFlow
    createFlow_kmsArn,
    createFlow_description,
    createFlow_tags,
    createFlow_flowName,
    createFlow_triggerConfig,
    createFlow_sourceFlowConfig,
    createFlow_destinationFlowConfigList,
    createFlow_tasks,
    createFlowResponse_flowArn,
    createFlowResponse_flowStatus,
    createFlowResponse_httpStatus,

    -- ** DeleteConnectorProfile
    deleteConnectorProfile_forceDelete,
    deleteConnectorProfile_connectorProfileName,
    deleteConnectorProfileResponse_httpStatus,

    -- ** UpdateConnectorProfile
    updateConnectorProfile_connectorProfileName,
    updateConnectorProfile_connectionMode,
    updateConnectorProfile_connectorProfileConfig,
    updateConnectorProfileResponse_connectorProfileArn,
    updateConnectorProfileResponse_httpStatus,

    -- ** DescribeFlow
    describeFlow_flowName,
    describeFlowResponse_lastUpdatedBy,
    describeFlowResponse_flowArn,
    describeFlowResponse_lastUpdatedAt,
    describeFlowResponse_createdAt,
    describeFlowResponse_createdBy,
    describeFlowResponse_tasks,
    describeFlowResponse_triggerConfig,
    describeFlowResponse_flowName,
    describeFlowResponse_sourceFlowConfig,
    describeFlowResponse_flowStatusMessage,
    describeFlowResponse_kmsArn,
    describeFlowResponse_lastRunExecutionDetails,
    describeFlowResponse_flowStatus,
    describeFlowResponse_destinationFlowConfigList,
    describeFlowResponse_description,
    describeFlowResponse_tags,
    describeFlowResponse_httpStatus,

    -- ** StopFlow
    stopFlow_flowName,
    stopFlowResponse_flowArn,
    stopFlowResponse_flowStatus,
    stopFlowResponse_httpStatus,

    -- ** DescribeConnectors
    describeConnectors_nextToken,
    describeConnectors_connectorTypes,
    describeConnectorsResponse_connectorConfigurations,
    describeConnectorsResponse_nextToken,
    describeConnectorsResponse_httpStatus,

    -- ** DescribeConnectorEntity
    describeConnectorEntity_connectorProfileName,
    describeConnectorEntity_connectorType,
    describeConnectorEntity_connectorEntityName,
    describeConnectorEntityResponse_httpStatus,
    describeConnectorEntityResponse_connectorEntityFields,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListFlows
    listFlows_nextToken,
    listFlows_maxResults,
    listFlowsResponse_nextToken,
    listFlowsResponse_flows,
    listFlowsResponse_httpStatus,

    -- ** DescribeFlowExecutionRecords
    describeFlowExecutionRecords_nextToken,
    describeFlowExecutionRecords_maxResults,
    describeFlowExecutionRecords_flowName,
    describeFlowExecutionRecordsResponse_flowExecutions,
    describeFlowExecutionRecordsResponse_nextToken,
    describeFlowExecutionRecordsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateFlow
    updateFlow_description,
    updateFlow_flowName,
    updateFlow_triggerConfig,
    updateFlow_sourceFlowConfig,
    updateFlow_destinationFlowConfigList,
    updateFlow_tasks,
    updateFlowResponse_flowStatus,
    updateFlowResponse_httpStatus,

    -- ** DeleteFlow
    deleteFlow_forceDelete,
    deleteFlow_flowName,
    deleteFlowResponse_httpStatus,

    -- ** DescribeConnectorProfiles
    describeConnectorProfiles_connectorProfileNames,
    describeConnectorProfiles_nextToken,
    describeConnectorProfiles_connectorType,
    describeConnectorProfiles_maxResults,
    describeConnectorProfilesResponse_connectorProfileDetails,
    describeConnectorProfilesResponse_nextToken,
    describeConnectorProfilesResponse_httpStatus,

    -- * Types

    -- ** AggregationConfig
    aggregationConfig_aggregationType,

    -- ** AmplitudeConnectorProfileCredentials
    amplitudeConnectorProfileCredentials_apiKey,
    amplitudeConnectorProfileCredentials_secretKey,

    -- ** AmplitudeConnectorProfileProperties

    -- ** AmplitudeMetadata

    -- ** AmplitudeSourceProperties
    amplitudeSourceProperties_object,

    -- ** BasicAuthCredentials
    basicAuthCredentials_username,
    basicAuthCredentials_password,

    -- ** ConnectorConfiguration
    connectorConfiguration_isPrivateLinkEnabled,
    connectorConfiguration_supportedTriggerTypes,
    connectorConfiguration_canUseAsSource,
    connectorConfiguration_connectorMetadata,
    connectorConfiguration_canUseAsDestination,
    connectorConfiguration_supportedSchedulingFrequencies,
    connectorConfiguration_supportedDestinationConnectors,
    connectorConfiguration_isPrivateLinkEndpointUrlRequired,

    -- ** ConnectorEntity
    connectorEntity_hasNestedEntities,
    connectorEntity_label,
    connectorEntity_name,

    -- ** ConnectorEntityField
    connectorEntityField_sourceProperties,
    connectorEntityField_supportedFieldTypeDetails,
    connectorEntityField_destinationProperties,
    connectorEntityField_description,
    connectorEntityField_label,
    connectorEntityField_identifier,

    -- ** ConnectorMetadata
    connectorMetadata_upsolver,
    connectorMetadata_snowflake,
    connectorMetadata_honeycode,
    connectorMetadata_serviceNow,
    connectorMetadata_dynatrace,
    connectorMetadata_marketo,
    connectorMetadata_slack,
    connectorMetadata_singular,
    connectorMetadata_inforNexus,
    connectorMetadata_amplitude,
    connectorMetadata_customerProfiles,
    connectorMetadata_datadog,
    connectorMetadata_googleAnalytics,
    connectorMetadata_sAPOData,
    connectorMetadata_salesforce,
    connectorMetadata_zendesk,
    connectorMetadata_s3,
    connectorMetadata_eventBridge,
    connectorMetadata_trendmicro,
    connectorMetadata_redshift,
    connectorMetadata_veeva,

    -- ** ConnectorOAuthRequest
    connectorOAuthRequest_redirectUri,
    connectorOAuthRequest_authCode,

    -- ** ConnectorOperator
    connectorOperator_serviceNow,
    connectorOperator_dynatrace,
    connectorOperator_marketo,
    connectorOperator_slack,
    connectorOperator_singular,
    connectorOperator_inforNexus,
    connectorOperator_amplitude,
    connectorOperator_datadog,
    connectorOperator_googleAnalytics,
    connectorOperator_sAPOData,
    connectorOperator_salesforce,
    connectorOperator_zendesk,
    connectorOperator_s3,
    connectorOperator_trendmicro,
    connectorOperator_veeva,

    -- ** ConnectorProfile
    connectorProfile_connectorProfileName,
    connectorProfile_lastUpdatedAt,
    connectorProfile_createdAt,
    connectorProfile_credentialsArn,
    connectorProfile_connectorProfileProperties,
    connectorProfile_connectionMode,
    connectorProfile_connectorProfileArn,
    connectorProfile_privateConnectionProvisioningState,
    connectorProfile_connectorType,

    -- ** ConnectorProfileConfig
    connectorProfileConfig_connectorProfileProperties,
    connectorProfileConfig_connectorProfileCredentials,

    -- ** ConnectorProfileCredentials
    connectorProfileCredentials_snowflake,
    connectorProfileCredentials_honeycode,
    connectorProfileCredentials_serviceNow,
    connectorProfileCredentials_dynatrace,
    connectorProfileCredentials_marketo,
    connectorProfileCredentials_slack,
    connectorProfileCredentials_singular,
    connectorProfileCredentials_inforNexus,
    connectorProfileCredentials_amplitude,
    connectorProfileCredentials_datadog,
    connectorProfileCredentials_googleAnalytics,
    connectorProfileCredentials_sAPOData,
    connectorProfileCredentials_salesforce,
    connectorProfileCredentials_zendesk,
    connectorProfileCredentials_trendmicro,
    connectorProfileCredentials_redshift,
    connectorProfileCredentials_veeva,

    -- ** ConnectorProfileProperties
    connectorProfileProperties_snowflake,
    connectorProfileProperties_honeycode,
    connectorProfileProperties_serviceNow,
    connectorProfileProperties_dynatrace,
    connectorProfileProperties_marketo,
    connectorProfileProperties_slack,
    connectorProfileProperties_singular,
    connectorProfileProperties_inforNexus,
    connectorProfileProperties_amplitude,
    connectorProfileProperties_datadog,
    connectorProfileProperties_googleAnalytics,
    connectorProfileProperties_sAPOData,
    connectorProfileProperties_salesforce,
    connectorProfileProperties_zendesk,
    connectorProfileProperties_trendmicro,
    connectorProfileProperties_redshift,
    connectorProfileProperties_veeva,

    -- ** CustomerProfilesDestinationProperties
    customerProfilesDestinationProperties_objectTypeName,
    customerProfilesDestinationProperties_domainName,

    -- ** CustomerProfilesMetadata

    -- ** DatadogConnectorProfileCredentials
    datadogConnectorProfileCredentials_apiKey,
    datadogConnectorProfileCredentials_applicationKey,

    -- ** DatadogConnectorProfileProperties
    datadogConnectorProfileProperties_instanceUrl,

    -- ** DatadogMetadata

    -- ** DatadogSourceProperties
    datadogSourceProperties_object,

    -- ** DestinationConnectorProperties
    destinationConnectorProperties_upsolver,
    destinationConnectorProperties_snowflake,
    destinationConnectorProperties_honeycode,
    destinationConnectorProperties_lookoutMetrics,
    destinationConnectorProperties_customerProfiles,
    destinationConnectorProperties_salesforce,
    destinationConnectorProperties_zendesk,
    destinationConnectorProperties_s3,
    destinationConnectorProperties_eventBridge,
    destinationConnectorProperties_redshift,

    -- ** DestinationFieldProperties
    destinationFieldProperties_isUpdatable,
    destinationFieldProperties_isNullable,
    destinationFieldProperties_supportedWriteOperations,
    destinationFieldProperties_isUpsertable,
    destinationFieldProperties_isCreatable,

    -- ** DestinationFlowConfig
    destinationFlowConfig_connectorProfileName,
    destinationFlowConfig_connectorType,
    destinationFlowConfig_destinationConnectorProperties,

    -- ** DynatraceConnectorProfileCredentials
    dynatraceConnectorProfileCredentials_apiToken,

    -- ** DynatraceConnectorProfileProperties
    dynatraceConnectorProfileProperties_instanceUrl,

    -- ** DynatraceMetadata

    -- ** DynatraceSourceProperties
    dynatraceSourceProperties_object,

    -- ** ErrorHandlingConfig
    errorHandlingConfig_failOnFirstDestinationError,
    errorHandlingConfig_bucketPrefix,
    errorHandlingConfig_bucketName,

    -- ** ErrorInfo
    errorInfo_executionMessage,
    errorInfo_putFailuresCount,

    -- ** EventBridgeDestinationProperties
    eventBridgeDestinationProperties_errorHandlingConfig,
    eventBridgeDestinationProperties_object,

    -- ** EventBridgeMetadata

    -- ** ExecutionDetails
    executionDetails_mostRecentExecutionMessage,
    executionDetails_mostRecentExecutionStatus,
    executionDetails_mostRecentExecutionTime,

    -- ** ExecutionRecord
    executionRecord_executionId,
    executionRecord_lastUpdatedAt,
    executionRecord_dataPullStartTime,
    executionRecord_executionStatus,
    executionRecord_startedAt,
    executionRecord_dataPullEndTime,
    executionRecord_executionResult,

    -- ** ExecutionResult
    executionResult_recordsProcessed,
    executionResult_bytesWritten,
    executionResult_bytesProcessed,
    executionResult_errorInfo,

    -- ** FieldTypeDetails
    fieldTypeDetails_supportedValues,
    fieldTypeDetails_fieldType,
    fieldTypeDetails_filterOperators,

    -- ** FlowDefinition
    flowDefinition_lastUpdatedBy,
    flowDefinition_flowArn,
    flowDefinition_lastUpdatedAt,
    flowDefinition_createdAt,
    flowDefinition_triggerType,
    flowDefinition_createdBy,
    flowDefinition_sourceConnectorType,
    flowDefinition_destinationConnectorType,
    flowDefinition_flowName,
    flowDefinition_lastRunExecutionDetails,
    flowDefinition_flowStatus,
    flowDefinition_description,
    flowDefinition_tags,

    -- ** GoogleAnalyticsConnectorProfileCredentials
    googleAnalyticsConnectorProfileCredentials_accessToken,
    googleAnalyticsConnectorProfileCredentials_refreshToken,
    googleAnalyticsConnectorProfileCredentials_oAuthRequest,
    googleAnalyticsConnectorProfileCredentials_clientId,
    googleAnalyticsConnectorProfileCredentials_clientSecret,

    -- ** GoogleAnalyticsConnectorProfileProperties

    -- ** GoogleAnalyticsMetadata
    googleAnalyticsMetadata_oAuthScopes,

    -- ** GoogleAnalyticsSourceProperties
    googleAnalyticsSourceProperties_object,

    -- ** HoneycodeConnectorProfileCredentials
    honeycodeConnectorProfileCredentials_accessToken,
    honeycodeConnectorProfileCredentials_refreshToken,
    honeycodeConnectorProfileCredentials_oAuthRequest,

    -- ** HoneycodeConnectorProfileProperties

    -- ** HoneycodeDestinationProperties
    honeycodeDestinationProperties_errorHandlingConfig,
    honeycodeDestinationProperties_object,

    -- ** HoneycodeMetadata
    honeycodeMetadata_oAuthScopes,

    -- ** IncrementalPullConfig
    incrementalPullConfig_datetimeTypeFieldName,

    -- ** InforNexusConnectorProfileCredentials
    inforNexusConnectorProfileCredentials_accessKeyId,
    inforNexusConnectorProfileCredentials_userId,
    inforNexusConnectorProfileCredentials_secretAccessKey,
    inforNexusConnectorProfileCredentials_datakey,

    -- ** InforNexusConnectorProfileProperties
    inforNexusConnectorProfileProperties_instanceUrl,

    -- ** InforNexusMetadata

    -- ** InforNexusSourceProperties
    inforNexusSourceProperties_object,

    -- ** LookoutMetricsDestinationProperties

    -- ** MarketoConnectorProfileCredentials
    marketoConnectorProfileCredentials_accessToken,
    marketoConnectorProfileCredentials_oAuthRequest,
    marketoConnectorProfileCredentials_clientId,
    marketoConnectorProfileCredentials_clientSecret,

    -- ** MarketoConnectorProfileProperties
    marketoConnectorProfileProperties_instanceUrl,

    -- ** MarketoMetadata

    -- ** MarketoSourceProperties
    marketoSourceProperties_object,

    -- ** OAuthCredentials
    oAuthCredentials_accessToken,
    oAuthCredentials_refreshToken,
    oAuthCredentials_oAuthRequest,
    oAuthCredentials_clientId,
    oAuthCredentials_clientSecret,

    -- ** OAuthProperties
    oAuthProperties_tokenUrl,
    oAuthProperties_authCodeUrl,
    oAuthProperties_oAuthScopes,

    -- ** PrefixConfig
    prefixConfig_prefixFormat,
    prefixConfig_prefixType,

    -- ** PrivateConnectionProvisioningState
    privateConnectionProvisioningState_status,
    privateConnectionProvisioningState_failureMessage,
    privateConnectionProvisioningState_failureCause,

    -- ** RedshiftConnectorProfileCredentials
    redshiftConnectorProfileCredentials_username,
    redshiftConnectorProfileCredentials_password,

    -- ** RedshiftConnectorProfileProperties
    redshiftConnectorProfileProperties_bucketPrefix,
    redshiftConnectorProfileProperties_databaseUrl,
    redshiftConnectorProfileProperties_bucketName,
    redshiftConnectorProfileProperties_roleArn,

    -- ** RedshiftDestinationProperties
    redshiftDestinationProperties_bucketPrefix,
    redshiftDestinationProperties_errorHandlingConfig,
    redshiftDestinationProperties_object,
    redshiftDestinationProperties_intermediateBucketName,

    -- ** RedshiftMetadata

    -- ** S3DestinationProperties
    s3DestinationProperties_bucketPrefix,
    s3DestinationProperties_s3OutputFormatConfig,
    s3DestinationProperties_bucketName,

    -- ** S3InputFormatConfig
    s3InputFormatConfig_s3InputFileType,

    -- ** S3Metadata

    -- ** S3OutputFormatConfig
    s3OutputFormatConfig_prefixConfig,
    s3OutputFormatConfig_fileType,
    s3OutputFormatConfig_aggregationConfig,

    -- ** S3SourceProperties
    s3SourceProperties_s3InputFormatConfig,
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- ** SAPODataConnectorProfileCredentials
    sAPODataConnectorProfileCredentials_oAuthCredentials,
    sAPODataConnectorProfileCredentials_basicAuthCredentials,

    -- ** SAPODataConnectorProfileProperties
    sAPODataConnectorProfileProperties_logonLanguage,
    sAPODataConnectorProfileProperties_privateLinkServiceName,
    sAPODataConnectorProfileProperties_oAuthProperties,
    sAPODataConnectorProfileProperties_applicationHostUrl,
    sAPODataConnectorProfileProperties_applicationServicePath,
    sAPODataConnectorProfileProperties_portNumber,
    sAPODataConnectorProfileProperties_clientNumber,

    -- ** SAPODataMetadata

    -- ** SAPODataSourceProperties
    sAPODataSourceProperties_objectPath,

    -- ** SalesforceConnectorProfileCredentials
    salesforceConnectorProfileCredentials_accessToken,
    salesforceConnectorProfileCredentials_refreshToken,
    salesforceConnectorProfileCredentials_oAuthRequest,
    salesforceConnectorProfileCredentials_clientCredentialsArn,

    -- ** SalesforceConnectorProfileProperties
    salesforceConnectorProfileProperties_instanceUrl,
    salesforceConnectorProfileProperties_isSandboxEnvironment,

    -- ** SalesforceDestinationProperties
    salesforceDestinationProperties_writeOperationType,
    salesforceDestinationProperties_idFieldNames,
    salesforceDestinationProperties_errorHandlingConfig,
    salesforceDestinationProperties_object,

    -- ** SalesforceMetadata
    salesforceMetadata_oAuthScopes,

    -- ** SalesforceSourceProperties
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_object,

    -- ** ScheduledTriggerProperties
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_scheduleExpression,

    -- ** ServiceNowConnectorProfileCredentials
    serviceNowConnectorProfileCredentials_username,
    serviceNowConnectorProfileCredentials_password,

    -- ** ServiceNowConnectorProfileProperties
    serviceNowConnectorProfileProperties_instanceUrl,

    -- ** ServiceNowMetadata

    -- ** ServiceNowSourceProperties
    serviceNowSourceProperties_object,

    -- ** SingularConnectorProfileCredentials
    singularConnectorProfileCredentials_apiKey,

    -- ** SingularConnectorProfileProperties

    -- ** SingularMetadata

    -- ** SingularSourceProperties
    singularSourceProperties_object,

    -- ** SlackConnectorProfileCredentials
    slackConnectorProfileCredentials_accessToken,
    slackConnectorProfileCredentials_oAuthRequest,
    slackConnectorProfileCredentials_clientId,
    slackConnectorProfileCredentials_clientSecret,

    -- ** SlackConnectorProfileProperties
    slackConnectorProfileProperties_instanceUrl,

    -- ** SlackMetadata
    slackMetadata_oAuthScopes,

    -- ** SlackSourceProperties
    slackSourceProperties_object,

    -- ** SnowflakeConnectorProfileCredentials
    snowflakeConnectorProfileCredentials_username,
    snowflakeConnectorProfileCredentials_password,

    -- ** SnowflakeConnectorProfileProperties
    snowflakeConnectorProfileProperties_privateLinkServiceName,
    snowflakeConnectorProfileProperties_accountName,
    snowflakeConnectorProfileProperties_bucketPrefix,
    snowflakeConnectorProfileProperties_region,
    snowflakeConnectorProfileProperties_warehouse,
    snowflakeConnectorProfileProperties_stage,
    snowflakeConnectorProfileProperties_bucketName,

    -- ** SnowflakeDestinationProperties
    snowflakeDestinationProperties_bucketPrefix,
    snowflakeDestinationProperties_errorHandlingConfig,
    snowflakeDestinationProperties_object,
    snowflakeDestinationProperties_intermediateBucketName,

    -- ** SnowflakeMetadata
    snowflakeMetadata_supportedRegions,

    -- ** SourceConnectorProperties
    sourceConnectorProperties_serviceNow,
    sourceConnectorProperties_dynatrace,
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_slack,
    sourceConnectorProperties_singular,
    sourceConnectorProperties_inforNexus,
    sourceConnectorProperties_amplitude,
    sourceConnectorProperties_datadog,
    sourceConnectorProperties_googleAnalytics,
    sourceConnectorProperties_sAPOData,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_zendesk,
    sourceConnectorProperties_s3,
    sourceConnectorProperties_trendmicro,
    sourceConnectorProperties_veeva,

    -- ** SourceFieldProperties
    sourceFieldProperties_isRetrievable,
    sourceFieldProperties_isQueryable,

    -- ** SourceFlowConfig
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- ** SupportedFieldTypeDetails
    supportedFieldTypeDetails_v1,

    -- ** Task
    task_taskProperties,
    task_connectorOperator,
    task_destinationField,
    task_sourceFields,
    task_taskType,

    -- ** TrendmicroConnectorProfileCredentials
    trendmicroConnectorProfileCredentials_apiSecretKey,

    -- ** TrendmicroConnectorProfileProperties

    -- ** TrendmicroMetadata

    -- ** TrendmicroSourceProperties
    trendmicroSourceProperties_object,

    -- ** TriggerConfig
    triggerConfig_triggerProperties,
    triggerConfig_triggerType,

    -- ** TriggerProperties
    triggerProperties_scheduled,

    -- ** UpsolverDestinationProperties
    upsolverDestinationProperties_bucketPrefix,
    upsolverDestinationProperties_bucketName,
    upsolverDestinationProperties_s3OutputFormatConfig,

    -- ** UpsolverMetadata

    -- ** UpsolverS3OutputFormatConfig
    upsolverS3OutputFormatConfig_fileType,
    upsolverS3OutputFormatConfig_aggregationConfig,
    upsolverS3OutputFormatConfig_prefixConfig,

    -- ** VeevaConnectorProfileCredentials
    veevaConnectorProfileCredentials_username,
    veevaConnectorProfileCredentials_password,

    -- ** VeevaConnectorProfileProperties
    veevaConnectorProfileProperties_instanceUrl,

    -- ** VeevaMetadata

    -- ** VeevaSourceProperties
    veevaSourceProperties_includeAllVersions,
    veevaSourceProperties_documentType,
    veevaSourceProperties_includeRenditions,
    veevaSourceProperties_includeSourceFiles,
    veevaSourceProperties_object,

    -- ** ZendeskConnectorProfileCredentials
    zendeskConnectorProfileCredentials_accessToken,
    zendeskConnectorProfileCredentials_oAuthRequest,
    zendeskConnectorProfileCredentials_clientId,
    zendeskConnectorProfileCredentials_clientSecret,

    -- ** ZendeskConnectorProfileProperties
    zendeskConnectorProfileProperties_instanceUrl,

    -- ** ZendeskDestinationProperties
    zendeskDestinationProperties_writeOperationType,
    zendeskDestinationProperties_idFieldNames,
    zendeskDestinationProperties_errorHandlingConfig,
    zendeskDestinationProperties_object,

    -- ** ZendeskMetadata
    zendeskMetadata_oAuthScopes,

    -- ** ZendeskSourceProperties
    zendeskSourceProperties_object,
  )
where

import Network.AWS.AppFlow.CreateConnectorProfile
import Network.AWS.AppFlow.CreateFlow
import Network.AWS.AppFlow.DeleteConnectorProfile
import Network.AWS.AppFlow.DeleteFlow
import Network.AWS.AppFlow.DescribeConnectorEntity
import Network.AWS.AppFlow.DescribeConnectorProfiles
import Network.AWS.AppFlow.DescribeConnectors
import Network.AWS.AppFlow.DescribeFlow
import Network.AWS.AppFlow.DescribeFlowExecutionRecords
import Network.AWS.AppFlow.ListConnectorEntities
import Network.AWS.AppFlow.ListFlows
import Network.AWS.AppFlow.ListTagsForResource
import Network.AWS.AppFlow.StartFlow
import Network.AWS.AppFlow.StopFlow
import Network.AWS.AppFlow.TagResource
import Network.AWS.AppFlow.Types.AggregationConfig
import Network.AWS.AppFlow.Types.AmplitudeConnectorProfileCredentials
import Network.AWS.AppFlow.Types.AmplitudeConnectorProfileProperties
import Network.AWS.AppFlow.Types.AmplitudeMetadata
import Network.AWS.AppFlow.Types.AmplitudeSourceProperties
import Network.AWS.AppFlow.Types.BasicAuthCredentials
import Network.AWS.AppFlow.Types.ConnectorConfiguration
import Network.AWS.AppFlow.Types.ConnectorEntity
import Network.AWS.AppFlow.Types.ConnectorEntityField
import Network.AWS.AppFlow.Types.ConnectorMetadata
import Network.AWS.AppFlow.Types.ConnectorOAuthRequest
import Network.AWS.AppFlow.Types.ConnectorOperator
import Network.AWS.AppFlow.Types.ConnectorProfile
import Network.AWS.AppFlow.Types.ConnectorProfileConfig
import Network.AWS.AppFlow.Types.ConnectorProfileCredentials
import Network.AWS.AppFlow.Types.ConnectorProfileProperties
import Network.AWS.AppFlow.Types.CustomerProfilesDestinationProperties
import Network.AWS.AppFlow.Types.CustomerProfilesMetadata
import Network.AWS.AppFlow.Types.DatadogConnectorProfileCredentials
import Network.AWS.AppFlow.Types.DatadogConnectorProfileProperties
import Network.AWS.AppFlow.Types.DatadogMetadata
import Network.AWS.AppFlow.Types.DatadogSourceProperties
import Network.AWS.AppFlow.Types.DestinationConnectorProperties
import Network.AWS.AppFlow.Types.DestinationFieldProperties
import Network.AWS.AppFlow.Types.DestinationFlowConfig
import Network.AWS.AppFlow.Types.DynatraceConnectorProfileCredentials
import Network.AWS.AppFlow.Types.DynatraceConnectorProfileProperties
import Network.AWS.AppFlow.Types.DynatraceMetadata
import Network.AWS.AppFlow.Types.DynatraceSourceProperties
import Network.AWS.AppFlow.Types.ErrorHandlingConfig
import Network.AWS.AppFlow.Types.ErrorInfo
import Network.AWS.AppFlow.Types.EventBridgeDestinationProperties
import Network.AWS.AppFlow.Types.EventBridgeMetadata
import Network.AWS.AppFlow.Types.ExecutionDetails
import Network.AWS.AppFlow.Types.ExecutionRecord
import Network.AWS.AppFlow.Types.ExecutionResult
import Network.AWS.AppFlow.Types.FieldTypeDetails
import Network.AWS.AppFlow.Types.FlowDefinition
import Network.AWS.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
import Network.AWS.AppFlow.Types.GoogleAnalyticsConnectorProfileProperties
import Network.AWS.AppFlow.Types.GoogleAnalyticsMetadata
import Network.AWS.AppFlow.Types.GoogleAnalyticsSourceProperties
import Network.AWS.AppFlow.Types.HoneycodeConnectorProfileCredentials
import Network.AWS.AppFlow.Types.HoneycodeConnectorProfileProperties
import Network.AWS.AppFlow.Types.HoneycodeDestinationProperties
import Network.AWS.AppFlow.Types.HoneycodeMetadata
import Network.AWS.AppFlow.Types.IncrementalPullConfig
import Network.AWS.AppFlow.Types.InforNexusConnectorProfileCredentials
import Network.AWS.AppFlow.Types.InforNexusConnectorProfileProperties
import Network.AWS.AppFlow.Types.InforNexusMetadata
import Network.AWS.AppFlow.Types.InforNexusSourceProperties
import Network.AWS.AppFlow.Types.LookoutMetricsDestinationProperties
import Network.AWS.AppFlow.Types.MarketoConnectorProfileCredentials
import Network.AWS.AppFlow.Types.MarketoConnectorProfileProperties
import Network.AWS.AppFlow.Types.MarketoMetadata
import Network.AWS.AppFlow.Types.MarketoSourceProperties
import Network.AWS.AppFlow.Types.OAuthCredentials
import Network.AWS.AppFlow.Types.OAuthProperties
import Network.AWS.AppFlow.Types.PrefixConfig
import Network.AWS.AppFlow.Types.PrivateConnectionProvisioningState
import Network.AWS.AppFlow.Types.RedshiftConnectorProfileCredentials
import Network.AWS.AppFlow.Types.RedshiftConnectorProfileProperties
import Network.AWS.AppFlow.Types.RedshiftDestinationProperties
import Network.AWS.AppFlow.Types.RedshiftMetadata
import Network.AWS.AppFlow.Types.S3DestinationProperties
import Network.AWS.AppFlow.Types.S3InputFormatConfig
import Network.AWS.AppFlow.Types.S3Metadata
import Network.AWS.AppFlow.Types.S3OutputFormatConfig
import Network.AWS.AppFlow.Types.S3SourceProperties
import Network.AWS.AppFlow.Types.SAPODataConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SAPODataConnectorProfileProperties
import Network.AWS.AppFlow.Types.SAPODataMetadata
import Network.AWS.AppFlow.Types.SAPODataSourceProperties
import Network.AWS.AppFlow.Types.SalesforceConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SalesforceConnectorProfileProperties
import Network.AWS.AppFlow.Types.SalesforceDestinationProperties
import Network.AWS.AppFlow.Types.SalesforceMetadata
import Network.AWS.AppFlow.Types.SalesforceSourceProperties
import Network.AWS.AppFlow.Types.ScheduledTriggerProperties
import Network.AWS.AppFlow.Types.ServiceNowConnectorProfileCredentials
import Network.AWS.AppFlow.Types.ServiceNowConnectorProfileProperties
import Network.AWS.AppFlow.Types.ServiceNowMetadata
import Network.AWS.AppFlow.Types.ServiceNowSourceProperties
import Network.AWS.AppFlow.Types.SingularConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SingularConnectorProfileProperties
import Network.AWS.AppFlow.Types.SingularMetadata
import Network.AWS.AppFlow.Types.SingularSourceProperties
import Network.AWS.AppFlow.Types.SlackConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SlackConnectorProfileProperties
import Network.AWS.AppFlow.Types.SlackMetadata
import Network.AWS.AppFlow.Types.SlackSourceProperties
import Network.AWS.AppFlow.Types.SnowflakeConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SnowflakeConnectorProfileProperties
import Network.AWS.AppFlow.Types.SnowflakeDestinationProperties
import Network.AWS.AppFlow.Types.SnowflakeMetadata
import Network.AWS.AppFlow.Types.SourceConnectorProperties
import Network.AWS.AppFlow.Types.SourceFieldProperties
import Network.AWS.AppFlow.Types.SourceFlowConfig
import Network.AWS.AppFlow.Types.SupportedFieldTypeDetails
import Network.AWS.AppFlow.Types.Task
import Network.AWS.AppFlow.Types.TrendmicroConnectorProfileCredentials
import Network.AWS.AppFlow.Types.TrendmicroConnectorProfileProperties
import Network.AWS.AppFlow.Types.TrendmicroMetadata
import Network.AWS.AppFlow.Types.TrendmicroSourceProperties
import Network.AWS.AppFlow.Types.TriggerConfig
import Network.AWS.AppFlow.Types.TriggerProperties
import Network.AWS.AppFlow.Types.UpsolverDestinationProperties
import Network.AWS.AppFlow.Types.UpsolverMetadata
import Network.AWS.AppFlow.Types.UpsolverS3OutputFormatConfig
import Network.AWS.AppFlow.Types.VeevaConnectorProfileCredentials
import Network.AWS.AppFlow.Types.VeevaConnectorProfileProperties
import Network.AWS.AppFlow.Types.VeevaMetadata
import Network.AWS.AppFlow.Types.VeevaSourceProperties
import Network.AWS.AppFlow.Types.ZendeskConnectorProfileCredentials
import Network.AWS.AppFlow.Types.ZendeskConnectorProfileProperties
import Network.AWS.AppFlow.Types.ZendeskDestinationProperties
import Network.AWS.AppFlow.Types.ZendeskMetadata
import Network.AWS.AppFlow.Types.ZendeskSourceProperties
import Network.AWS.AppFlow.UntagResource
import Network.AWS.AppFlow.UpdateConnectorProfile
import Network.AWS.AppFlow.UpdateFlow
