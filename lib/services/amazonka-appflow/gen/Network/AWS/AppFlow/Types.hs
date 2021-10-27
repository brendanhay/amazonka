{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppFlow.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _UnsupportedOperationException,
    _ConflictException,
    _ConnectorAuthenticationException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ConnectorServerException,

    -- * AggregationType
    AggregationType (..),

    -- * AmplitudeConnectorOperator
    AmplitudeConnectorOperator (..),

    -- * ConnectionMode
    ConnectionMode (..),

    -- * ConnectorType
    ConnectorType (..),

    -- * DataPullMode
    DataPullMode (..),

    -- * DatadogConnectorOperator
    DatadogConnectorOperator (..),

    -- * DynatraceConnectorOperator
    DynatraceConnectorOperator (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * FileType
    FileType (..),

    -- * FlowStatus
    FlowStatus (..),

    -- * GoogleAnalyticsConnectorOperator
    GoogleAnalyticsConnectorOperator (..),

    -- * InforNexusConnectorOperator
    InforNexusConnectorOperator (..),

    -- * MarketoConnectorOperator
    MarketoConnectorOperator (..),

    -- * Operator
    Operator (..),

    -- * OperatorPropertiesKeys
    OperatorPropertiesKeys (..),

    -- * PrefixFormat
    PrefixFormat (..),

    -- * PrefixType
    PrefixType (..),

    -- * PrivateConnectionProvisioningFailureCause
    PrivateConnectionProvisioningFailureCause (..),

    -- * PrivateConnectionProvisioningStatus
    PrivateConnectionProvisioningStatus (..),

    -- * S3ConnectorOperator
    S3ConnectorOperator (..),

    -- * S3InputFileType
    S3InputFileType (..),

    -- * SAPODataConnectorOperator
    SAPODataConnectorOperator (..),

    -- * SalesforceConnectorOperator
    SalesforceConnectorOperator (..),

    -- * ScheduleFrequencyType
    ScheduleFrequencyType (..),

    -- * ServiceNowConnectorOperator
    ServiceNowConnectorOperator (..),

    -- * SingularConnectorOperator
    SingularConnectorOperator (..),

    -- * SlackConnectorOperator
    SlackConnectorOperator (..),

    -- * TaskType
    TaskType (..),

    -- * TrendmicroConnectorOperator
    TrendmicroConnectorOperator (..),

    -- * TriggerType
    TriggerType (..),

    -- * VeevaConnectorOperator
    VeevaConnectorOperator (..),

    -- * WriteOperationType
    WriteOperationType (..),

    -- * ZendeskConnectorOperator
    ZendeskConnectorOperator (..),

    -- * AggregationConfig
    AggregationConfig (..),
    newAggregationConfig,
    aggregationConfig_aggregationType,

    -- * AmplitudeConnectorProfileCredentials
    AmplitudeConnectorProfileCredentials (..),
    newAmplitudeConnectorProfileCredentials,
    amplitudeConnectorProfileCredentials_apiKey,
    amplitudeConnectorProfileCredentials_secretKey,

    -- * AmplitudeConnectorProfileProperties
    AmplitudeConnectorProfileProperties (..),
    newAmplitudeConnectorProfileProperties,

    -- * AmplitudeMetadata
    AmplitudeMetadata (..),
    newAmplitudeMetadata,

    -- * AmplitudeSourceProperties
    AmplitudeSourceProperties (..),
    newAmplitudeSourceProperties,
    amplitudeSourceProperties_object,

    -- * BasicAuthCredentials
    BasicAuthCredentials (..),
    newBasicAuthCredentials,
    basicAuthCredentials_username,
    basicAuthCredentials_password,

    -- * ConnectorConfiguration
    ConnectorConfiguration (..),
    newConnectorConfiguration,
    connectorConfiguration_isPrivateLinkEnabled,
    connectorConfiguration_supportedTriggerTypes,
    connectorConfiguration_canUseAsSource,
    connectorConfiguration_connectorMetadata,
    connectorConfiguration_canUseAsDestination,
    connectorConfiguration_supportedSchedulingFrequencies,
    connectorConfiguration_supportedDestinationConnectors,
    connectorConfiguration_isPrivateLinkEndpointUrlRequired,

    -- * ConnectorEntity
    ConnectorEntity (..),
    newConnectorEntity,
    connectorEntity_hasNestedEntities,
    connectorEntity_label,
    connectorEntity_name,

    -- * ConnectorEntityField
    ConnectorEntityField (..),
    newConnectorEntityField,
    connectorEntityField_sourceProperties,
    connectorEntityField_supportedFieldTypeDetails,
    connectorEntityField_destinationProperties,
    connectorEntityField_description,
    connectorEntityField_label,
    connectorEntityField_identifier,

    -- * ConnectorMetadata
    ConnectorMetadata (..),
    newConnectorMetadata,
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

    -- * ConnectorOAuthRequest
    ConnectorOAuthRequest (..),
    newConnectorOAuthRequest,
    connectorOAuthRequest_redirectUri,
    connectorOAuthRequest_authCode,

    -- * ConnectorOperator
    ConnectorOperator (..),
    newConnectorOperator,
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

    -- * ConnectorProfile
    ConnectorProfile (..),
    newConnectorProfile,
    connectorProfile_connectorProfileName,
    connectorProfile_lastUpdatedAt,
    connectorProfile_createdAt,
    connectorProfile_credentialsArn,
    connectorProfile_connectorProfileProperties,
    connectorProfile_connectionMode,
    connectorProfile_connectorProfileArn,
    connectorProfile_privateConnectionProvisioningState,
    connectorProfile_connectorType,

    -- * ConnectorProfileConfig
    ConnectorProfileConfig (..),
    newConnectorProfileConfig,
    connectorProfileConfig_connectorProfileProperties,
    connectorProfileConfig_connectorProfileCredentials,

    -- * ConnectorProfileCredentials
    ConnectorProfileCredentials (..),
    newConnectorProfileCredentials,
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

    -- * ConnectorProfileProperties
    ConnectorProfileProperties (..),
    newConnectorProfileProperties,
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

    -- * CustomerProfilesDestinationProperties
    CustomerProfilesDestinationProperties (..),
    newCustomerProfilesDestinationProperties,
    customerProfilesDestinationProperties_objectTypeName,
    customerProfilesDestinationProperties_domainName,

    -- * CustomerProfilesMetadata
    CustomerProfilesMetadata (..),
    newCustomerProfilesMetadata,

    -- * DatadogConnectorProfileCredentials
    DatadogConnectorProfileCredentials (..),
    newDatadogConnectorProfileCredentials,
    datadogConnectorProfileCredentials_apiKey,
    datadogConnectorProfileCredentials_applicationKey,

    -- * DatadogConnectorProfileProperties
    DatadogConnectorProfileProperties (..),
    newDatadogConnectorProfileProperties,
    datadogConnectorProfileProperties_instanceUrl,

    -- * DatadogMetadata
    DatadogMetadata (..),
    newDatadogMetadata,

    -- * DatadogSourceProperties
    DatadogSourceProperties (..),
    newDatadogSourceProperties,
    datadogSourceProperties_object,

    -- * DestinationConnectorProperties
    DestinationConnectorProperties (..),
    newDestinationConnectorProperties,
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

    -- * DestinationFieldProperties
    DestinationFieldProperties (..),
    newDestinationFieldProperties,
    destinationFieldProperties_isUpdatable,
    destinationFieldProperties_isNullable,
    destinationFieldProperties_supportedWriteOperations,
    destinationFieldProperties_isUpsertable,
    destinationFieldProperties_isCreatable,

    -- * DestinationFlowConfig
    DestinationFlowConfig (..),
    newDestinationFlowConfig,
    destinationFlowConfig_connectorProfileName,
    destinationFlowConfig_connectorType,
    destinationFlowConfig_destinationConnectorProperties,

    -- * DynatraceConnectorProfileCredentials
    DynatraceConnectorProfileCredentials (..),
    newDynatraceConnectorProfileCredentials,
    dynatraceConnectorProfileCredentials_apiToken,

    -- * DynatraceConnectorProfileProperties
    DynatraceConnectorProfileProperties (..),
    newDynatraceConnectorProfileProperties,
    dynatraceConnectorProfileProperties_instanceUrl,

    -- * DynatraceMetadata
    DynatraceMetadata (..),
    newDynatraceMetadata,

    -- * DynatraceSourceProperties
    DynatraceSourceProperties (..),
    newDynatraceSourceProperties,
    dynatraceSourceProperties_object,

    -- * ErrorHandlingConfig
    ErrorHandlingConfig (..),
    newErrorHandlingConfig,
    errorHandlingConfig_failOnFirstDestinationError,
    errorHandlingConfig_bucketPrefix,
    errorHandlingConfig_bucketName,

    -- * ErrorInfo
    ErrorInfo (..),
    newErrorInfo,
    errorInfo_executionMessage,
    errorInfo_putFailuresCount,

    -- * EventBridgeDestinationProperties
    EventBridgeDestinationProperties (..),
    newEventBridgeDestinationProperties,
    eventBridgeDestinationProperties_errorHandlingConfig,
    eventBridgeDestinationProperties_object,

    -- * EventBridgeMetadata
    EventBridgeMetadata (..),
    newEventBridgeMetadata,

    -- * ExecutionDetails
    ExecutionDetails (..),
    newExecutionDetails,
    executionDetails_mostRecentExecutionMessage,
    executionDetails_mostRecentExecutionStatus,
    executionDetails_mostRecentExecutionTime,

    -- * ExecutionRecord
    ExecutionRecord (..),
    newExecutionRecord,
    executionRecord_executionId,
    executionRecord_lastUpdatedAt,
    executionRecord_dataPullStartTime,
    executionRecord_executionStatus,
    executionRecord_startedAt,
    executionRecord_dataPullEndTime,
    executionRecord_executionResult,

    -- * ExecutionResult
    ExecutionResult (..),
    newExecutionResult,
    executionResult_recordsProcessed,
    executionResult_bytesWritten,
    executionResult_bytesProcessed,
    executionResult_errorInfo,

    -- * FieldTypeDetails
    FieldTypeDetails (..),
    newFieldTypeDetails,
    fieldTypeDetails_supportedValues,
    fieldTypeDetails_fieldType,
    fieldTypeDetails_filterOperators,

    -- * FlowDefinition
    FlowDefinition (..),
    newFlowDefinition,
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

    -- * GoogleAnalyticsConnectorProfileCredentials
    GoogleAnalyticsConnectorProfileCredentials (..),
    newGoogleAnalyticsConnectorProfileCredentials,
    googleAnalyticsConnectorProfileCredentials_accessToken,
    googleAnalyticsConnectorProfileCredentials_refreshToken,
    googleAnalyticsConnectorProfileCredentials_oAuthRequest,
    googleAnalyticsConnectorProfileCredentials_clientId,
    googleAnalyticsConnectorProfileCredentials_clientSecret,

    -- * GoogleAnalyticsConnectorProfileProperties
    GoogleAnalyticsConnectorProfileProperties (..),
    newGoogleAnalyticsConnectorProfileProperties,

    -- * GoogleAnalyticsMetadata
    GoogleAnalyticsMetadata (..),
    newGoogleAnalyticsMetadata,
    googleAnalyticsMetadata_oAuthScopes,

    -- * GoogleAnalyticsSourceProperties
    GoogleAnalyticsSourceProperties (..),
    newGoogleAnalyticsSourceProperties,
    googleAnalyticsSourceProperties_object,

    -- * HoneycodeConnectorProfileCredentials
    HoneycodeConnectorProfileCredentials (..),
    newHoneycodeConnectorProfileCredentials,
    honeycodeConnectorProfileCredentials_accessToken,
    honeycodeConnectorProfileCredentials_refreshToken,
    honeycodeConnectorProfileCredentials_oAuthRequest,

    -- * HoneycodeConnectorProfileProperties
    HoneycodeConnectorProfileProperties (..),
    newHoneycodeConnectorProfileProperties,

    -- * HoneycodeDestinationProperties
    HoneycodeDestinationProperties (..),
    newHoneycodeDestinationProperties,
    honeycodeDestinationProperties_errorHandlingConfig,
    honeycodeDestinationProperties_object,

    -- * HoneycodeMetadata
    HoneycodeMetadata (..),
    newHoneycodeMetadata,
    honeycodeMetadata_oAuthScopes,

    -- * IncrementalPullConfig
    IncrementalPullConfig (..),
    newIncrementalPullConfig,
    incrementalPullConfig_datetimeTypeFieldName,

    -- * InforNexusConnectorProfileCredentials
    InforNexusConnectorProfileCredentials (..),
    newInforNexusConnectorProfileCredentials,
    inforNexusConnectorProfileCredentials_accessKeyId,
    inforNexusConnectorProfileCredentials_userId,
    inforNexusConnectorProfileCredentials_secretAccessKey,
    inforNexusConnectorProfileCredentials_datakey,

    -- * InforNexusConnectorProfileProperties
    InforNexusConnectorProfileProperties (..),
    newInforNexusConnectorProfileProperties,
    inforNexusConnectorProfileProperties_instanceUrl,

    -- * InforNexusMetadata
    InforNexusMetadata (..),
    newInforNexusMetadata,

    -- * InforNexusSourceProperties
    InforNexusSourceProperties (..),
    newInforNexusSourceProperties,
    inforNexusSourceProperties_object,

    -- * LookoutMetricsDestinationProperties
    LookoutMetricsDestinationProperties (..),
    newLookoutMetricsDestinationProperties,

    -- * MarketoConnectorProfileCredentials
    MarketoConnectorProfileCredentials (..),
    newMarketoConnectorProfileCredentials,
    marketoConnectorProfileCredentials_accessToken,
    marketoConnectorProfileCredentials_oAuthRequest,
    marketoConnectorProfileCredentials_clientId,
    marketoConnectorProfileCredentials_clientSecret,

    -- * MarketoConnectorProfileProperties
    MarketoConnectorProfileProperties (..),
    newMarketoConnectorProfileProperties,
    marketoConnectorProfileProperties_instanceUrl,

    -- * MarketoMetadata
    MarketoMetadata (..),
    newMarketoMetadata,

    -- * MarketoSourceProperties
    MarketoSourceProperties (..),
    newMarketoSourceProperties,
    marketoSourceProperties_object,

    -- * OAuthCredentials
    OAuthCredentials (..),
    newOAuthCredentials,
    oAuthCredentials_accessToken,
    oAuthCredentials_refreshToken,
    oAuthCredentials_oAuthRequest,
    oAuthCredentials_clientId,
    oAuthCredentials_clientSecret,

    -- * OAuthProperties
    OAuthProperties (..),
    newOAuthProperties,
    oAuthProperties_tokenUrl,
    oAuthProperties_authCodeUrl,
    oAuthProperties_oAuthScopes,

    -- * PrefixConfig
    PrefixConfig (..),
    newPrefixConfig,
    prefixConfig_prefixFormat,
    prefixConfig_prefixType,

    -- * PrivateConnectionProvisioningState
    PrivateConnectionProvisioningState (..),
    newPrivateConnectionProvisioningState,
    privateConnectionProvisioningState_status,
    privateConnectionProvisioningState_failureMessage,
    privateConnectionProvisioningState_failureCause,

    -- * RedshiftConnectorProfileCredentials
    RedshiftConnectorProfileCredentials (..),
    newRedshiftConnectorProfileCredentials,
    redshiftConnectorProfileCredentials_username,
    redshiftConnectorProfileCredentials_password,

    -- * RedshiftConnectorProfileProperties
    RedshiftConnectorProfileProperties (..),
    newRedshiftConnectorProfileProperties,
    redshiftConnectorProfileProperties_bucketPrefix,
    redshiftConnectorProfileProperties_databaseUrl,
    redshiftConnectorProfileProperties_bucketName,
    redshiftConnectorProfileProperties_roleArn,

    -- * RedshiftDestinationProperties
    RedshiftDestinationProperties (..),
    newRedshiftDestinationProperties,
    redshiftDestinationProperties_bucketPrefix,
    redshiftDestinationProperties_errorHandlingConfig,
    redshiftDestinationProperties_object,
    redshiftDestinationProperties_intermediateBucketName,

    -- * RedshiftMetadata
    RedshiftMetadata (..),
    newRedshiftMetadata,

    -- * S3DestinationProperties
    S3DestinationProperties (..),
    newS3DestinationProperties,
    s3DestinationProperties_bucketPrefix,
    s3DestinationProperties_s3OutputFormatConfig,
    s3DestinationProperties_bucketName,

    -- * S3InputFormatConfig
    S3InputFormatConfig (..),
    newS3InputFormatConfig,
    s3InputFormatConfig_s3InputFileType,

    -- * S3Metadata
    S3Metadata (..),
    newS3Metadata,

    -- * S3OutputFormatConfig
    S3OutputFormatConfig (..),
    newS3OutputFormatConfig,
    s3OutputFormatConfig_prefixConfig,
    s3OutputFormatConfig_fileType,
    s3OutputFormatConfig_aggregationConfig,

    -- * S3SourceProperties
    S3SourceProperties (..),
    newS3SourceProperties,
    s3SourceProperties_s3InputFormatConfig,
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_bucketName,

    -- * SAPODataConnectorProfileCredentials
    SAPODataConnectorProfileCredentials (..),
    newSAPODataConnectorProfileCredentials,
    sAPODataConnectorProfileCredentials_oAuthCredentials,
    sAPODataConnectorProfileCredentials_basicAuthCredentials,

    -- * SAPODataConnectorProfileProperties
    SAPODataConnectorProfileProperties (..),
    newSAPODataConnectorProfileProperties,
    sAPODataConnectorProfileProperties_logonLanguage,
    sAPODataConnectorProfileProperties_privateLinkServiceName,
    sAPODataConnectorProfileProperties_oAuthProperties,
    sAPODataConnectorProfileProperties_applicationHostUrl,
    sAPODataConnectorProfileProperties_applicationServicePath,
    sAPODataConnectorProfileProperties_portNumber,
    sAPODataConnectorProfileProperties_clientNumber,

    -- * SAPODataMetadata
    SAPODataMetadata (..),
    newSAPODataMetadata,

    -- * SAPODataSourceProperties
    SAPODataSourceProperties (..),
    newSAPODataSourceProperties,
    sAPODataSourceProperties_objectPath,

    -- * SalesforceConnectorProfileCredentials
    SalesforceConnectorProfileCredentials (..),
    newSalesforceConnectorProfileCredentials,
    salesforceConnectorProfileCredentials_accessToken,
    salesforceConnectorProfileCredentials_refreshToken,
    salesforceConnectorProfileCredentials_oAuthRequest,
    salesforceConnectorProfileCredentials_clientCredentialsArn,

    -- * SalesforceConnectorProfileProperties
    SalesforceConnectorProfileProperties (..),
    newSalesforceConnectorProfileProperties,
    salesforceConnectorProfileProperties_instanceUrl,
    salesforceConnectorProfileProperties_isSandboxEnvironment,

    -- * SalesforceDestinationProperties
    SalesforceDestinationProperties (..),
    newSalesforceDestinationProperties,
    salesforceDestinationProperties_writeOperationType,
    salesforceDestinationProperties_idFieldNames,
    salesforceDestinationProperties_errorHandlingConfig,
    salesforceDestinationProperties_object,

    -- * SalesforceMetadata
    SalesforceMetadata (..),
    newSalesforceMetadata,
    salesforceMetadata_oAuthScopes,

    -- * SalesforceSourceProperties
    SalesforceSourceProperties (..),
    newSalesforceSourceProperties,
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_object,

    -- * ScheduledTriggerProperties
    ScheduledTriggerProperties (..),
    newScheduledTriggerProperties,
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_dataPullMode,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_scheduleExpression,

    -- * ServiceNowConnectorProfileCredentials
    ServiceNowConnectorProfileCredentials (..),
    newServiceNowConnectorProfileCredentials,
    serviceNowConnectorProfileCredentials_username,
    serviceNowConnectorProfileCredentials_password,

    -- * ServiceNowConnectorProfileProperties
    ServiceNowConnectorProfileProperties (..),
    newServiceNowConnectorProfileProperties,
    serviceNowConnectorProfileProperties_instanceUrl,

    -- * ServiceNowMetadata
    ServiceNowMetadata (..),
    newServiceNowMetadata,

    -- * ServiceNowSourceProperties
    ServiceNowSourceProperties (..),
    newServiceNowSourceProperties,
    serviceNowSourceProperties_object,

    -- * SingularConnectorProfileCredentials
    SingularConnectorProfileCredentials (..),
    newSingularConnectorProfileCredentials,
    singularConnectorProfileCredentials_apiKey,

    -- * SingularConnectorProfileProperties
    SingularConnectorProfileProperties (..),
    newSingularConnectorProfileProperties,

    -- * SingularMetadata
    SingularMetadata (..),
    newSingularMetadata,

    -- * SingularSourceProperties
    SingularSourceProperties (..),
    newSingularSourceProperties,
    singularSourceProperties_object,

    -- * SlackConnectorProfileCredentials
    SlackConnectorProfileCredentials (..),
    newSlackConnectorProfileCredentials,
    slackConnectorProfileCredentials_accessToken,
    slackConnectorProfileCredentials_oAuthRequest,
    slackConnectorProfileCredentials_clientId,
    slackConnectorProfileCredentials_clientSecret,

    -- * SlackConnectorProfileProperties
    SlackConnectorProfileProperties (..),
    newSlackConnectorProfileProperties,
    slackConnectorProfileProperties_instanceUrl,

    -- * SlackMetadata
    SlackMetadata (..),
    newSlackMetadata,
    slackMetadata_oAuthScopes,

    -- * SlackSourceProperties
    SlackSourceProperties (..),
    newSlackSourceProperties,
    slackSourceProperties_object,

    -- * SnowflakeConnectorProfileCredentials
    SnowflakeConnectorProfileCredentials (..),
    newSnowflakeConnectorProfileCredentials,
    snowflakeConnectorProfileCredentials_username,
    snowflakeConnectorProfileCredentials_password,

    -- * SnowflakeConnectorProfileProperties
    SnowflakeConnectorProfileProperties (..),
    newSnowflakeConnectorProfileProperties,
    snowflakeConnectorProfileProperties_privateLinkServiceName,
    snowflakeConnectorProfileProperties_accountName,
    snowflakeConnectorProfileProperties_bucketPrefix,
    snowflakeConnectorProfileProperties_region,
    snowflakeConnectorProfileProperties_warehouse,
    snowflakeConnectorProfileProperties_stage,
    snowflakeConnectorProfileProperties_bucketName,

    -- * SnowflakeDestinationProperties
    SnowflakeDestinationProperties (..),
    newSnowflakeDestinationProperties,
    snowflakeDestinationProperties_bucketPrefix,
    snowflakeDestinationProperties_errorHandlingConfig,
    snowflakeDestinationProperties_object,
    snowflakeDestinationProperties_intermediateBucketName,

    -- * SnowflakeMetadata
    SnowflakeMetadata (..),
    newSnowflakeMetadata,
    snowflakeMetadata_supportedRegions,

    -- * SourceConnectorProperties
    SourceConnectorProperties (..),
    newSourceConnectorProperties,
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

    -- * SourceFieldProperties
    SourceFieldProperties (..),
    newSourceFieldProperties,
    sourceFieldProperties_isRetrievable,
    sourceFieldProperties_isQueryable,

    -- * SourceFlowConfig
    SourceFlowConfig (..),
    newSourceFlowConfig,
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- * SupportedFieldTypeDetails
    SupportedFieldTypeDetails (..),
    newSupportedFieldTypeDetails,
    supportedFieldTypeDetails_v1,

    -- * Task
    Task (..),
    newTask,
    task_taskProperties,
    task_connectorOperator,
    task_destinationField,
    task_sourceFields,
    task_taskType,

    -- * TrendmicroConnectorProfileCredentials
    TrendmicroConnectorProfileCredentials (..),
    newTrendmicroConnectorProfileCredentials,
    trendmicroConnectorProfileCredentials_apiSecretKey,

    -- * TrendmicroConnectorProfileProperties
    TrendmicroConnectorProfileProperties (..),
    newTrendmicroConnectorProfileProperties,

    -- * TrendmicroMetadata
    TrendmicroMetadata (..),
    newTrendmicroMetadata,

    -- * TrendmicroSourceProperties
    TrendmicroSourceProperties (..),
    newTrendmicroSourceProperties,
    trendmicroSourceProperties_object,

    -- * TriggerConfig
    TriggerConfig (..),
    newTriggerConfig,
    triggerConfig_triggerProperties,
    triggerConfig_triggerType,

    -- * TriggerProperties
    TriggerProperties (..),
    newTriggerProperties,
    triggerProperties_scheduled,

    -- * UpsolverDestinationProperties
    UpsolverDestinationProperties (..),
    newUpsolverDestinationProperties,
    upsolverDestinationProperties_bucketPrefix,
    upsolverDestinationProperties_bucketName,
    upsolverDestinationProperties_s3OutputFormatConfig,

    -- * UpsolverMetadata
    UpsolverMetadata (..),
    newUpsolverMetadata,

    -- * UpsolverS3OutputFormatConfig
    UpsolverS3OutputFormatConfig (..),
    newUpsolverS3OutputFormatConfig,
    upsolverS3OutputFormatConfig_fileType,
    upsolverS3OutputFormatConfig_aggregationConfig,
    upsolverS3OutputFormatConfig_prefixConfig,

    -- * VeevaConnectorProfileCredentials
    VeevaConnectorProfileCredentials (..),
    newVeevaConnectorProfileCredentials,
    veevaConnectorProfileCredentials_username,
    veevaConnectorProfileCredentials_password,

    -- * VeevaConnectorProfileProperties
    VeevaConnectorProfileProperties (..),
    newVeevaConnectorProfileProperties,
    veevaConnectorProfileProperties_instanceUrl,

    -- * VeevaMetadata
    VeevaMetadata (..),
    newVeevaMetadata,

    -- * VeevaSourceProperties
    VeevaSourceProperties (..),
    newVeevaSourceProperties,
    veevaSourceProperties_includeAllVersions,
    veevaSourceProperties_documentType,
    veevaSourceProperties_includeRenditions,
    veevaSourceProperties_includeSourceFiles,
    veevaSourceProperties_object,

    -- * ZendeskConnectorProfileCredentials
    ZendeskConnectorProfileCredentials (..),
    newZendeskConnectorProfileCredentials,
    zendeskConnectorProfileCredentials_accessToken,
    zendeskConnectorProfileCredentials_oAuthRequest,
    zendeskConnectorProfileCredentials_clientId,
    zendeskConnectorProfileCredentials_clientSecret,

    -- * ZendeskConnectorProfileProperties
    ZendeskConnectorProfileProperties (..),
    newZendeskConnectorProfileProperties,
    zendeskConnectorProfileProperties_instanceUrl,

    -- * ZendeskDestinationProperties
    ZendeskDestinationProperties (..),
    newZendeskDestinationProperties,
    zendeskDestinationProperties_writeOperationType,
    zendeskDestinationProperties_idFieldNames,
    zendeskDestinationProperties_errorHandlingConfig,
    zendeskDestinationProperties_object,

    -- * ZendeskMetadata
    ZendeskMetadata (..),
    newZendeskMetadata,
    zendeskMetadata_oAuthScopes,

    -- * ZendeskSourceProperties
    ZendeskSourceProperties (..),
    newZendeskSourceProperties,
    zendeskSourceProperties_object,
  )
where

import Network.AWS.AppFlow.Types.AggregationConfig
import Network.AWS.AppFlow.Types.AggregationType
import Network.AWS.AppFlow.Types.AmplitudeConnectorOperator
import Network.AWS.AppFlow.Types.AmplitudeConnectorProfileCredentials
import Network.AWS.AppFlow.Types.AmplitudeConnectorProfileProperties
import Network.AWS.AppFlow.Types.AmplitudeMetadata
import Network.AWS.AppFlow.Types.AmplitudeSourceProperties
import Network.AWS.AppFlow.Types.BasicAuthCredentials
import Network.AWS.AppFlow.Types.ConnectionMode
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
import Network.AWS.AppFlow.Types.ConnectorType
import Network.AWS.AppFlow.Types.CustomerProfilesDestinationProperties
import Network.AWS.AppFlow.Types.CustomerProfilesMetadata
import Network.AWS.AppFlow.Types.DataPullMode
import Network.AWS.AppFlow.Types.DatadogConnectorOperator
import Network.AWS.AppFlow.Types.DatadogConnectorProfileCredentials
import Network.AWS.AppFlow.Types.DatadogConnectorProfileProperties
import Network.AWS.AppFlow.Types.DatadogMetadata
import Network.AWS.AppFlow.Types.DatadogSourceProperties
import Network.AWS.AppFlow.Types.DestinationConnectorProperties
import Network.AWS.AppFlow.Types.DestinationFieldProperties
import Network.AWS.AppFlow.Types.DestinationFlowConfig
import Network.AWS.AppFlow.Types.DynatraceConnectorOperator
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
import Network.AWS.AppFlow.Types.ExecutionStatus
import Network.AWS.AppFlow.Types.FieldTypeDetails
import Network.AWS.AppFlow.Types.FileType
import Network.AWS.AppFlow.Types.FlowDefinition
import Network.AWS.AppFlow.Types.FlowStatus
import Network.AWS.AppFlow.Types.GoogleAnalyticsConnectorOperator
import Network.AWS.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
import Network.AWS.AppFlow.Types.GoogleAnalyticsConnectorProfileProperties
import Network.AWS.AppFlow.Types.GoogleAnalyticsMetadata
import Network.AWS.AppFlow.Types.GoogleAnalyticsSourceProperties
import Network.AWS.AppFlow.Types.HoneycodeConnectorProfileCredentials
import Network.AWS.AppFlow.Types.HoneycodeConnectorProfileProperties
import Network.AWS.AppFlow.Types.HoneycodeDestinationProperties
import Network.AWS.AppFlow.Types.HoneycodeMetadata
import Network.AWS.AppFlow.Types.IncrementalPullConfig
import Network.AWS.AppFlow.Types.InforNexusConnectorOperator
import Network.AWS.AppFlow.Types.InforNexusConnectorProfileCredentials
import Network.AWS.AppFlow.Types.InforNexusConnectorProfileProperties
import Network.AWS.AppFlow.Types.InforNexusMetadata
import Network.AWS.AppFlow.Types.InforNexusSourceProperties
import Network.AWS.AppFlow.Types.LookoutMetricsDestinationProperties
import Network.AWS.AppFlow.Types.MarketoConnectorOperator
import Network.AWS.AppFlow.Types.MarketoConnectorProfileCredentials
import Network.AWS.AppFlow.Types.MarketoConnectorProfileProperties
import Network.AWS.AppFlow.Types.MarketoMetadata
import Network.AWS.AppFlow.Types.MarketoSourceProperties
import Network.AWS.AppFlow.Types.OAuthCredentials
import Network.AWS.AppFlow.Types.OAuthProperties
import Network.AWS.AppFlow.Types.Operator
import Network.AWS.AppFlow.Types.OperatorPropertiesKeys
import Network.AWS.AppFlow.Types.PrefixConfig
import Network.AWS.AppFlow.Types.PrefixFormat
import Network.AWS.AppFlow.Types.PrefixType
import Network.AWS.AppFlow.Types.PrivateConnectionProvisioningFailureCause
import Network.AWS.AppFlow.Types.PrivateConnectionProvisioningState
import Network.AWS.AppFlow.Types.PrivateConnectionProvisioningStatus
import Network.AWS.AppFlow.Types.RedshiftConnectorProfileCredentials
import Network.AWS.AppFlow.Types.RedshiftConnectorProfileProperties
import Network.AWS.AppFlow.Types.RedshiftDestinationProperties
import Network.AWS.AppFlow.Types.RedshiftMetadata
import Network.AWS.AppFlow.Types.S3ConnectorOperator
import Network.AWS.AppFlow.Types.S3DestinationProperties
import Network.AWS.AppFlow.Types.S3InputFileType
import Network.AWS.AppFlow.Types.S3InputFormatConfig
import Network.AWS.AppFlow.Types.S3Metadata
import Network.AWS.AppFlow.Types.S3OutputFormatConfig
import Network.AWS.AppFlow.Types.S3SourceProperties
import Network.AWS.AppFlow.Types.SAPODataConnectorOperator
import Network.AWS.AppFlow.Types.SAPODataConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SAPODataConnectorProfileProperties
import Network.AWS.AppFlow.Types.SAPODataMetadata
import Network.AWS.AppFlow.Types.SAPODataSourceProperties
import Network.AWS.AppFlow.Types.SalesforceConnectorOperator
import Network.AWS.AppFlow.Types.SalesforceConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SalesforceConnectorProfileProperties
import Network.AWS.AppFlow.Types.SalesforceDestinationProperties
import Network.AWS.AppFlow.Types.SalesforceMetadata
import Network.AWS.AppFlow.Types.SalesforceSourceProperties
import Network.AWS.AppFlow.Types.ScheduleFrequencyType
import Network.AWS.AppFlow.Types.ScheduledTriggerProperties
import Network.AWS.AppFlow.Types.ServiceNowConnectorOperator
import Network.AWS.AppFlow.Types.ServiceNowConnectorProfileCredentials
import Network.AWS.AppFlow.Types.ServiceNowConnectorProfileProperties
import Network.AWS.AppFlow.Types.ServiceNowMetadata
import Network.AWS.AppFlow.Types.ServiceNowSourceProperties
import Network.AWS.AppFlow.Types.SingularConnectorOperator
import Network.AWS.AppFlow.Types.SingularConnectorProfileCredentials
import Network.AWS.AppFlow.Types.SingularConnectorProfileProperties
import Network.AWS.AppFlow.Types.SingularMetadata
import Network.AWS.AppFlow.Types.SingularSourceProperties
import Network.AWS.AppFlow.Types.SlackConnectorOperator
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
import Network.AWS.AppFlow.Types.TaskType
import Network.AWS.AppFlow.Types.TrendmicroConnectorOperator
import Network.AWS.AppFlow.Types.TrendmicroConnectorProfileCredentials
import Network.AWS.AppFlow.Types.TrendmicroConnectorProfileProperties
import Network.AWS.AppFlow.Types.TrendmicroMetadata
import Network.AWS.AppFlow.Types.TrendmicroSourceProperties
import Network.AWS.AppFlow.Types.TriggerConfig
import Network.AWS.AppFlow.Types.TriggerProperties
import Network.AWS.AppFlow.Types.TriggerType
import Network.AWS.AppFlow.Types.UpsolverDestinationProperties
import Network.AWS.AppFlow.Types.UpsolverMetadata
import Network.AWS.AppFlow.Types.UpsolverS3OutputFormatConfig
import Network.AWS.AppFlow.Types.VeevaConnectorOperator
import Network.AWS.AppFlow.Types.VeevaConnectorProfileCredentials
import Network.AWS.AppFlow.Types.VeevaConnectorProfileProperties
import Network.AWS.AppFlow.Types.VeevaMetadata
import Network.AWS.AppFlow.Types.VeevaSourceProperties
import Network.AWS.AppFlow.Types.WriteOperationType
import Network.AWS.AppFlow.Types.ZendeskConnectorOperator
import Network.AWS.AppFlow.Types.ZendeskConnectorProfileCredentials
import Network.AWS.AppFlow.Types.ZendeskConnectorProfileProperties
import Network.AWS.AppFlow.Types.ZendeskDestinationProperties
import Network.AWS.AppFlow.Types.ZendeskMetadata
import Network.AWS.AppFlow.Types.ZendeskSourceProperties
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-08-23@ of the Amazon Appflow SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AppFlow",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "appflow",
      Core._serviceSigningName = "appflow",
      Core._serviceVersion = "2020-08-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "AppFlow",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The request has invalid or missing parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The requested operation is not supported for the current flow.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
    Prelude.. Core.hasStatus 400

-- | There was a conflict when processing the request (for example, a flow
-- with the given name already exists within the account. Check for
-- conflicting resource names and try again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An error occurred when authenticating with the connector endpoint.
_ConnectorAuthenticationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConnectorAuthenticationException =
  Core._MatchServiceError
    defaultService
    "ConnectorAuthenticationException"
    Prelude.. Core.hasStatus 401

-- | The request would cause a service quota (such as the number of flows) to
-- be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | An internal service error occurred during the processing of your
-- request. Try again later.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource specified in the request (such as the source or destination
-- connector profile) is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | An error occurred when retrieving data from the connector endpoint.
_ConnectorServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConnectorServerException =
  Core._MatchServiceError
    defaultService
    "ConnectorServerException"
    Prelude.. Core.hasStatus 400
