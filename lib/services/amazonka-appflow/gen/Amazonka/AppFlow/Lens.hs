{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Lens
  ( -- * Operations

    -- ** CreateConnectorProfile
    createConnectorProfile_kmsArn,
    createConnectorProfile_connectorLabel,
    createConnectorProfile_connectorProfileName,
    createConnectorProfile_connectorType,
    createConnectorProfile_connectionMode,
    createConnectorProfile_connectorProfileConfig,
    createConnectorProfileResponse_connectorProfileArn,
    createConnectorProfileResponse_httpStatus,

    -- ** CreateFlow
    createFlow_tags,
    createFlow_kmsArn,
    createFlow_metadataCatalogConfig,
    createFlow_description,
    createFlow_flowName,
    createFlow_triggerConfig,
    createFlow_sourceFlowConfig,
    createFlow_destinationFlowConfigList,
    createFlow_tasks,
    createFlowResponse_flowStatus,
    createFlowResponse_flowArn,
    createFlowResponse_httpStatus,

    -- ** DeleteConnectorProfile
    deleteConnectorProfile_forceDelete,
    deleteConnectorProfile_connectorProfileName,
    deleteConnectorProfileResponse_httpStatus,

    -- ** DeleteFlow
    deleteFlow_forceDelete,
    deleteFlow_flowName,
    deleteFlowResponse_httpStatus,

    -- ** DescribeConnector
    describeConnector_connectorLabel,
    describeConnector_connectorType,
    describeConnectorResponse_connectorConfiguration,
    describeConnectorResponse_httpStatus,

    -- ** DescribeConnectorEntity
    describeConnectorEntity_apiVersion,
    describeConnectorEntity_connectorProfileName,
    describeConnectorEntity_connectorType,
    describeConnectorEntity_connectorEntityName,
    describeConnectorEntityResponse_httpStatus,
    describeConnectorEntityResponse_connectorEntityFields,

    -- ** DescribeConnectorProfiles
    describeConnectorProfiles_nextToken,
    describeConnectorProfiles_connectorType,
    describeConnectorProfiles_maxResults,
    describeConnectorProfiles_connectorProfileNames,
    describeConnectorProfiles_connectorLabel,
    describeConnectorProfilesResponse_nextToken,
    describeConnectorProfilesResponse_connectorProfileDetails,
    describeConnectorProfilesResponse_httpStatus,

    -- ** DescribeConnectors
    describeConnectors_nextToken,
    describeConnectors_connectorTypes,
    describeConnectors_maxResults,
    describeConnectorsResponse_nextToken,
    describeConnectorsResponse_connectors,
    describeConnectorsResponse_connectorConfigurations,
    describeConnectorsResponse_httpStatus,

    -- ** DescribeFlow
    describeFlow_flowName,
    describeFlowResponse_tags,
    describeFlowResponse_sourceFlowConfig,
    describeFlowResponse_tasks,
    describeFlowResponse_kmsArn,
    describeFlowResponse_lastUpdatedAt,
    describeFlowResponse_metadataCatalogConfig,
    describeFlowResponse_flowName,
    describeFlowResponse_destinationFlowConfigList,
    describeFlowResponse_flowStatusMessage,
    describeFlowResponse_description,
    describeFlowResponse_lastRunExecutionDetails,
    describeFlowResponse_lastRunMetadataCatalogDetails,
    describeFlowResponse_flowStatus,
    describeFlowResponse_schemaVersion,
    describeFlowResponse_createdBy,
    describeFlowResponse_triggerConfig,
    describeFlowResponse_flowArn,
    describeFlowResponse_createdAt,
    describeFlowResponse_lastUpdatedBy,
    describeFlowResponse_httpStatus,

    -- ** DescribeFlowExecutionRecords
    describeFlowExecutionRecords_nextToken,
    describeFlowExecutionRecords_maxResults,
    describeFlowExecutionRecords_flowName,
    describeFlowExecutionRecordsResponse_flowExecutions,
    describeFlowExecutionRecordsResponse_nextToken,
    describeFlowExecutionRecordsResponse_httpStatus,

    -- ** ListConnectorEntities
    listConnectorEntities_entitiesPath,
    listConnectorEntities_apiVersion,
    listConnectorEntities_connectorProfileName,
    listConnectorEntities_connectorType,
    listConnectorEntitiesResponse_httpStatus,
    listConnectorEntitiesResponse_connectorEntityMap,

    -- ** ListConnectors
    listConnectors_nextToken,
    listConnectors_maxResults,
    listConnectorsResponse_nextToken,
    listConnectorsResponse_connectors,
    listConnectorsResponse_httpStatus,

    -- ** ListFlows
    listFlows_nextToken,
    listFlows_maxResults,
    listFlowsResponse_nextToken,
    listFlowsResponse_flows,
    listFlowsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RegisterConnector
    registerConnector_connectorProvisioningType,
    registerConnector_description,
    registerConnector_connectorProvisioningConfig,
    registerConnector_connectorLabel,
    registerConnectorResponse_connectorArn,
    registerConnectorResponse_httpStatus,

    -- ** StartFlow
    startFlow_flowName,
    startFlowResponse_executionId,
    startFlowResponse_flowStatus,
    startFlowResponse_flowArn,
    startFlowResponse_httpStatus,

    -- ** StopFlow
    stopFlow_flowName,
    stopFlowResponse_flowStatus,
    stopFlowResponse_flowArn,
    stopFlowResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UnregisterConnector
    unregisterConnector_forceDelete,
    unregisterConnector_connectorLabel,
    unregisterConnectorResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConnectorProfile
    updateConnectorProfile_connectorProfileName,
    updateConnectorProfile_connectionMode,
    updateConnectorProfile_connectorProfileConfig,
    updateConnectorProfileResponse_connectorProfileArn,
    updateConnectorProfileResponse_httpStatus,

    -- ** UpdateConnectorRegistration
    updateConnectorRegistration_description,
    updateConnectorRegistration_connectorProvisioningConfig,
    updateConnectorRegistration_connectorLabel,
    updateConnectorRegistrationResponse_connectorArn,
    updateConnectorRegistrationResponse_httpStatus,

    -- ** UpdateFlow
    updateFlow_metadataCatalogConfig,
    updateFlow_description,
    updateFlow_flowName,
    updateFlow_triggerConfig,
    updateFlow_sourceFlowConfig,
    updateFlow_destinationFlowConfigList,
    updateFlow_tasks,
    updateFlowResponse_flowStatus,
    updateFlowResponse_httpStatus,

    -- * Types

    -- ** AggregationConfig
    aggregationConfig_aggregationType,
    aggregationConfig_targetFileSize,

    -- ** AmplitudeConnectorProfileCredentials
    amplitudeConnectorProfileCredentials_apiKey,
    amplitudeConnectorProfileCredentials_secretKey,

    -- ** AmplitudeConnectorProfileProperties

    -- ** AmplitudeMetadata

    -- ** AmplitudeSourceProperties
    amplitudeSourceProperties_object,

    -- ** ApiKeyCredentials
    apiKeyCredentials_apiSecretKey,
    apiKeyCredentials_apiKey,

    -- ** AuthParameter
    authParameter_key,
    authParameter_connectorSuppliedValues,
    authParameter_label,
    authParameter_description,
    authParameter_isSensitiveField,
    authParameter_isRequired,

    -- ** AuthenticationConfig
    authenticationConfig_customAuthConfigs,
    authenticationConfig_isCustomAuthSupported,
    authenticationConfig_isApiKeyAuthSupported,
    authenticationConfig_isBasicAuthSupported,
    authenticationConfig_isOAuth2Supported,
    authenticationConfig_oAuth2Defaults,

    -- ** BasicAuthCredentials
    basicAuthCredentials_username,
    basicAuthCredentials_password,

    -- ** ConnectorConfiguration
    connectorConfiguration_connectorProvisioningType,
    connectorConfiguration_canUseAsSource,
    connectorConfiguration_connectorDescription,
    connectorConfiguration_registeredBy,
    connectorConfiguration_supportedWriteOperations,
    connectorConfiguration_logoURL,
    connectorConfiguration_isPrivateLinkEnabled,
    connectorConfiguration_connectorVersion,
    connectorConfiguration_connectorArn,
    connectorConfiguration_connectorRuntimeSettings,
    connectorConfiguration_connectorType,
    connectorConfiguration_canUseAsDestination,
    connectorConfiguration_connectorProvisioningConfig,
    connectorConfiguration_supportedOperators,
    connectorConfiguration_connectorName,
    connectorConfiguration_connectorModes,
    connectorConfiguration_supportedTriggerTypes,
    connectorConfiguration_authenticationConfig,
    connectorConfiguration_isPrivateLinkEndpointUrlRequired,
    connectorConfiguration_connectorLabel,
    connectorConfiguration_supportedApiVersions,
    connectorConfiguration_supportedDestinationConnectors,
    connectorConfiguration_connectorOwner,
    connectorConfiguration_connectorMetadata,
    connectorConfiguration_registeredAt,
    connectorConfiguration_supportedSchedulingFrequencies,

    -- ** ConnectorDetail
    connectorDetail_connectorProvisioningType,
    connectorDetail_connectorDescription,
    connectorDetail_registeredBy,
    connectorDetail_connectorVersion,
    connectorDetail_connectorType,
    connectorDetail_connectorName,
    connectorDetail_applicationType,
    connectorDetail_connectorModes,
    connectorDetail_connectorLabel,
    connectorDetail_connectorOwner,
    connectorDetail_registeredAt,

    -- ** ConnectorEntity
    connectorEntity_label,
    connectorEntity_hasNestedEntities,
    connectorEntity_name,

    -- ** ConnectorEntityField
    connectorEntityField_customProperties,
    connectorEntityField_parentIdentifier,
    connectorEntityField_label,
    connectorEntityField_defaultValue,
    connectorEntityField_description,
    connectorEntityField_destinationProperties,
    connectorEntityField_supportedFieldTypeDetails,
    connectorEntityField_isPrimaryKey,
    connectorEntityField_sourceProperties,
    connectorEntityField_isDeprecated,
    connectorEntityField_identifier,

    -- ** ConnectorMetadata
    connectorMetadata_zendesk,
    connectorMetadata_slack,
    connectorMetadata_singular,
    connectorMetadata_s3,
    connectorMetadata_veeva,
    connectorMetadata_honeycode,
    connectorMetadata_salesforce,
    connectorMetadata_snowflake,
    connectorMetadata_sAPOData,
    connectorMetadata_marketo,
    connectorMetadata_redshift,
    connectorMetadata_trendmicro,
    connectorMetadata_inforNexus,
    connectorMetadata_customerProfiles,
    connectorMetadata_upsolver,
    connectorMetadata_serviceNow,
    connectorMetadata_datadog,
    connectorMetadata_amplitude,
    connectorMetadata_dynatrace,
    connectorMetadata_googleAnalytics,
    connectorMetadata_eventBridge,

    -- ** ConnectorOAuthRequest
    connectorOAuthRequest_authCode,
    connectorOAuthRequest_redirectUri,

    -- ** ConnectorOperator
    connectorOperator_zendesk,
    connectorOperator_slack,
    connectorOperator_singular,
    connectorOperator_s3,
    connectorOperator_veeva,
    connectorOperator_salesforce,
    connectorOperator_sAPOData,
    connectorOperator_marketo,
    connectorOperator_trendmicro,
    connectorOperator_inforNexus,
    connectorOperator_serviceNow,
    connectorOperator_datadog,
    connectorOperator_customConnector,
    connectorOperator_amplitude,
    connectorOperator_dynatrace,
    connectorOperator_googleAnalytics,

    -- ** ConnectorProfile
    connectorProfile_credentialsArn,
    connectorProfile_connectorProfileProperties,
    connectorProfile_lastUpdatedAt,
    connectorProfile_connectionMode,
    connectorProfile_connectorProfileName,
    connectorProfile_privateConnectionProvisioningState,
    connectorProfile_connectorType,
    connectorProfile_connectorProfileArn,
    connectorProfile_connectorLabel,
    connectorProfile_createdAt,

    -- ** ConnectorProfileConfig
    connectorProfileConfig_connectorProfileCredentials,
    connectorProfileConfig_connectorProfileProperties,

    -- ** ConnectorProfileCredentials
    connectorProfileCredentials_zendesk,
    connectorProfileCredentials_slack,
    connectorProfileCredentials_singular,
    connectorProfileCredentials_veeva,
    connectorProfileCredentials_honeycode,
    connectorProfileCredentials_salesforce,
    connectorProfileCredentials_snowflake,
    connectorProfileCredentials_sAPOData,
    connectorProfileCredentials_marketo,
    connectorProfileCredentials_redshift,
    connectorProfileCredentials_trendmicro,
    connectorProfileCredentials_inforNexus,
    connectorProfileCredentials_serviceNow,
    connectorProfileCredentials_datadog,
    connectorProfileCredentials_customConnector,
    connectorProfileCredentials_amplitude,
    connectorProfileCredentials_dynatrace,
    connectorProfileCredentials_googleAnalytics,

    -- ** ConnectorProfileProperties
    connectorProfileProperties_zendesk,
    connectorProfileProperties_slack,
    connectorProfileProperties_singular,
    connectorProfileProperties_veeva,
    connectorProfileProperties_honeycode,
    connectorProfileProperties_salesforce,
    connectorProfileProperties_snowflake,
    connectorProfileProperties_sAPOData,
    connectorProfileProperties_marketo,
    connectorProfileProperties_redshift,
    connectorProfileProperties_trendmicro,
    connectorProfileProperties_inforNexus,
    connectorProfileProperties_serviceNow,
    connectorProfileProperties_datadog,
    connectorProfileProperties_customConnector,
    connectorProfileProperties_amplitude,
    connectorProfileProperties_dynatrace,
    connectorProfileProperties_googleAnalytics,

    -- ** ConnectorProvisioningConfig
    connectorProvisioningConfig_lambda,

    -- ** ConnectorRuntimeSetting
    connectorRuntimeSetting_key,
    connectorRuntimeSetting_label,
    connectorRuntimeSetting_connectorSuppliedValueOptions,
    connectorRuntimeSetting_description,
    connectorRuntimeSetting_scope,
    connectorRuntimeSetting_isRequired,
    connectorRuntimeSetting_dataType,

    -- ** CustomAuthConfig
    customAuthConfig_authParameters,
    customAuthConfig_customAuthenticationType,

    -- ** CustomAuthCredentials
    customAuthCredentials_credentialsMap,
    customAuthCredentials_customAuthenticationType,

    -- ** CustomConnectorDestinationProperties
    customConnectorDestinationProperties_errorHandlingConfig,
    customConnectorDestinationProperties_customProperties,
    customConnectorDestinationProperties_idFieldNames,
    customConnectorDestinationProperties_writeOperationType,
    customConnectorDestinationProperties_entityName,

    -- ** CustomConnectorProfileCredentials
    customConnectorProfileCredentials_apiKey,
    customConnectorProfileCredentials_oauth2,
    customConnectorProfileCredentials_custom,
    customConnectorProfileCredentials_basic,
    customConnectorProfileCredentials_authenticationType,

    -- ** CustomConnectorProfileProperties
    customConnectorProfileProperties_profileProperties,
    customConnectorProfileProperties_oAuth2Properties,

    -- ** CustomConnectorSourceProperties
    customConnectorSourceProperties_customProperties,
    customConnectorSourceProperties_entityName,

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
    destinationConnectorProperties_zendesk,
    destinationConnectorProperties_s3,
    destinationConnectorProperties_honeycode,
    destinationConnectorProperties_salesforce,
    destinationConnectorProperties_snowflake,
    destinationConnectorProperties_sAPOData,
    destinationConnectorProperties_marketo,
    destinationConnectorProperties_redshift,
    destinationConnectorProperties_customerProfiles,
    destinationConnectorProperties_upsolver,
    destinationConnectorProperties_customConnector,
    destinationConnectorProperties_eventBridge,
    destinationConnectorProperties_lookoutMetrics,

    -- ** DestinationFieldProperties
    destinationFieldProperties_isUpdatable,
    destinationFieldProperties_supportedWriteOperations,
    destinationFieldProperties_isNullable,
    destinationFieldProperties_isDefaultedOnCreate,
    destinationFieldProperties_isUpsertable,
    destinationFieldProperties_isCreatable,

    -- ** DestinationFlowConfig
    destinationFlowConfig_apiVersion,
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
    errorHandlingConfig_bucketPrefix,
    errorHandlingConfig_bucketName,
    errorHandlingConfig_failOnFirstDestinationError,

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
    executionRecord_lastUpdatedAt,
    executionRecord_metadataCatalogDetails,
    executionRecord_dataPullStartTime,
    executionRecord_executionId,
    executionRecord_startedAt,
    executionRecord_executionResult,
    executionRecord_executionStatus,
    executionRecord_dataPullEndTime,

    -- ** ExecutionResult
    executionResult_recordsProcessed,
    executionResult_bytesProcessed,
    executionResult_errorInfo,
    executionResult_bytesWritten,

    -- ** FieldTypeDetails
    fieldTypeDetails_fieldValueRange,
    fieldTypeDetails_fieldLengthRange,
    fieldTypeDetails_supportedValues,
    fieldTypeDetails_valueRegexPattern,
    fieldTypeDetails_supportedDateFormat,
    fieldTypeDetails_fieldType,
    fieldTypeDetails_filterOperators,

    -- ** FlowDefinition
    flowDefinition_tags,
    flowDefinition_destinationConnectorLabel,
    flowDefinition_lastUpdatedAt,
    flowDefinition_flowName,
    flowDefinition_sourceConnectorLabel,
    flowDefinition_description,
    flowDefinition_sourceConnectorType,
    flowDefinition_lastRunExecutionDetails,
    flowDefinition_flowStatus,
    flowDefinition_triggerType,
    flowDefinition_destinationConnectorType,
    flowDefinition_createdBy,
    flowDefinition_flowArn,
    flowDefinition_createdAt,
    flowDefinition_lastUpdatedBy,

    -- ** GlueDataCatalogConfig
    glueDataCatalogConfig_roleArn,
    glueDataCatalogConfig_databaseName,
    glueDataCatalogConfig_tablePrefix,

    -- ** GoogleAnalyticsConnectorProfileCredentials
    googleAnalyticsConnectorProfileCredentials_accessToken,
    googleAnalyticsConnectorProfileCredentials_oAuthRequest,
    googleAnalyticsConnectorProfileCredentials_refreshToken,
    googleAnalyticsConnectorProfileCredentials_clientId,
    googleAnalyticsConnectorProfileCredentials_clientSecret,

    -- ** GoogleAnalyticsConnectorProfileProperties

    -- ** GoogleAnalyticsMetadata
    googleAnalyticsMetadata_oAuthScopes,

    -- ** GoogleAnalyticsSourceProperties
    googleAnalyticsSourceProperties_object,

    -- ** HoneycodeConnectorProfileCredentials
    honeycodeConnectorProfileCredentials_accessToken,
    honeycodeConnectorProfileCredentials_oAuthRequest,
    honeycodeConnectorProfileCredentials_refreshToken,

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

    -- ** LambdaConnectorProvisioningConfig
    lambdaConnectorProvisioningConfig_lambdaArn,

    -- ** LookoutMetricsDestinationProperties

    -- ** MarketoConnectorProfileCredentials
    marketoConnectorProfileCredentials_accessToken,
    marketoConnectorProfileCredentials_oAuthRequest,
    marketoConnectorProfileCredentials_clientId,
    marketoConnectorProfileCredentials_clientSecret,

    -- ** MarketoConnectorProfileProperties
    marketoConnectorProfileProperties_instanceUrl,

    -- ** MarketoDestinationProperties
    marketoDestinationProperties_errorHandlingConfig,
    marketoDestinationProperties_object,

    -- ** MarketoMetadata

    -- ** MarketoSourceProperties
    marketoSourceProperties_object,

    -- ** MetadataCatalogConfig
    metadataCatalogConfig_glueDataCatalog,

    -- ** MetadataCatalogDetail
    metadataCatalogDetail_tableName,
    metadataCatalogDetail_tableRegistrationOutput,
    metadataCatalogDetail_partitionRegistrationOutput,
    metadataCatalogDetail_catalogType,

    -- ** OAuth2Credentials
    oAuth2Credentials_accessToken,
    oAuth2Credentials_clientSecret,
    oAuth2Credentials_clientId,
    oAuth2Credentials_oAuthRequest,
    oAuth2Credentials_refreshToken,

    -- ** OAuth2CustomParameter
    oAuth2CustomParameter_key,
    oAuth2CustomParameter_type,
    oAuth2CustomParameter_connectorSuppliedValues,
    oAuth2CustomParameter_label,
    oAuth2CustomParameter_description,
    oAuth2CustomParameter_isSensitiveField,
    oAuth2CustomParameter_isRequired,

    -- ** OAuth2Defaults
    oAuth2Defaults_tokenUrls,
    oAuth2Defaults_authCodeUrls,
    oAuth2Defaults_oauth2CustomProperties,
    oAuth2Defaults_oauth2GrantTypesSupported,
    oAuth2Defaults_oauthScopes,

    -- ** OAuth2Properties
    oAuth2Properties_tokenUrlCustomProperties,
    oAuth2Properties_tokenUrl,
    oAuth2Properties_oAuth2GrantType,

    -- ** OAuthCredentials
    oAuthCredentials_accessToken,
    oAuthCredentials_oAuthRequest,
    oAuthCredentials_refreshToken,
    oAuthCredentials_clientId,
    oAuthCredentials_clientSecret,

    -- ** OAuthProperties
    oAuthProperties_tokenUrl,
    oAuthProperties_authCodeUrl,
    oAuthProperties_oAuthScopes,

    -- ** PrefixConfig
    prefixConfig_prefixType,
    prefixConfig_prefixFormat,
    prefixConfig_pathPrefixHierarchy,

    -- ** PrivateConnectionProvisioningState
    privateConnectionProvisioningState_status,
    privateConnectionProvisioningState_failureMessage,
    privateConnectionProvisioningState_failureCause,

    -- ** Range
    range_minimum,
    range_maximum,

    -- ** RedshiftConnectorProfileCredentials
    redshiftConnectorProfileCredentials_password,
    redshiftConnectorProfileCredentials_username,

    -- ** RedshiftConnectorProfileProperties
    redshiftConnectorProfileProperties_clusterIdentifier,
    redshiftConnectorProfileProperties_isRedshiftServerless,
    redshiftConnectorProfileProperties_databaseUrl,
    redshiftConnectorProfileProperties_databaseName,
    redshiftConnectorProfileProperties_workgroupName,
    redshiftConnectorProfileProperties_bucketPrefix,
    redshiftConnectorProfileProperties_dataApiRoleArn,
    redshiftConnectorProfileProperties_bucketName,
    redshiftConnectorProfileProperties_roleArn,

    -- ** RedshiftDestinationProperties
    redshiftDestinationProperties_errorHandlingConfig,
    redshiftDestinationProperties_bucketPrefix,
    redshiftDestinationProperties_object,
    redshiftDestinationProperties_intermediateBucketName,

    -- ** RedshiftMetadata

    -- ** RegistrationOutput
    registrationOutput_message,
    registrationOutput_status,
    registrationOutput_result,

    -- ** S3DestinationProperties
    s3DestinationProperties_bucketPrefix,
    s3DestinationProperties_s3OutputFormatConfig,
    s3DestinationProperties_bucketName,

    -- ** S3InputFormatConfig
    s3InputFormatConfig_s3InputFileType,

    -- ** S3Metadata

    -- ** S3OutputFormatConfig
    s3OutputFormatConfig_prefixConfig,
    s3OutputFormatConfig_aggregationConfig,
    s3OutputFormatConfig_preserveSourceDataTyping,
    s3OutputFormatConfig_fileType,

    -- ** S3SourceProperties
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_s3InputFormatConfig,
    s3SourceProperties_bucketName,

    -- ** SAPODataConnectorProfileCredentials
    sAPODataConnectorProfileCredentials_basicAuthCredentials,
    sAPODataConnectorProfileCredentials_oAuthCredentials,

    -- ** SAPODataConnectorProfileProperties
    sAPODataConnectorProfileProperties_oAuthProperties,
    sAPODataConnectorProfileProperties_logonLanguage,
    sAPODataConnectorProfileProperties_privateLinkServiceName,
    sAPODataConnectorProfileProperties_applicationHostUrl,
    sAPODataConnectorProfileProperties_applicationServicePath,
    sAPODataConnectorProfileProperties_portNumber,
    sAPODataConnectorProfileProperties_clientNumber,

    -- ** SAPODataDestinationProperties
    sAPODataDestinationProperties_errorHandlingConfig,
    sAPODataDestinationProperties_successResponseHandlingConfig,
    sAPODataDestinationProperties_idFieldNames,
    sAPODataDestinationProperties_writeOperationType,
    sAPODataDestinationProperties_objectPath,

    -- ** SAPODataMetadata

    -- ** SAPODataSourceProperties
    sAPODataSourceProperties_objectPath,

    -- ** SalesforceConnectorProfileCredentials
    salesforceConnectorProfileCredentials_accessToken,
    salesforceConnectorProfileCredentials_clientCredentialsArn,
    salesforceConnectorProfileCredentials_oAuthRequest,
    salesforceConnectorProfileCredentials_refreshToken,

    -- ** SalesforceConnectorProfileProperties
    salesforceConnectorProfileProperties_isSandboxEnvironment,
    salesforceConnectorProfileProperties_instanceUrl,

    -- ** SalesforceDestinationProperties
    salesforceDestinationProperties_errorHandlingConfig,
    salesforceDestinationProperties_dataTransferApi,
    salesforceDestinationProperties_idFieldNames,
    salesforceDestinationProperties_writeOperationType,
    salesforceDestinationProperties_object,

    -- ** SalesforceMetadata
    salesforceMetadata_dataTransferApis,
    salesforceMetadata_oAuthScopes,

    -- ** SalesforceSourceProperties
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_dataTransferApi,
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_object,

    -- ** ScheduledTriggerProperties
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_flowErrorDeactivationThreshold,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_dataPullMode,
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
    snowflakeConnectorProfileProperties_bucketPrefix,
    snowflakeConnectorProfileProperties_region,
    snowflakeConnectorProfileProperties_accountName,
    snowflakeConnectorProfileProperties_privateLinkServiceName,
    snowflakeConnectorProfileProperties_warehouse,
    snowflakeConnectorProfileProperties_stage,
    snowflakeConnectorProfileProperties_bucketName,

    -- ** SnowflakeDestinationProperties
    snowflakeDestinationProperties_errorHandlingConfig,
    snowflakeDestinationProperties_bucketPrefix,
    snowflakeDestinationProperties_object,
    snowflakeDestinationProperties_intermediateBucketName,

    -- ** SnowflakeMetadata
    snowflakeMetadata_supportedRegions,

    -- ** SourceConnectorProperties
    sourceConnectorProperties_zendesk,
    sourceConnectorProperties_slack,
    sourceConnectorProperties_singular,
    sourceConnectorProperties_s3,
    sourceConnectorProperties_veeva,
    sourceConnectorProperties_salesforce,
    sourceConnectorProperties_sAPOData,
    sourceConnectorProperties_marketo,
    sourceConnectorProperties_trendmicro,
    sourceConnectorProperties_inforNexus,
    sourceConnectorProperties_serviceNow,
    sourceConnectorProperties_datadog,
    sourceConnectorProperties_customConnector,
    sourceConnectorProperties_amplitude,
    sourceConnectorProperties_dynatrace,
    sourceConnectorProperties_googleAnalytics,

    -- ** SourceFieldProperties
    sourceFieldProperties_isRetrievable,
    sourceFieldProperties_isTimestampFieldForIncrementalQueries,
    sourceFieldProperties_isQueryable,

    -- ** SourceFlowConfig
    sourceFlowConfig_apiVersion,
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- ** SuccessResponseHandlingConfig
    successResponseHandlingConfig_bucketPrefix,
    successResponseHandlingConfig_bucketName,

    -- ** SupportedFieldTypeDetails
    supportedFieldTypeDetails_v1,

    -- ** Task
    task_connectorOperator,
    task_taskProperties,
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
    upsolverS3OutputFormatConfig_aggregationConfig,
    upsolverS3OutputFormatConfig_fileType,
    upsolverS3OutputFormatConfig_prefixConfig,

    -- ** VeevaConnectorProfileCredentials
    veevaConnectorProfileCredentials_username,
    veevaConnectorProfileCredentials_password,

    -- ** VeevaConnectorProfileProperties
    veevaConnectorProfileProperties_instanceUrl,

    -- ** VeevaMetadata

    -- ** VeevaSourceProperties
    veevaSourceProperties_documentType,
    veevaSourceProperties_includeSourceFiles,
    veevaSourceProperties_includeRenditions,
    veevaSourceProperties_includeAllVersions,
    veevaSourceProperties_object,

    -- ** ZendeskConnectorProfileCredentials
    zendeskConnectorProfileCredentials_accessToken,
    zendeskConnectorProfileCredentials_oAuthRequest,
    zendeskConnectorProfileCredentials_clientId,
    zendeskConnectorProfileCredentials_clientSecret,

    -- ** ZendeskConnectorProfileProperties
    zendeskConnectorProfileProperties_instanceUrl,

    -- ** ZendeskDestinationProperties
    zendeskDestinationProperties_errorHandlingConfig,
    zendeskDestinationProperties_idFieldNames,
    zendeskDestinationProperties_writeOperationType,
    zendeskDestinationProperties_object,

    -- ** ZendeskMetadata
    zendeskMetadata_oAuthScopes,

    -- ** ZendeskSourceProperties
    zendeskSourceProperties_object,
  )
where

import Amazonka.AppFlow.CreateConnectorProfile
import Amazonka.AppFlow.CreateFlow
import Amazonka.AppFlow.DeleteConnectorProfile
import Amazonka.AppFlow.DeleteFlow
import Amazonka.AppFlow.DescribeConnector
import Amazonka.AppFlow.DescribeConnectorEntity
import Amazonka.AppFlow.DescribeConnectorProfiles
import Amazonka.AppFlow.DescribeConnectors
import Amazonka.AppFlow.DescribeFlow
import Amazonka.AppFlow.DescribeFlowExecutionRecords
import Amazonka.AppFlow.ListConnectorEntities
import Amazonka.AppFlow.ListConnectors
import Amazonka.AppFlow.ListFlows
import Amazonka.AppFlow.ListTagsForResource
import Amazonka.AppFlow.RegisterConnector
import Amazonka.AppFlow.StartFlow
import Amazonka.AppFlow.StopFlow
import Amazonka.AppFlow.TagResource
import Amazonka.AppFlow.Types.AggregationConfig
import Amazonka.AppFlow.Types.AmplitudeConnectorProfileCredentials
import Amazonka.AppFlow.Types.AmplitudeConnectorProfileProperties
import Amazonka.AppFlow.Types.AmplitudeMetadata
import Amazonka.AppFlow.Types.AmplitudeSourceProperties
import Amazonka.AppFlow.Types.ApiKeyCredentials
import Amazonka.AppFlow.Types.AuthParameter
import Amazonka.AppFlow.Types.AuthenticationConfig
import Amazonka.AppFlow.Types.BasicAuthCredentials
import Amazonka.AppFlow.Types.ConnectorConfiguration
import Amazonka.AppFlow.Types.ConnectorDetail
import Amazonka.AppFlow.Types.ConnectorEntity
import Amazonka.AppFlow.Types.ConnectorEntityField
import Amazonka.AppFlow.Types.ConnectorMetadata
import Amazonka.AppFlow.Types.ConnectorOAuthRequest
import Amazonka.AppFlow.Types.ConnectorOperator
import Amazonka.AppFlow.Types.ConnectorProfile
import Amazonka.AppFlow.Types.ConnectorProfileConfig
import Amazonka.AppFlow.Types.ConnectorProfileCredentials
import Amazonka.AppFlow.Types.ConnectorProfileProperties
import Amazonka.AppFlow.Types.ConnectorProvisioningConfig
import Amazonka.AppFlow.Types.ConnectorRuntimeSetting
import Amazonka.AppFlow.Types.CustomAuthConfig
import Amazonka.AppFlow.Types.CustomAuthCredentials
import Amazonka.AppFlow.Types.CustomConnectorDestinationProperties
import Amazonka.AppFlow.Types.CustomConnectorProfileCredentials
import Amazonka.AppFlow.Types.CustomConnectorProfileProperties
import Amazonka.AppFlow.Types.CustomConnectorSourceProperties
import Amazonka.AppFlow.Types.CustomerProfilesDestinationProperties
import Amazonka.AppFlow.Types.CustomerProfilesMetadata
import Amazonka.AppFlow.Types.DatadogConnectorProfileCredentials
import Amazonka.AppFlow.Types.DatadogConnectorProfileProperties
import Amazonka.AppFlow.Types.DatadogMetadata
import Amazonka.AppFlow.Types.DatadogSourceProperties
import Amazonka.AppFlow.Types.DestinationConnectorProperties
import Amazonka.AppFlow.Types.DestinationFieldProperties
import Amazonka.AppFlow.Types.DestinationFlowConfig
import Amazonka.AppFlow.Types.DynatraceConnectorProfileCredentials
import Amazonka.AppFlow.Types.DynatraceConnectorProfileProperties
import Amazonka.AppFlow.Types.DynatraceMetadata
import Amazonka.AppFlow.Types.DynatraceSourceProperties
import Amazonka.AppFlow.Types.ErrorHandlingConfig
import Amazonka.AppFlow.Types.ErrorInfo
import Amazonka.AppFlow.Types.EventBridgeDestinationProperties
import Amazonka.AppFlow.Types.EventBridgeMetadata
import Amazonka.AppFlow.Types.ExecutionDetails
import Amazonka.AppFlow.Types.ExecutionRecord
import Amazonka.AppFlow.Types.ExecutionResult
import Amazonka.AppFlow.Types.FieldTypeDetails
import Amazonka.AppFlow.Types.FlowDefinition
import Amazonka.AppFlow.Types.GlueDataCatalogConfig
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileProperties
import Amazonka.AppFlow.Types.GoogleAnalyticsMetadata
import Amazonka.AppFlow.Types.GoogleAnalyticsSourceProperties
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileCredentials
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileProperties
import Amazonka.AppFlow.Types.HoneycodeDestinationProperties
import Amazonka.AppFlow.Types.HoneycodeMetadata
import Amazonka.AppFlow.Types.IncrementalPullConfig
import Amazonka.AppFlow.Types.InforNexusConnectorProfileCredentials
import Amazonka.AppFlow.Types.InforNexusConnectorProfileProperties
import Amazonka.AppFlow.Types.InforNexusMetadata
import Amazonka.AppFlow.Types.InforNexusSourceProperties
import Amazonka.AppFlow.Types.LambdaConnectorProvisioningConfig
import Amazonka.AppFlow.Types.LookoutMetricsDestinationProperties
import Amazonka.AppFlow.Types.MarketoConnectorProfileCredentials
import Amazonka.AppFlow.Types.MarketoConnectorProfileProperties
import Amazonka.AppFlow.Types.MarketoDestinationProperties
import Amazonka.AppFlow.Types.MarketoMetadata
import Amazonka.AppFlow.Types.MarketoSourceProperties
import Amazonka.AppFlow.Types.MetadataCatalogConfig
import Amazonka.AppFlow.Types.MetadataCatalogDetail
import Amazonka.AppFlow.Types.OAuth2Credentials
import Amazonka.AppFlow.Types.OAuth2CustomParameter
import Amazonka.AppFlow.Types.OAuth2Defaults
import Amazonka.AppFlow.Types.OAuth2Properties
import Amazonka.AppFlow.Types.OAuthCredentials
import Amazonka.AppFlow.Types.OAuthProperties
import Amazonka.AppFlow.Types.PrefixConfig
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningState
import Amazonka.AppFlow.Types.Range
import Amazonka.AppFlow.Types.RedshiftConnectorProfileCredentials
import Amazonka.AppFlow.Types.RedshiftConnectorProfileProperties
import Amazonka.AppFlow.Types.RedshiftDestinationProperties
import Amazonka.AppFlow.Types.RedshiftMetadata
import Amazonka.AppFlow.Types.RegistrationOutput
import Amazonka.AppFlow.Types.S3DestinationProperties
import Amazonka.AppFlow.Types.S3InputFormatConfig
import Amazonka.AppFlow.Types.S3Metadata
import Amazonka.AppFlow.Types.S3OutputFormatConfig
import Amazonka.AppFlow.Types.S3SourceProperties
import Amazonka.AppFlow.Types.SAPODataConnectorProfileCredentials
import Amazonka.AppFlow.Types.SAPODataConnectorProfileProperties
import Amazonka.AppFlow.Types.SAPODataDestinationProperties
import Amazonka.AppFlow.Types.SAPODataMetadata
import Amazonka.AppFlow.Types.SAPODataSourceProperties
import Amazonka.AppFlow.Types.SalesforceConnectorProfileCredentials
import Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties
import Amazonka.AppFlow.Types.SalesforceDestinationProperties
import Amazonka.AppFlow.Types.SalesforceMetadata
import Amazonka.AppFlow.Types.SalesforceSourceProperties
import Amazonka.AppFlow.Types.ScheduledTriggerProperties
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileCredentials
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileProperties
import Amazonka.AppFlow.Types.ServiceNowMetadata
import Amazonka.AppFlow.Types.ServiceNowSourceProperties
import Amazonka.AppFlow.Types.SingularConnectorProfileCredentials
import Amazonka.AppFlow.Types.SingularConnectorProfileProperties
import Amazonka.AppFlow.Types.SingularMetadata
import Amazonka.AppFlow.Types.SingularSourceProperties
import Amazonka.AppFlow.Types.SlackConnectorProfileCredentials
import Amazonka.AppFlow.Types.SlackConnectorProfileProperties
import Amazonka.AppFlow.Types.SlackMetadata
import Amazonka.AppFlow.Types.SlackSourceProperties
import Amazonka.AppFlow.Types.SnowflakeConnectorProfileCredentials
import Amazonka.AppFlow.Types.SnowflakeConnectorProfileProperties
import Amazonka.AppFlow.Types.SnowflakeDestinationProperties
import Amazonka.AppFlow.Types.SnowflakeMetadata
import Amazonka.AppFlow.Types.SourceConnectorProperties
import Amazonka.AppFlow.Types.SourceFieldProperties
import Amazonka.AppFlow.Types.SourceFlowConfig
import Amazonka.AppFlow.Types.SuccessResponseHandlingConfig
import Amazonka.AppFlow.Types.SupportedFieldTypeDetails
import Amazonka.AppFlow.Types.Task
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileCredentials
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileProperties
import Amazonka.AppFlow.Types.TrendmicroMetadata
import Amazonka.AppFlow.Types.TrendmicroSourceProperties
import Amazonka.AppFlow.Types.TriggerConfig
import Amazonka.AppFlow.Types.TriggerProperties
import Amazonka.AppFlow.Types.UpsolverDestinationProperties
import Amazonka.AppFlow.Types.UpsolverMetadata
import Amazonka.AppFlow.Types.UpsolverS3OutputFormatConfig
import Amazonka.AppFlow.Types.VeevaConnectorProfileCredentials
import Amazonka.AppFlow.Types.VeevaConnectorProfileProperties
import Amazonka.AppFlow.Types.VeevaMetadata
import Amazonka.AppFlow.Types.VeevaSourceProperties
import Amazonka.AppFlow.Types.ZendeskConnectorProfileCredentials
import Amazonka.AppFlow.Types.ZendeskConnectorProfileProperties
import Amazonka.AppFlow.Types.ZendeskDestinationProperties
import Amazonka.AppFlow.Types.ZendeskMetadata
import Amazonka.AppFlow.Types.ZendeskSourceProperties
import Amazonka.AppFlow.UnregisterConnector
import Amazonka.AppFlow.UntagResource
import Amazonka.AppFlow.UpdateConnectorProfile
import Amazonka.AppFlow.UpdateConnectorRegistration
import Amazonka.AppFlow.UpdateFlow
