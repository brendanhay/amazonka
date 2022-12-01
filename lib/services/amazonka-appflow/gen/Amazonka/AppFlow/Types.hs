{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConnectorAuthenticationException,
    _UnsupportedOperationException,
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,
    _ConnectorServerException,

    -- * AggregationType
    AggregationType (..),

    -- * AmplitudeConnectorOperator
    AmplitudeConnectorOperator (..),

    -- * AuthenticationType
    AuthenticationType (..),

    -- * CatalogType
    CatalogType (..),

    -- * ConnectionMode
    ConnectionMode (..),

    -- * ConnectorProvisioningType
    ConnectorProvisioningType (..),

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

    -- * OAuth2CustomPropType
    OAuth2CustomPropType (..),

    -- * OAuth2GrantType
    OAuth2GrantType (..),

    -- * Operator
    Operator (..),

    -- * OperatorPropertiesKeys
    OperatorPropertiesKeys (..),

    -- * Operators
    Operators (..),

    -- * PathPrefix
    PathPrefix (..),

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

    -- * SalesforceDataTransferApi
    SalesforceDataTransferApi (..),

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
    aggregationConfig_targetFileSize,

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

    -- * ApiKeyCredentials
    ApiKeyCredentials (..),
    newApiKeyCredentials,
    apiKeyCredentials_apiSecretKey,
    apiKeyCredentials_apiKey,

    -- * AuthParameter
    AuthParameter (..),
    newAuthParameter,
    authParameter_key,
    authParameter_connectorSuppliedValues,
    authParameter_label,
    authParameter_description,
    authParameter_isSensitiveField,
    authParameter_isRequired,

    -- * AuthenticationConfig
    AuthenticationConfig (..),
    newAuthenticationConfig,
    authenticationConfig_customAuthConfigs,
    authenticationConfig_isCustomAuthSupported,
    authenticationConfig_isApiKeyAuthSupported,
    authenticationConfig_isBasicAuthSupported,
    authenticationConfig_isOAuth2Supported,
    authenticationConfig_oAuth2Defaults,

    -- * BasicAuthCredentials
    BasicAuthCredentials (..),
    newBasicAuthCredentials,
    basicAuthCredentials_username,
    basicAuthCredentials_password,

    -- * ConnectorConfiguration
    ConnectorConfiguration (..),
    newConnectorConfiguration,
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

    -- * ConnectorDetail
    ConnectorDetail (..),
    newConnectorDetail,
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

    -- * ConnectorEntity
    ConnectorEntity (..),
    newConnectorEntity,
    connectorEntity_label,
    connectorEntity_hasNestedEntities,
    connectorEntity_name,

    -- * ConnectorEntityField
    ConnectorEntityField (..),
    newConnectorEntityField,
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

    -- * ConnectorMetadata
    ConnectorMetadata (..),
    newConnectorMetadata,
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

    -- * ConnectorOAuthRequest
    ConnectorOAuthRequest (..),
    newConnectorOAuthRequest,
    connectorOAuthRequest_authCode,
    connectorOAuthRequest_redirectUri,

    -- * ConnectorOperator
    ConnectorOperator (..),
    newConnectorOperator,
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

    -- * ConnectorProfile
    ConnectorProfile (..),
    newConnectorProfile,
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

    -- * ConnectorProfileConfig
    ConnectorProfileConfig (..),
    newConnectorProfileConfig,
    connectorProfileConfig_connectorProfileCredentials,
    connectorProfileConfig_connectorProfileProperties,

    -- * ConnectorProfileCredentials
    ConnectorProfileCredentials (..),
    newConnectorProfileCredentials,
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

    -- * ConnectorProfileProperties
    ConnectorProfileProperties (..),
    newConnectorProfileProperties,
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

    -- * ConnectorProvisioningConfig
    ConnectorProvisioningConfig (..),
    newConnectorProvisioningConfig,
    connectorProvisioningConfig_lambda,

    -- * ConnectorRuntimeSetting
    ConnectorRuntimeSetting (..),
    newConnectorRuntimeSetting,
    connectorRuntimeSetting_key,
    connectorRuntimeSetting_label,
    connectorRuntimeSetting_connectorSuppliedValueOptions,
    connectorRuntimeSetting_description,
    connectorRuntimeSetting_scope,
    connectorRuntimeSetting_isRequired,
    connectorRuntimeSetting_dataType,

    -- * CustomAuthConfig
    CustomAuthConfig (..),
    newCustomAuthConfig,
    customAuthConfig_authParameters,
    customAuthConfig_customAuthenticationType,

    -- * CustomAuthCredentials
    CustomAuthCredentials (..),
    newCustomAuthCredentials,
    customAuthCredentials_credentialsMap,
    customAuthCredentials_customAuthenticationType,

    -- * CustomConnectorDestinationProperties
    CustomConnectorDestinationProperties (..),
    newCustomConnectorDestinationProperties,
    customConnectorDestinationProperties_errorHandlingConfig,
    customConnectorDestinationProperties_customProperties,
    customConnectorDestinationProperties_idFieldNames,
    customConnectorDestinationProperties_writeOperationType,
    customConnectorDestinationProperties_entityName,

    -- * CustomConnectorProfileCredentials
    CustomConnectorProfileCredentials (..),
    newCustomConnectorProfileCredentials,
    customConnectorProfileCredentials_apiKey,
    customConnectorProfileCredentials_oauth2,
    customConnectorProfileCredentials_custom,
    customConnectorProfileCredentials_basic,
    customConnectorProfileCredentials_authenticationType,

    -- * CustomConnectorProfileProperties
    CustomConnectorProfileProperties (..),
    newCustomConnectorProfileProperties,
    customConnectorProfileProperties_profileProperties,
    customConnectorProfileProperties_oAuth2Properties,

    -- * CustomConnectorSourceProperties
    CustomConnectorSourceProperties (..),
    newCustomConnectorSourceProperties,
    customConnectorSourceProperties_customProperties,
    customConnectorSourceProperties_entityName,

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

    -- * DestinationFieldProperties
    DestinationFieldProperties (..),
    newDestinationFieldProperties,
    destinationFieldProperties_isUpdatable,
    destinationFieldProperties_supportedWriteOperations,
    destinationFieldProperties_isNullable,
    destinationFieldProperties_isDefaultedOnCreate,
    destinationFieldProperties_isUpsertable,
    destinationFieldProperties_isCreatable,

    -- * DestinationFlowConfig
    DestinationFlowConfig (..),
    newDestinationFlowConfig,
    destinationFlowConfig_apiVersion,
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
    errorHandlingConfig_bucketPrefix,
    errorHandlingConfig_bucketName,
    errorHandlingConfig_failOnFirstDestinationError,

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
    executionRecord_lastUpdatedAt,
    executionRecord_metadataCatalogDetails,
    executionRecord_dataPullStartTime,
    executionRecord_executionId,
    executionRecord_startedAt,
    executionRecord_executionResult,
    executionRecord_executionStatus,
    executionRecord_dataPullEndTime,

    -- * ExecutionResult
    ExecutionResult (..),
    newExecutionResult,
    executionResult_recordsProcessed,
    executionResult_bytesProcessed,
    executionResult_errorInfo,
    executionResult_bytesWritten,

    -- * FieldTypeDetails
    FieldTypeDetails (..),
    newFieldTypeDetails,
    fieldTypeDetails_fieldValueRange,
    fieldTypeDetails_fieldLengthRange,
    fieldTypeDetails_supportedValues,
    fieldTypeDetails_valueRegexPattern,
    fieldTypeDetails_supportedDateFormat,
    fieldTypeDetails_fieldType,
    fieldTypeDetails_filterOperators,

    -- * FlowDefinition
    FlowDefinition (..),
    newFlowDefinition,
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

    -- * GlueDataCatalogConfig
    GlueDataCatalogConfig (..),
    newGlueDataCatalogConfig,
    glueDataCatalogConfig_roleArn,
    glueDataCatalogConfig_databaseName,
    glueDataCatalogConfig_tablePrefix,

    -- * GoogleAnalyticsConnectorProfileCredentials
    GoogleAnalyticsConnectorProfileCredentials (..),
    newGoogleAnalyticsConnectorProfileCredentials,
    googleAnalyticsConnectorProfileCredentials_accessToken,
    googleAnalyticsConnectorProfileCredentials_oAuthRequest,
    googleAnalyticsConnectorProfileCredentials_refreshToken,
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
    honeycodeConnectorProfileCredentials_oAuthRequest,
    honeycodeConnectorProfileCredentials_refreshToken,

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

    -- * LambdaConnectorProvisioningConfig
    LambdaConnectorProvisioningConfig (..),
    newLambdaConnectorProvisioningConfig,
    lambdaConnectorProvisioningConfig_lambdaArn,

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

    -- * MarketoDestinationProperties
    MarketoDestinationProperties (..),
    newMarketoDestinationProperties,
    marketoDestinationProperties_errorHandlingConfig,
    marketoDestinationProperties_object,

    -- * MarketoMetadata
    MarketoMetadata (..),
    newMarketoMetadata,

    -- * MarketoSourceProperties
    MarketoSourceProperties (..),
    newMarketoSourceProperties,
    marketoSourceProperties_object,

    -- * MetadataCatalogConfig
    MetadataCatalogConfig (..),
    newMetadataCatalogConfig,
    metadataCatalogConfig_glueDataCatalog,

    -- * MetadataCatalogDetail
    MetadataCatalogDetail (..),
    newMetadataCatalogDetail,
    metadataCatalogDetail_tableName,
    metadataCatalogDetail_tableRegistrationOutput,
    metadataCatalogDetail_partitionRegistrationOutput,
    metadataCatalogDetail_catalogType,

    -- * OAuth2Credentials
    OAuth2Credentials (..),
    newOAuth2Credentials,
    oAuth2Credentials_accessToken,
    oAuth2Credentials_clientSecret,
    oAuth2Credentials_clientId,
    oAuth2Credentials_oAuthRequest,
    oAuth2Credentials_refreshToken,

    -- * OAuth2CustomParameter
    OAuth2CustomParameter (..),
    newOAuth2CustomParameter,
    oAuth2CustomParameter_key,
    oAuth2CustomParameter_type,
    oAuth2CustomParameter_connectorSuppliedValues,
    oAuth2CustomParameter_label,
    oAuth2CustomParameter_description,
    oAuth2CustomParameter_isSensitiveField,
    oAuth2CustomParameter_isRequired,

    -- * OAuth2Defaults
    OAuth2Defaults (..),
    newOAuth2Defaults,
    oAuth2Defaults_tokenUrls,
    oAuth2Defaults_authCodeUrls,
    oAuth2Defaults_oauth2CustomProperties,
    oAuth2Defaults_oauth2GrantTypesSupported,
    oAuth2Defaults_oauthScopes,

    -- * OAuth2Properties
    OAuth2Properties (..),
    newOAuth2Properties,
    oAuth2Properties_tokenUrlCustomProperties,
    oAuth2Properties_tokenUrl,
    oAuth2Properties_oAuth2GrantType,

    -- * OAuthCredentials
    OAuthCredentials (..),
    newOAuthCredentials,
    oAuthCredentials_accessToken,
    oAuthCredentials_oAuthRequest,
    oAuthCredentials_refreshToken,
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
    prefixConfig_prefixType,
    prefixConfig_prefixFormat,
    prefixConfig_pathPrefixHierarchy,

    -- * PrivateConnectionProvisioningState
    PrivateConnectionProvisioningState (..),
    newPrivateConnectionProvisioningState,
    privateConnectionProvisioningState_status,
    privateConnectionProvisioningState_failureMessage,
    privateConnectionProvisioningState_failureCause,

    -- * Range
    Range (..),
    newRange,
    range_minimum,
    range_maximum,

    -- * RedshiftConnectorProfileCredentials
    RedshiftConnectorProfileCredentials (..),
    newRedshiftConnectorProfileCredentials,
    redshiftConnectorProfileCredentials_password,
    redshiftConnectorProfileCredentials_username,

    -- * RedshiftConnectorProfileProperties
    RedshiftConnectorProfileProperties (..),
    newRedshiftConnectorProfileProperties,
    redshiftConnectorProfileProperties_clusterIdentifier,
    redshiftConnectorProfileProperties_isRedshiftServerless,
    redshiftConnectorProfileProperties_databaseUrl,
    redshiftConnectorProfileProperties_databaseName,
    redshiftConnectorProfileProperties_workgroupName,
    redshiftConnectorProfileProperties_bucketPrefix,
    redshiftConnectorProfileProperties_dataApiRoleArn,
    redshiftConnectorProfileProperties_bucketName,
    redshiftConnectorProfileProperties_roleArn,

    -- * RedshiftDestinationProperties
    RedshiftDestinationProperties (..),
    newRedshiftDestinationProperties,
    redshiftDestinationProperties_errorHandlingConfig,
    redshiftDestinationProperties_bucketPrefix,
    redshiftDestinationProperties_object,
    redshiftDestinationProperties_intermediateBucketName,

    -- * RedshiftMetadata
    RedshiftMetadata (..),
    newRedshiftMetadata,

    -- * RegistrationOutput
    RegistrationOutput (..),
    newRegistrationOutput,
    registrationOutput_message,
    registrationOutput_status,
    registrationOutput_result,

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
    s3OutputFormatConfig_aggregationConfig,
    s3OutputFormatConfig_preserveSourceDataTyping,
    s3OutputFormatConfig_fileType,

    -- * S3SourceProperties
    S3SourceProperties (..),
    newS3SourceProperties,
    s3SourceProperties_bucketPrefix,
    s3SourceProperties_s3InputFormatConfig,
    s3SourceProperties_bucketName,

    -- * SAPODataConnectorProfileCredentials
    SAPODataConnectorProfileCredentials (..),
    newSAPODataConnectorProfileCredentials,
    sAPODataConnectorProfileCredentials_basicAuthCredentials,
    sAPODataConnectorProfileCredentials_oAuthCredentials,

    -- * SAPODataConnectorProfileProperties
    SAPODataConnectorProfileProperties (..),
    newSAPODataConnectorProfileProperties,
    sAPODataConnectorProfileProperties_oAuthProperties,
    sAPODataConnectorProfileProperties_logonLanguage,
    sAPODataConnectorProfileProperties_privateLinkServiceName,
    sAPODataConnectorProfileProperties_applicationHostUrl,
    sAPODataConnectorProfileProperties_applicationServicePath,
    sAPODataConnectorProfileProperties_portNumber,
    sAPODataConnectorProfileProperties_clientNumber,

    -- * SAPODataDestinationProperties
    SAPODataDestinationProperties (..),
    newSAPODataDestinationProperties,
    sAPODataDestinationProperties_errorHandlingConfig,
    sAPODataDestinationProperties_successResponseHandlingConfig,
    sAPODataDestinationProperties_idFieldNames,
    sAPODataDestinationProperties_writeOperationType,
    sAPODataDestinationProperties_objectPath,

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
    salesforceConnectorProfileCredentials_clientCredentialsArn,
    salesforceConnectorProfileCredentials_oAuthRequest,
    salesforceConnectorProfileCredentials_refreshToken,

    -- * SalesforceConnectorProfileProperties
    SalesforceConnectorProfileProperties (..),
    newSalesforceConnectorProfileProperties,
    salesforceConnectorProfileProperties_isSandboxEnvironment,
    salesforceConnectorProfileProperties_instanceUrl,

    -- * SalesforceDestinationProperties
    SalesforceDestinationProperties (..),
    newSalesforceDestinationProperties,
    salesforceDestinationProperties_errorHandlingConfig,
    salesforceDestinationProperties_dataTransferApi,
    salesforceDestinationProperties_idFieldNames,
    salesforceDestinationProperties_writeOperationType,
    salesforceDestinationProperties_object,

    -- * SalesforceMetadata
    SalesforceMetadata (..),
    newSalesforceMetadata,
    salesforceMetadata_dataTransferApis,
    salesforceMetadata_oAuthScopes,

    -- * SalesforceSourceProperties
    SalesforceSourceProperties (..),
    newSalesforceSourceProperties,
    salesforceSourceProperties_includeDeletedRecords,
    salesforceSourceProperties_dataTransferApi,
    salesforceSourceProperties_enableDynamicFieldUpdate,
    salesforceSourceProperties_object,

    -- * ScheduledTriggerProperties
    ScheduledTriggerProperties (..),
    newScheduledTriggerProperties,
    scheduledTriggerProperties_scheduleEndTime,
    scheduledTriggerProperties_scheduleStartTime,
    scheduledTriggerProperties_flowErrorDeactivationThreshold,
    scheduledTriggerProperties_timezone,
    scheduledTriggerProperties_scheduleOffset,
    scheduledTriggerProperties_firstExecutionFrom,
    scheduledTriggerProperties_dataPullMode,
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
    snowflakeConnectorProfileProperties_bucketPrefix,
    snowflakeConnectorProfileProperties_region,
    snowflakeConnectorProfileProperties_accountName,
    snowflakeConnectorProfileProperties_privateLinkServiceName,
    snowflakeConnectorProfileProperties_warehouse,
    snowflakeConnectorProfileProperties_stage,
    snowflakeConnectorProfileProperties_bucketName,

    -- * SnowflakeDestinationProperties
    SnowflakeDestinationProperties (..),
    newSnowflakeDestinationProperties,
    snowflakeDestinationProperties_errorHandlingConfig,
    snowflakeDestinationProperties_bucketPrefix,
    snowflakeDestinationProperties_object,
    snowflakeDestinationProperties_intermediateBucketName,

    -- * SnowflakeMetadata
    SnowflakeMetadata (..),
    newSnowflakeMetadata,
    snowflakeMetadata_supportedRegions,

    -- * SourceConnectorProperties
    SourceConnectorProperties (..),
    newSourceConnectorProperties,
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

    -- * SourceFieldProperties
    SourceFieldProperties (..),
    newSourceFieldProperties,
    sourceFieldProperties_isRetrievable,
    sourceFieldProperties_isTimestampFieldForIncrementalQueries,
    sourceFieldProperties_isQueryable,

    -- * SourceFlowConfig
    SourceFlowConfig (..),
    newSourceFlowConfig,
    sourceFlowConfig_apiVersion,
    sourceFlowConfig_connectorProfileName,
    sourceFlowConfig_incrementalPullConfig,
    sourceFlowConfig_connectorType,
    sourceFlowConfig_sourceConnectorProperties,

    -- * SuccessResponseHandlingConfig
    SuccessResponseHandlingConfig (..),
    newSuccessResponseHandlingConfig,
    successResponseHandlingConfig_bucketPrefix,
    successResponseHandlingConfig_bucketName,

    -- * SupportedFieldTypeDetails
    SupportedFieldTypeDetails (..),
    newSupportedFieldTypeDetails,
    supportedFieldTypeDetails_v1,

    -- * Task
    Task (..),
    newTask,
    task_connectorOperator,
    task_taskProperties,
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
    upsolverS3OutputFormatConfig_aggregationConfig,
    upsolverS3OutputFormatConfig_fileType,
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
    veevaSourceProperties_documentType,
    veevaSourceProperties_includeSourceFiles,
    veevaSourceProperties_includeRenditions,
    veevaSourceProperties_includeAllVersions,
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
    zendeskDestinationProperties_errorHandlingConfig,
    zendeskDestinationProperties_idFieldNames,
    zendeskDestinationProperties_writeOperationType,
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

import Amazonka.AppFlow.Types.AggregationConfig
import Amazonka.AppFlow.Types.AggregationType
import Amazonka.AppFlow.Types.AmplitudeConnectorOperator
import Amazonka.AppFlow.Types.AmplitudeConnectorProfileCredentials
import Amazonka.AppFlow.Types.AmplitudeConnectorProfileProperties
import Amazonka.AppFlow.Types.AmplitudeMetadata
import Amazonka.AppFlow.Types.AmplitudeSourceProperties
import Amazonka.AppFlow.Types.ApiKeyCredentials
import Amazonka.AppFlow.Types.AuthParameter
import Amazonka.AppFlow.Types.AuthenticationConfig
import Amazonka.AppFlow.Types.AuthenticationType
import Amazonka.AppFlow.Types.BasicAuthCredentials
import Amazonka.AppFlow.Types.CatalogType
import Amazonka.AppFlow.Types.ConnectionMode
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
import Amazonka.AppFlow.Types.ConnectorProvisioningType
import Amazonka.AppFlow.Types.ConnectorRuntimeSetting
import Amazonka.AppFlow.Types.ConnectorType
import Amazonka.AppFlow.Types.CustomAuthConfig
import Amazonka.AppFlow.Types.CustomAuthCredentials
import Amazonka.AppFlow.Types.CustomConnectorDestinationProperties
import Amazonka.AppFlow.Types.CustomConnectorProfileCredentials
import Amazonka.AppFlow.Types.CustomConnectorProfileProperties
import Amazonka.AppFlow.Types.CustomConnectorSourceProperties
import Amazonka.AppFlow.Types.CustomerProfilesDestinationProperties
import Amazonka.AppFlow.Types.CustomerProfilesMetadata
import Amazonka.AppFlow.Types.DataPullMode
import Amazonka.AppFlow.Types.DatadogConnectorOperator
import Amazonka.AppFlow.Types.DatadogConnectorProfileCredentials
import Amazonka.AppFlow.Types.DatadogConnectorProfileProperties
import Amazonka.AppFlow.Types.DatadogMetadata
import Amazonka.AppFlow.Types.DatadogSourceProperties
import Amazonka.AppFlow.Types.DestinationConnectorProperties
import Amazonka.AppFlow.Types.DestinationFieldProperties
import Amazonka.AppFlow.Types.DestinationFlowConfig
import Amazonka.AppFlow.Types.DynatraceConnectorOperator
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
import Amazonka.AppFlow.Types.ExecutionStatus
import Amazonka.AppFlow.Types.FieldTypeDetails
import Amazonka.AppFlow.Types.FileType
import Amazonka.AppFlow.Types.FlowDefinition
import Amazonka.AppFlow.Types.FlowStatus
import Amazonka.AppFlow.Types.GlueDataCatalogConfig
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorOperator
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileProperties
import Amazonka.AppFlow.Types.GoogleAnalyticsMetadata
import Amazonka.AppFlow.Types.GoogleAnalyticsSourceProperties
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileCredentials
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileProperties
import Amazonka.AppFlow.Types.HoneycodeDestinationProperties
import Amazonka.AppFlow.Types.HoneycodeMetadata
import Amazonka.AppFlow.Types.IncrementalPullConfig
import Amazonka.AppFlow.Types.InforNexusConnectorOperator
import Amazonka.AppFlow.Types.InforNexusConnectorProfileCredentials
import Amazonka.AppFlow.Types.InforNexusConnectorProfileProperties
import Amazonka.AppFlow.Types.InforNexusMetadata
import Amazonka.AppFlow.Types.InforNexusSourceProperties
import Amazonka.AppFlow.Types.LambdaConnectorProvisioningConfig
import Amazonka.AppFlow.Types.LookoutMetricsDestinationProperties
import Amazonka.AppFlow.Types.MarketoConnectorOperator
import Amazonka.AppFlow.Types.MarketoConnectorProfileCredentials
import Amazonka.AppFlow.Types.MarketoConnectorProfileProperties
import Amazonka.AppFlow.Types.MarketoDestinationProperties
import Amazonka.AppFlow.Types.MarketoMetadata
import Amazonka.AppFlow.Types.MarketoSourceProperties
import Amazonka.AppFlow.Types.MetadataCatalogConfig
import Amazonka.AppFlow.Types.MetadataCatalogDetail
import Amazonka.AppFlow.Types.OAuth2Credentials
import Amazonka.AppFlow.Types.OAuth2CustomParameter
import Amazonka.AppFlow.Types.OAuth2CustomPropType
import Amazonka.AppFlow.Types.OAuth2Defaults
import Amazonka.AppFlow.Types.OAuth2GrantType
import Amazonka.AppFlow.Types.OAuth2Properties
import Amazonka.AppFlow.Types.OAuthCredentials
import Amazonka.AppFlow.Types.OAuthProperties
import Amazonka.AppFlow.Types.Operator
import Amazonka.AppFlow.Types.OperatorPropertiesKeys
import Amazonka.AppFlow.Types.Operators
import Amazonka.AppFlow.Types.PathPrefix
import Amazonka.AppFlow.Types.PrefixConfig
import Amazonka.AppFlow.Types.PrefixFormat
import Amazonka.AppFlow.Types.PrefixType
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningFailureCause
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningState
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningStatus
import Amazonka.AppFlow.Types.Range
import Amazonka.AppFlow.Types.RedshiftConnectorProfileCredentials
import Amazonka.AppFlow.Types.RedshiftConnectorProfileProperties
import Amazonka.AppFlow.Types.RedshiftDestinationProperties
import Amazonka.AppFlow.Types.RedshiftMetadata
import Amazonka.AppFlow.Types.RegistrationOutput
import Amazonka.AppFlow.Types.S3ConnectorOperator
import Amazonka.AppFlow.Types.S3DestinationProperties
import Amazonka.AppFlow.Types.S3InputFileType
import Amazonka.AppFlow.Types.S3InputFormatConfig
import Amazonka.AppFlow.Types.S3Metadata
import Amazonka.AppFlow.Types.S3OutputFormatConfig
import Amazonka.AppFlow.Types.S3SourceProperties
import Amazonka.AppFlow.Types.SAPODataConnectorOperator
import Amazonka.AppFlow.Types.SAPODataConnectorProfileCredentials
import Amazonka.AppFlow.Types.SAPODataConnectorProfileProperties
import Amazonka.AppFlow.Types.SAPODataDestinationProperties
import Amazonka.AppFlow.Types.SAPODataMetadata
import Amazonka.AppFlow.Types.SAPODataSourceProperties
import Amazonka.AppFlow.Types.SalesforceConnectorOperator
import Amazonka.AppFlow.Types.SalesforceConnectorProfileCredentials
import Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties
import Amazonka.AppFlow.Types.SalesforceDataTransferApi
import Amazonka.AppFlow.Types.SalesforceDestinationProperties
import Amazonka.AppFlow.Types.SalesforceMetadata
import Amazonka.AppFlow.Types.SalesforceSourceProperties
import Amazonka.AppFlow.Types.ScheduleFrequencyType
import Amazonka.AppFlow.Types.ScheduledTriggerProperties
import Amazonka.AppFlow.Types.ServiceNowConnectorOperator
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileCredentials
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileProperties
import Amazonka.AppFlow.Types.ServiceNowMetadata
import Amazonka.AppFlow.Types.ServiceNowSourceProperties
import Amazonka.AppFlow.Types.SingularConnectorOperator
import Amazonka.AppFlow.Types.SingularConnectorProfileCredentials
import Amazonka.AppFlow.Types.SingularConnectorProfileProperties
import Amazonka.AppFlow.Types.SingularMetadata
import Amazonka.AppFlow.Types.SingularSourceProperties
import Amazonka.AppFlow.Types.SlackConnectorOperator
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
import Amazonka.AppFlow.Types.TaskType
import Amazonka.AppFlow.Types.TrendmicroConnectorOperator
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileCredentials
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileProperties
import Amazonka.AppFlow.Types.TrendmicroMetadata
import Amazonka.AppFlow.Types.TrendmicroSourceProperties
import Amazonka.AppFlow.Types.TriggerConfig
import Amazonka.AppFlow.Types.TriggerProperties
import Amazonka.AppFlow.Types.TriggerType
import Amazonka.AppFlow.Types.UpsolverDestinationProperties
import Amazonka.AppFlow.Types.UpsolverMetadata
import Amazonka.AppFlow.Types.UpsolverS3OutputFormatConfig
import Amazonka.AppFlow.Types.VeevaConnectorOperator
import Amazonka.AppFlow.Types.VeevaConnectorProfileCredentials
import Amazonka.AppFlow.Types.VeevaConnectorProfileProperties
import Amazonka.AppFlow.Types.VeevaMetadata
import Amazonka.AppFlow.Types.VeevaSourceProperties
import Amazonka.AppFlow.Types.WriteOperationType
import Amazonka.AppFlow.Types.ZendeskConnectorOperator
import Amazonka.AppFlow.Types.ZendeskConnectorProfileCredentials
import Amazonka.AppFlow.Types.ZendeskConnectorProfileProperties
import Amazonka.AppFlow.Types.ZendeskDestinationProperties
import Amazonka.AppFlow.Types.ZendeskMetadata
import Amazonka.AppFlow.Types.ZendeskSourceProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-23@ of the Amazon Appflow SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AppFlow",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "appflow",
      Core.signingName = "appflow",
      Core.version = "2020-08-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AppFlow",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | An error occurred when authenticating with the connector endpoint.
_ConnectorAuthenticationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConnectorAuthenticationException =
  Core._MatchServiceError
    defaultService
    "ConnectorAuthenticationException"
    Prelude.. Core.hasStatus 401

-- | The requested operation is not supported for the current flow.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"
    Prelude.. Core.hasStatus 400

-- | AppFlow\/Requester has invalid or missing permissions.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An internal service error occurred during the processing of your
-- request. Try again later.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request would cause a service quota (such as the number of flows) to
-- be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The resource specified in the request (such as the source or destination
-- connector profile) is not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | There was a conflict when processing the request (for example, a flow
-- with the given name already exists within the account. Check for
-- conflicting resource names and try again.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | API calls have exceeded the maximum allowed API request rate per account
-- and per Region.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The request has invalid or missing parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | An error occurred when retrieving data from the connector endpoint.
_ConnectorServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConnectorServerException =
  Core._MatchServiceError
    defaultService
    "ConnectorServerException"
    Prelude.. Core.hasStatus 400
