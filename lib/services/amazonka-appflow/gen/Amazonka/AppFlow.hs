{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AppFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-08-23@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the Amazon AppFlow API reference. This guide is for
-- developers who need detailed information about the Amazon AppFlow API
-- operations, data types, and errors.
--
-- Amazon AppFlow is a fully managed integration service that enables you
-- to securely transfer data between software as a service (SaaS)
-- applications like Salesforce, Marketo, Slack, and ServiceNow, and Amazon
-- Web Services like Amazon S3 and Amazon Redshift.
--
-- Use the following links to get started on the Amazon AppFlow API:
--
-- -   <https://docs.aws.amazon.com/appflow/1.0/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all Amazon AppFlow API operations.
--
-- -   <https://docs.aws.amazon.com/appflow/1.0/APIReference/API_Types.html Data types>:
--     An alphabetical list of all Amazon AppFlow data types.
--
-- -   <https://docs.aws.amazon.com/appflow/1.0/APIReference/CommonParameters.html Common parameters>:
--     Parameters that all Query operations can use.
--
-- -   <https://docs.aws.amazon.com/appflow/1.0/APIReference/CommonErrors.html Common errors>:
--     Client and server errors that all operations can return.
--
-- If you\'re new to Amazon AppFlow, we recommend that you review the
-- <https://docs.aws.amazon.com/appflow/latest/userguide/what-is-appflow.html Amazon AppFlow User Guide>.
--
-- Amazon AppFlow API users can use vendor-specific mechanisms for OAuth,
-- and include applicable OAuth attributes (such as @auth-code@ and
-- @redirecturi@) with the connector-specific @ConnectorProfileProperties@
-- when creating a new connector profile using Amazon AppFlow API
-- operations. For example, Salesforce users can refer to the
-- <https://help.salesforce.com/articleView?id=remoteaccess_authenticate.htm Authorize Apps with OAuth>
-- documentation.
module Amazonka.AppFlow
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ConnectorAuthenticationException
    _ConnectorAuthenticationException,

    -- ** ConnectorServerException
    _ConnectorServerException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateConnectorProfile
    CreateConnectorProfile (CreateConnectorProfile'),
    newCreateConnectorProfile,
    CreateConnectorProfileResponse (CreateConnectorProfileResponse'),
    newCreateConnectorProfileResponse,

    -- ** CreateFlow
    CreateFlow (CreateFlow'),
    newCreateFlow,
    CreateFlowResponse (CreateFlowResponse'),
    newCreateFlowResponse,

    -- ** DeleteConnectorProfile
    DeleteConnectorProfile (DeleteConnectorProfile'),
    newDeleteConnectorProfile,
    DeleteConnectorProfileResponse (DeleteConnectorProfileResponse'),
    newDeleteConnectorProfileResponse,

    -- ** DeleteFlow
    DeleteFlow (DeleteFlow'),
    newDeleteFlow,
    DeleteFlowResponse (DeleteFlowResponse'),
    newDeleteFlowResponse,

    -- ** DescribeConnector
    DescribeConnector (DescribeConnector'),
    newDescribeConnector,
    DescribeConnectorResponse (DescribeConnectorResponse'),
    newDescribeConnectorResponse,

    -- ** DescribeConnectorEntity
    DescribeConnectorEntity (DescribeConnectorEntity'),
    newDescribeConnectorEntity,
    DescribeConnectorEntityResponse (DescribeConnectorEntityResponse'),
    newDescribeConnectorEntityResponse,

    -- ** DescribeConnectorProfiles
    DescribeConnectorProfiles (DescribeConnectorProfiles'),
    newDescribeConnectorProfiles,
    DescribeConnectorProfilesResponse (DescribeConnectorProfilesResponse'),
    newDescribeConnectorProfilesResponse,

    -- ** DescribeConnectors
    DescribeConnectors (DescribeConnectors'),
    newDescribeConnectors,
    DescribeConnectorsResponse (DescribeConnectorsResponse'),
    newDescribeConnectorsResponse,

    -- ** DescribeFlow
    DescribeFlow (DescribeFlow'),
    newDescribeFlow,
    DescribeFlowResponse (DescribeFlowResponse'),
    newDescribeFlowResponse,

    -- ** DescribeFlowExecutionRecords
    DescribeFlowExecutionRecords (DescribeFlowExecutionRecords'),
    newDescribeFlowExecutionRecords,
    DescribeFlowExecutionRecordsResponse (DescribeFlowExecutionRecordsResponse'),
    newDescribeFlowExecutionRecordsResponse,

    -- ** ListConnectorEntities
    ListConnectorEntities (ListConnectorEntities'),
    newListConnectorEntities,
    ListConnectorEntitiesResponse (ListConnectorEntitiesResponse'),
    newListConnectorEntitiesResponse,

    -- ** ListConnectors
    ListConnectors (ListConnectors'),
    newListConnectors,
    ListConnectorsResponse (ListConnectorsResponse'),
    newListConnectorsResponse,

    -- ** ListFlows
    ListFlows (ListFlows'),
    newListFlows,
    ListFlowsResponse (ListFlowsResponse'),
    newListFlowsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RegisterConnector
    RegisterConnector (RegisterConnector'),
    newRegisterConnector,
    RegisterConnectorResponse (RegisterConnectorResponse'),
    newRegisterConnectorResponse,

    -- ** StartFlow
    StartFlow (StartFlow'),
    newStartFlow,
    StartFlowResponse (StartFlowResponse'),
    newStartFlowResponse,

    -- ** StopFlow
    StopFlow (StopFlow'),
    newStopFlow,
    StopFlowResponse (StopFlowResponse'),
    newStopFlowResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UnregisterConnector
    UnregisterConnector (UnregisterConnector'),
    newUnregisterConnector,
    UnregisterConnectorResponse (UnregisterConnectorResponse'),
    newUnregisterConnectorResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateConnectorProfile
    UpdateConnectorProfile (UpdateConnectorProfile'),
    newUpdateConnectorProfile,
    UpdateConnectorProfileResponse (UpdateConnectorProfileResponse'),
    newUpdateConnectorProfileResponse,

    -- ** UpdateConnectorRegistration
    UpdateConnectorRegistration (UpdateConnectorRegistration'),
    newUpdateConnectorRegistration,
    UpdateConnectorRegistrationResponse (UpdateConnectorRegistrationResponse'),
    newUpdateConnectorRegistrationResponse,

    -- ** UpdateFlow
    UpdateFlow (UpdateFlow'),
    newUpdateFlow,
    UpdateFlowResponse (UpdateFlowResponse'),
    newUpdateFlowResponse,

    -- * Types

    -- ** AggregationType
    AggregationType (..),

    -- ** AmplitudeConnectorOperator
    AmplitudeConnectorOperator (..),

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** CatalogType
    CatalogType (..),

    -- ** ConnectionMode
    ConnectionMode (..),

    -- ** ConnectorProvisioningType
    ConnectorProvisioningType (..),

    -- ** ConnectorType
    ConnectorType (..),

    -- ** DataPullMode
    DataPullMode (..),

    -- ** DatadogConnectorOperator
    DatadogConnectorOperator (..),

    -- ** DynatraceConnectorOperator
    DynatraceConnectorOperator (..),

    -- ** ExecutionStatus
    ExecutionStatus (..),

    -- ** FileType
    FileType (..),

    -- ** FlowStatus
    FlowStatus (..),

    -- ** GoogleAnalyticsConnectorOperator
    GoogleAnalyticsConnectorOperator (..),

    -- ** InforNexusConnectorOperator
    InforNexusConnectorOperator (..),

    -- ** MarketoConnectorOperator
    MarketoConnectorOperator (..),

    -- ** OAuth2CustomPropType
    OAuth2CustomPropType (..),

    -- ** OAuth2GrantType
    OAuth2GrantType (..),

    -- ** Operator
    Operator (..),

    -- ** OperatorPropertiesKeys
    OperatorPropertiesKeys (..),

    -- ** Operators
    Operators (..),

    -- ** PathPrefix
    PathPrefix (..),

    -- ** PrefixFormat
    PrefixFormat (..),

    -- ** PrefixType
    PrefixType (..),

    -- ** PrivateConnectionProvisioningFailureCause
    PrivateConnectionProvisioningFailureCause (..),

    -- ** PrivateConnectionProvisioningStatus
    PrivateConnectionProvisioningStatus (..),

    -- ** S3ConnectorOperator
    S3ConnectorOperator (..),

    -- ** S3InputFileType
    S3InputFileType (..),

    -- ** SAPODataConnectorOperator
    SAPODataConnectorOperator (..),

    -- ** SalesforceConnectorOperator
    SalesforceConnectorOperator (..),

    -- ** SalesforceDataTransferApi
    SalesforceDataTransferApi (..),

    -- ** ScheduleFrequencyType
    ScheduleFrequencyType (..),

    -- ** ServiceNowConnectorOperator
    ServiceNowConnectorOperator (..),

    -- ** SingularConnectorOperator
    SingularConnectorOperator (..),

    -- ** SlackConnectorOperator
    SlackConnectorOperator (..),

    -- ** TaskType
    TaskType (..),

    -- ** TrendmicroConnectorOperator
    TrendmicroConnectorOperator (..),

    -- ** TriggerType
    TriggerType (..),

    -- ** VeevaConnectorOperator
    VeevaConnectorOperator (..),

    -- ** WriteOperationType
    WriteOperationType (..),

    -- ** ZendeskConnectorOperator
    ZendeskConnectorOperator (..),

    -- ** AggregationConfig
    AggregationConfig (AggregationConfig'),
    newAggregationConfig,

    -- ** AmplitudeConnectorProfileCredentials
    AmplitudeConnectorProfileCredentials (AmplitudeConnectorProfileCredentials'),
    newAmplitudeConnectorProfileCredentials,

    -- ** AmplitudeConnectorProfileProperties
    AmplitudeConnectorProfileProperties (AmplitudeConnectorProfileProperties'),
    newAmplitudeConnectorProfileProperties,

    -- ** AmplitudeMetadata
    AmplitudeMetadata (AmplitudeMetadata'),
    newAmplitudeMetadata,

    -- ** AmplitudeSourceProperties
    AmplitudeSourceProperties (AmplitudeSourceProperties'),
    newAmplitudeSourceProperties,

    -- ** ApiKeyCredentials
    ApiKeyCredentials (ApiKeyCredentials'),
    newApiKeyCredentials,

    -- ** AuthParameter
    AuthParameter (AuthParameter'),
    newAuthParameter,

    -- ** AuthenticationConfig
    AuthenticationConfig (AuthenticationConfig'),
    newAuthenticationConfig,

    -- ** BasicAuthCredentials
    BasicAuthCredentials (BasicAuthCredentials'),
    newBasicAuthCredentials,

    -- ** ConnectorConfiguration
    ConnectorConfiguration (ConnectorConfiguration'),
    newConnectorConfiguration,

    -- ** ConnectorDetail
    ConnectorDetail (ConnectorDetail'),
    newConnectorDetail,

    -- ** ConnectorEntity
    ConnectorEntity (ConnectorEntity'),
    newConnectorEntity,

    -- ** ConnectorEntityField
    ConnectorEntityField (ConnectorEntityField'),
    newConnectorEntityField,

    -- ** ConnectorMetadata
    ConnectorMetadata (ConnectorMetadata'),
    newConnectorMetadata,

    -- ** ConnectorOAuthRequest
    ConnectorOAuthRequest (ConnectorOAuthRequest'),
    newConnectorOAuthRequest,

    -- ** ConnectorOperator
    ConnectorOperator (ConnectorOperator'),
    newConnectorOperator,

    -- ** ConnectorProfile
    ConnectorProfile (ConnectorProfile'),
    newConnectorProfile,

    -- ** ConnectorProfileConfig
    ConnectorProfileConfig (ConnectorProfileConfig'),
    newConnectorProfileConfig,

    -- ** ConnectorProfileCredentials
    ConnectorProfileCredentials (ConnectorProfileCredentials'),
    newConnectorProfileCredentials,

    -- ** ConnectorProfileProperties
    ConnectorProfileProperties (ConnectorProfileProperties'),
    newConnectorProfileProperties,

    -- ** ConnectorProvisioningConfig
    ConnectorProvisioningConfig (ConnectorProvisioningConfig'),
    newConnectorProvisioningConfig,

    -- ** ConnectorRuntimeSetting
    ConnectorRuntimeSetting (ConnectorRuntimeSetting'),
    newConnectorRuntimeSetting,

    -- ** CustomAuthConfig
    CustomAuthConfig (CustomAuthConfig'),
    newCustomAuthConfig,

    -- ** CustomAuthCredentials
    CustomAuthCredentials (CustomAuthCredentials'),
    newCustomAuthCredentials,

    -- ** CustomConnectorDestinationProperties
    CustomConnectorDestinationProperties (CustomConnectorDestinationProperties'),
    newCustomConnectorDestinationProperties,

    -- ** CustomConnectorProfileCredentials
    CustomConnectorProfileCredentials (CustomConnectorProfileCredentials'),
    newCustomConnectorProfileCredentials,

    -- ** CustomConnectorProfileProperties
    CustomConnectorProfileProperties (CustomConnectorProfileProperties'),
    newCustomConnectorProfileProperties,

    -- ** CustomConnectorSourceProperties
    CustomConnectorSourceProperties (CustomConnectorSourceProperties'),
    newCustomConnectorSourceProperties,

    -- ** CustomerProfilesDestinationProperties
    CustomerProfilesDestinationProperties (CustomerProfilesDestinationProperties'),
    newCustomerProfilesDestinationProperties,

    -- ** CustomerProfilesMetadata
    CustomerProfilesMetadata (CustomerProfilesMetadata'),
    newCustomerProfilesMetadata,

    -- ** DatadogConnectorProfileCredentials
    DatadogConnectorProfileCredentials (DatadogConnectorProfileCredentials'),
    newDatadogConnectorProfileCredentials,

    -- ** DatadogConnectorProfileProperties
    DatadogConnectorProfileProperties (DatadogConnectorProfileProperties'),
    newDatadogConnectorProfileProperties,

    -- ** DatadogMetadata
    DatadogMetadata (DatadogMetadata'),
    newDatadogMetadata,

    -- ** DatadogSourceProperties
    DatadogSourceProperties (DatadogSourceProperties'),
    newDatadogSourceProperties,

    -- ** DestinationConnectorProperties
    DestinationConnectorProperties (DestinationConnectorProperties'),
    newDestinationConnectorProperties,

    -- ** DestinationFieldProperties
    DestinationFieldProperties (DestinationFieldProperties'),
    newDestinationFieldProperties,

    -- ** DestinationFlowConfig
    DestinationFlowConfig (DestinationFlowConfig'),
    newDestinationFlowConfig,

    -- ** DynatraceConnectorProfileCredentials
    DynatraceConnectorProfileCredentials (DynatraceConnectorProfileCredentials'),
    newDynatraceConnectorProfileCredentials,

    -- ** DynatraceConnectorProfileProperties
    DynatraceConnectorProfileProperties (DynatraceConnectorProfileProperties'),
    newDynatraceConnectorProfileProperties,

    -- ** DynatraceMetadata
    DynatraceMetadata (DynatraceMetadata'),
    newDynatraceMetadata,

    -- ** DynatraceSourceProperties
    DynatraceSourceProperties (DynatraceSourceProperties'),
    newDynatraceSourceProperties,

    -- ** ErrorHandlingConfig
    ErrorHandlingConfig (ErrorHandlingConfig'),
    newErrorHandlingConfig,

    -- ** ErrorInfo
    ErrorInfo (ErrorInfo'),
    newErrorInfo,

    -- ** EventBridgeDestinationProperties
    EventBridgeDestinationProperties (EventBridgeDestinationProperties'),
    newEventBridgeDestinationProperties,

    -- ** EventBridgeMetadata
    EventBridgeMetadata (EventBridgeMetadata'),
    newEventBridgeMetadata,

    -- ** ExecutionDetails
    ExecutionDetails (ExecutionDetails'),
    newExecutionDetails,

    -- ** ExecutionRecord
    ExecutionRecord (ExecutionRecord'),
    newExecutionRecord,

    -- ** ExecutionResult
    ExecutionResult (ExecutionResult'),
    newExecutionResult,

    -- ** FieldTypeDetails
    FieldTypeDetails (FieldTypeDetails'),
    newFieldTypeDetails,

    -- ** FlowDefinition
    FlowDefinition (FlowDefinition'),
    newFlowDefinition,

    -- ** GlueDataCatalogConfig
    GlueDataCatalogConfig (GlueDataCatalogConfig'),
    newGlueDataCatalogConfig,

    -- ** GoogleAnalyticsConnectorProfileCredentials
    GoogleAnalyticsConnectorProfileCredentials (GoogleAnalyticsConnectorProfileCredentials'),
    newGoogleAnalyticsConnectorProfileCredentials,

    -- ** GoogleAnalyticsConnectorProfileProperties
    GoogleAnalyticsConnectorProfileProperties (GoogleAnalyticsConnectorProfileProperties'),
    newGoogleAnalyticsConnectorProfileProperties,

    -- ** GoogleAnalyticsMetadata
    GoogleAnalyticsMetadata (GoogleAnalyticsMetadata'),
    newGoogleAnalyticsMetadata,

    -- ** GoogleAnalyticsSourceProperties
    GoogleAnalyticsSourceProperties (GoogleAnalyticsSourceProperties'),
    newGoogleAnalyticsSourceProperties,

    -- ** HoneycodeConnectorProfileCredentials
    HoneycodeConnectorProfileCredentials (HoneycodeConnectorProfileCredentials'),
    newHoneycodeConnectorProfileCredentials,

    -- ** HoneycodeConnectorProfileProperties
    HoneycodeConnectorProfileProperties (HoneycodeConnectorProfileProperties'),
    newHoneycodeConnectorProfileProperties,

    -- ** HoneycodeDestinationProperties
    HoneycodeDestinationProperties (HoneycodeDestinationProperties'),
    newHoneycodeDestinationProperties,

    -- ** HoneycodeMetadata
    HoneycodeMetadata (HoneycodeMetadata'),
    newHoneycodeMetadata,

    -- ** IncrementalPullConfig
    IncrementalPullConfig (IncrementalPullConfig'),
    newIncrementalPullConfig,

    -- ** InforNexusConnectorProfileCredentials
    InforNexusConnectorProfileCredentials (InforNexusConnectorProfileCredentials'),
    newInforNexusConnectorProfileCredentials,

    -- ** InforNexusConnectorProfileProperties
    InforNexusConnectorProfileProperties (InforNexusConnectorProfileProperties'),
    newInforNexusConnectorProfileProperties,

    -- ** InforNexusMetadata
    InforNexusMetadata (InforNexusMetadata'),
    newInforNexusMetadata,

    -- ** InforNexusSourceProperties
    InforNexusSourceProperties (InforNexusSourceProperties'),
    newInforNexusSourceProperties,

    -- ** LambdaConnectorProvisioningConfig
    LambdaConnectorProvisioningConfig (LambdaConnectorProvisioningConfig'),
    newLambdaConnectorProvisioningConfig,

    -- ** LookoutMetricsDestinationProperties
    LookoutMetricsDestinationProperties (LookoutMetricsDestinationProperties'),
    newLookoutMetricsDestinationProperties,

    -- ** MarketoConnectorProfileCredentials
    MarketoConnectorProfileCredentials (MarketoConnectorProfileCredentials'),
    newMarketoConnectorProfileCredentials,

    -- ** MarketoConnectorProfileProperties
    MarketoConnectorProfileProperties (MarketoConnectorProfileProperties'),
    newMarketoConnectorProfileProperties,

    -- ** MarketoDestinationProperties
    MarketoDestinationProperties (MarketoDestinationProperties'),
    newMarketoDestinationProperties,

    -- ** MarketoMetadata
    MarketoMetadata (MarketoMetadata'),
    newMarketoMetadata,

    -- ** MarketoSourceProperties
    MarketoSourceProperties (MarketoSourceProperties'),
    newMarketoSourceProperties,

    -- ** MetadataCatalogConfig
    MetadataCatalogConfig (MetadataCatalogConfig'),
    newMetadataCatalogConfig,

    -- ** MetadataCatalogDetail
    MetadataCatalogDetail (MetadataCatalogDetail'),
    newMetadataCatalogDetail,

    -- ** OAuth2Credentials
    OAuth2Credentials (OAuth2Credentials'),
    newOAuth2Credentials,

    -- ** OAuth2CustomParameter
    OAuth2CustomParameter (OAuth2CustomParameter'),
    newOAuth2CustomParameter,

    -- ** OAuth2Defaults
    OAuth2Defaults (OAuth2Defaults'),
    newOAuth2Defaults,

    -- ** OAuth2Properties
    OAuth2Properties (OAuth2Properties'),
    newOAuth2Properties,

    -- ** OAuthCredentials
    OAuthCredentials (OAuthCredentials'),
    newOAuthCredentials,

    -- ** OAuthProperties
    OAuthProperties (OAuthProperties'),
    newOAuthProperties,

    -- ** PrefixConfig
    PrefixConfig (PrefixConfig'),
    newPrefixConfig,

    -- ** PrivateConnectionProvisioningState
    PrivateConnectionProvisioningState (PrivateConnectionProvisioningState'),
    newPrivateConnectionProvisioningState,

    -- ** Range
    Range (Range'),
    newRange,

    -- ** RedshiftConnectorProfileCredentials
    RedshiftConnectorProfileCredentials (RedshiftConnectorProfileCredentials'),
    newRedshiftConnectorProfileCredentials,

    -- ** RedshiftConnectorProfileProperties
    RedshiftConnectorProfileProperties (RedshiftConnectorProfileProperties'),
    newRedshiftConnectorProfileProperties,

    -- ** RedshiftDestinationProperties
    RedshiftDestinationProperties (RedshiftDestinationProperties'),
    newRedshiftDestinationProperties,

    -- ** RedshiftMetadata
    RedshiftMetadata (RedshiftMetadata'),
    newRedshiftMetadata,

    -- ** RegistrationOutput
    RegistrationOutput (RegistrationOutput'),
    newRegistrationOutput,

    -- ** S3DestinationProperties
    S3DestinationProperties (S3DestinationProperties'),
    newS3DestinationProperties,

    -- ** S3InputFormatConfig
    S3InputFormatConfig (S3InputFormatConfig'),
    newS3InputFormatConfig,

    -- ** S3Metadata
    S3Metadata (S3Metadata'),
    newS3Metadata,

    -- ** S3OutputFormatConfig
    S3OutputFormatConfig (S3OutputFormatConfig'),
    newS3OutputFormatConfig,

    -- ** S3SourceProperties
    S3SourceProperties (S3SourceProperties'),
    newS3SourceProperties,

    -- ** SAPODataConnectorProfileCredentials
    SAPODataConnectorProfileCredentials (SAPODataConnectorProfileCredentials'),
    newSAPODataConnectorProfileCredentials,

    -- ** SAPODataConnectorProfileProperties
    SAPODataConnectorProfileProperties (SAPODataConnectorProfileProperties'),
    newSAPODataConnectorProfileProperties,

    -- ** SAPODataDestinationProperties
    SAPODataDestinationProperties (SAPODataDestinationProperties'),
    newSAPODataDestinationProperties,

    -- ** SAPODataMetadata
    SAPODataMetadata (SAPODataMetadata'),
    newSAPODataMetadata,

    -- ** SAPODataSourceProperties
    SAPODataSourceProperties (SAPODataSourceProperties'),
    newSAPODataSourceProperties,

    -- ** SalesforceConnectorProfileCredentials
    SalesforceConnectorProfileCredentials (SalesforceConnectorProfileCredentials'),
    newSalesforceConnectorProfileCredentials,

    -- ** SalesforceConnectorProfileProperties
    SalesforceConnectorProfileProperties (SalesforceConnectorProfileProperties'),
    newSalesforceConnectorProfileProperties,

    -- ** SalesforceDestinationProperties
    SalesforceDestinationProperties (SalesforceDestinationProperties'),
    newSalesforceDestinationProperties,

    -- ** SalesforceMetadata
    SalesforceMetadata (SalesforceMetadata'),
    newSalesforceMetadata,

    -- ** SalesforceSourceProperties
    SalesforceSourceProperties (SalesforceSourceProperties'),
    newSalesforceSourceProperties,

    -- ** ScheduledTriggerProperties
    ScheduledTriggerProperties (ScheduledTriggerProperties'),
    newScheduledTriggerProperties,

    -- ** ServiceNowConnectorProfileCredentials
    ServiceNowConnectorProfileCredentials (ServiceNowConnectorProfileCredentials'),
    newServiceNowConnectorProfileCredentials,

    -- ** ServiceNowConnectorProfileProperties
    ServiceNowConnectorProfileProperties (ServiceNowConnectorProfileProperties'),
    newServiceNowConnectorProfileProperties,

    -- ** ServiceNowMetadata
    ServiceNowMetadata (ServiceNowMetadata'),
    newServiceNowMetadata,

    -- ** ServiceNowSourceProperties
    ServiceNowSourceProperties (ServiceNowSourceProperties'),
    newServiceNowSourceProperties,

    -- ** SingularConnectorProfileCredentials
    SingularConnectorProfileCredentials (SingularConnectorProfileCredentials'),
    newSingularConnectorProfileCredentials,

    -- ** SingularConnectorProfileProperties
    SingularConnectorProfileProperties (SingularConnectorProfileProperties'),
    newSingularConnectorProfileProperties,

    -- ** SingularMetadata
    SingularMetadata (SingularMetadata'),
    newSingularMetadata,

    -- ** SingularSourceProperties
    SingularSourceProperties (SingularSourceProperties'),
    newSingularSourceProperties,

    -- ** SlackConnectorProfileCredentials
    SlackConnectorProfileCredentials (SlackConnectorProfileCredentials'),
    newSlackConnectorProfileCredentials,

    -- ** SlackConnectorProfileProperties
    SlackConnectorProfileProperties (SlackConnectorProfileProperties'),
    newSlackConnectorProfileProperties,

    -- ** SlackMetadata
    SlackMetadata (SlackMetadata'),
    newSlackMetadata,

    -- ** SlackSourceProperties
    SlackSourceProperties (SlackSourceProperties'),
    newSlackSourceProperties,

    -- ** SnowflakeConnectorProfileCredentials
    SnowflakeConnectorProfileCredentials (SnowflakeConnectorProfileCredentials'),
    newSnowflakeConnectorProfileCredentials,

    -- ** SnowflakeConnectorProfileProperties
    SnowflakeConnectorProfileProperties (SnowflakeConnectorProfileProperties'),
    newSnowflakeConnectorProfileProperties,

    -- ** SnowflakeDestinationProperties
    SnowflakeDestinationProperties (SnowflakeDestinationProperties'),
    newSnowflakeDestinationProperties,

    -- ** SnowflakeMetadata
    SnowflakeMetadata (SnowflakeMetadata'),
    newSnowflakeMetadata,

    -- ** SourceConnectorProperties
    SourceConnectorProperties (SourceConnectorProperties'),
    newSourceConnectorProperties,

    -- ** SourceFieldProperties
    SourceFieldProperties (SourceFieldProperties'),
    newSourceFieldProperties,

    -- ** SourceFlowConfig
    SourceFlowConfig (SourceFlowConfig'),
    newSourceFlowConfig,

    -- ** SuccessResponseHandlingConfig
    SuccessResponseHandlingConfig (SuccessResponseHandlingConfig'),
    newSuccessResponseHandlingConfig,

    -- ** SupportedFieldTypeDetails
    SupportedFieldTypeDetails (SupportedFieldTypeDetails'),
    newSupportedFieldTypeDetails,

    -- ** Task
    Task (Task'),
    newTask,

    -- ** TrendmicroConnectorProfileCredentials
    TrendmicroConnectorProfileCredentials (TrendmicroConnectorProfileCredentials'),
    newTrendmicroConnectorProfileCredentials,

    -- ** TrendmicroConnectorProfileProperties
    TrendmicroConnectorProfileProperties (TrendmicroConnectorProfileProperties'),
    newTrendmicroConnectorProfileProperties,

    -- ** TrendmicroMetadata
    TrendmicroMetadata (TrendmicroMetadata'),
    newTrendmicroMetadata,

    -- ** TrendmicroSourceProperties
    TrendmicroSourceProperties (TrendmicroSourceProperties'),
    newTrendmicroSourceProperties,

    -- ** TriggerConfig
    TriggerConfig (TriggerConfig'),
    newTriggerConfig,

    -- ** TriggerProperties
    TriggerProperties (TriggerProperties'),
    newTriggerProperties,

    -- ** UpsolverDestinationProperties
    UpsolverDestinationProperties (UpsolverDestinationProperties'),
    newUpsolverDestinationProperties,

    -- ** UpsolverMetadata
    UpsolverMetadata (UpsolverMetadata'),
    newUpsolverMetadata,

    -- ** UpsolverS3OutputFormatConfig
    UpsolverS3OutputFormatConfig (UpsolverS3OutputFormatConfig'),
    newUpsolverS3OutputFormatConfig,

    -- ** VeevaConnectorProfileCredentials
    VeevaConnectorProfileCredentials (VeevaConnectorProfileCredentials'),
    newVeevaConnectorProfileCredentials,

    -- ** VeevaConnectorProfileProperties
    VeevaConnectorProfileProperties (VeevaConnectorProfileProperties'),
    newVeevaConnectorProfileProperties,

    -- ** VeevaMetadata
    VeevaMetadata (VeevaMetadata'),
    newVeevaMetadata,

    -- ** VeevaSourceProperties
    VeevaSourceProperties (VeevaSourceProperties'),
    newVeevaSourceProperties,

    -- ** ZendeskConnectorProfileCredentials
    ZendeskConnectorProfileCredentials (ZendeskConnectorProfileCredentials'),
    newZendeskConnectorProfileCredentials,

    -- ** ZendeskConnectorProfileProperties
    ZendeskConnectorProfileProperties (ZendeskConnectorProfileProperties'),
    newZendeskConnectorProfileProperties,

    -- ** ZendeskDestinationProperties
    ZendeskDestinationProperties (ZendeskDestinationProperties'),
    newZendeskDestinationProperties,

    -- ** ZendeskMetadata
    ZendeskMetadata (ZendeskMetadata'),
    newZendeskMetadata,

    -- ** ZendeskSourceProperties
    ZendeskSourceProperties (ZendeskSourceProperties'),
    newZendeskSourceProperties,
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
import Amazonka.AppFlow.Lens
import Amazonka.AppFlow.ListConnectorEntities
import Amazonka.AppFlow.ListConnectors
import Amazonka.AppFlow.ListFlows
import Amazonka.AppFlow.ListTagsForResource
import Amazonka.AppFlow.RegisterConnector
import Amazonka.AppFlow.StartFlow
import Amazonka.AppFlow.StopFlow
import Amazonka.AppFlow.TagResource
import Amazonka.AppFlow.Types
import Amazonka.AppFlow.UnregisterConnector
import Amazonka.AppFlow.UntagResource
import Amazonka.AppFlow.UpdateConnectorProfile
import Amazonka.AppFlow.UpdateConnectorRegistration
import Amazonka.AppFlow.UpdateFlow
import Amazonka.AppFlow.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AppFlow'.

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
