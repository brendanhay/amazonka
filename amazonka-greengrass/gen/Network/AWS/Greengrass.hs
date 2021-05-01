{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT Greengrass seamlessly extends AWS onto physical devices so they
-- can act locally on the data they generate, while still using the cloud
-- for management, analytics, and durable storage. AWS IoT Greengrass
-- ensures your devices can respond quickly to local events and operate
-- with intermittent connectivity. AWS IoT Greengrass minimizes the cost of
-- transmitting data to the cloud by allowing you to author AWS Lambda
-- functions that execute locally.
module Network.AWS.Greengrass
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteSubscriptionDefinition
    DeleteSubscriptionDefinition (DeleteSubscriptionDefinition'),
    newDeleteSubscriptionDefinition,
    DeleteSubscriptionDefinitionResponse (DeleteSubscriptionDefinitionResponse'),
    newDeleteSubscriptionDefinitionResponse,

    -- ** UpdateCoreDefinition
    UpdateCoreDefinition (UpdateCoreDefinition'),
    newUpdateCoreDefinition,
    UpdateCoreDefinitionResponse (UpdateCoreDefinitionResponse'),
    newUpdateCoreDefinitionResponse,

    -- ** UpdateSubscriptionDefinition
    UpdateSubscriptionDefinition (UpdateSubscriptionDefinition'),
    newUpdateSubscriptionDefinition,
    UpdateSubscriptionDefinitionResponse (UpdateSubscriptionDefinitionResponse'),
    newUpdateSubscriptionDefinitionResponse,

    -- ** AssociateServiceRoleToAccount
    AssociateServiceRoleToAccount (AssociateServiceRoleToAccount'),
    newAssociateServiceRoleToAccount,
    AssociateServiceRoleToAccountResponse (AssociateServiceRoleToAccountResponse'),
    newAssociateServiceRoleToAccountResponse,

    -- ** DeleteCoreDefinition
    DeleteCoreDefinition (DeleteCoreDefinition'),
    newDeleteCoreDefinition,
    DeleteCoreDefinitionResponse (DeleteCoreDefinitionResponse'),
    newDeleteCoreDefinitionResponse,

    -- ** AssociateRoleToGroup
    AssociateRoleToGroup (AssociateRoleToGroup'),
    newAssociateRoleToGroup,
    AssociateRoleToGroupResponse (AssociateRoleToGroupResponse'),
    newAssociateRoleToGroupResponse,

    -- ** GetGroupCertificateConfiguration
    GetGroupCertificateConfiguration (GetGroupCertificateConfiguration'),
    newGetGroupCertificateConfiguration,
    GetGroupCertificateConfigurationResponse (GetGroupCertificateConfigurationResponse'),
    newGetGroupCertificateConfigurationResponse,

    -- ** ListFunctionDefinitionVersions (Paginated)
    ListFunctionDefinitionVersions (ListFunctionDefinitionVersions'),
    newListFunctionDefinitionVersions,
    ListFunctionDefinitionVersionsResponse (ListFunctionDefinitionVersionsResponse'),
    newListFunctionDefinitionVersionsResponse,

    -- ** StartBulkDeployment
    StartBulkDeployment (StartBulkDeployment'),
    newStartBulkDeployment,
    StartBulkDeploymentResponse (StartBulkDeploymentResponse'),
    newStartBulkDeploymentResponse,

    -- ** CreateFunctionDefinitionVersion
    CreateFunctionDefinitionVersion (CreateFunctionDefinitionVersion'),
    newCreateFunctionDefinitionVersion,
    CreateFunctionDefinitionVersionResponse (CreateFunctionDefinitionVersionResponse'),
    newCreateFunctionDefinitionVersionResponse,

    -- ** GetFunctionDefinition
    GetFunctionDefinition (GetFunctionDefinition'),
    newGetFunctionDefinition,
    GetFunctionDefinitionResponse (GetFunctionDefinitionResponse'),
    newGetFunctionDefinitionResponse,

    -- ** UpdateThingRuntimeConfiguration
    UpdateThingRuntimeConfiguration (UpdateThingRuntimeConfiguration'),
    newUpdateThingRuntimeConfiguration,
    UpdateThingRuntimeConfigurationResponse (UpdateThingRuntimeConfigurationResponse'),
    newUpdateThingRuntimeConfigurationResponse,

    -- ** StopBulkDeployment
    StopBulkDeployment (StopBulkDeployment'),
    newStopBulkDeployment,
    StopBulkDeploymentResponse (StopBulkDeploymentResponse'),
    newStopBulkDeploymentResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** DeleteResourceDefinition
    DeleteResourceDefinition (DeleteResourceDefinition'),
    newDeleteResourceDefinition,
    DeleteResourceDefinitionResponse (DeleteResourceDefinitionResponse'),
    newDeleteResourceDefinitionResponse,

    -- ** GetThingRuntimeConfiguration
    GetThingRuntimeConfiguration (GetThingRuntimeConfiguration'),
    newGetThingRuntimeConfiguration,
    GetThingRuntimeConfigurationResponse (GetThingRuntimeConfigurationResponse'),
    newGetThingRuntimeConfigurationResponse,

    -- ** ListDeviceDefinitionVersions (Paginated)
    ListDeviceDefinitionVersions (ListDeviceDefinitionVersions'),
    newListDeviceDefinitionVersions,
    ListDeviceDefinitionVersionsResponse (ListDeviceDefinitionVersionsResponse'),
    newListDeviceDefinitionVersionsResponse,

    -- ** UpdateResourceDefinition
    UpdateResourceDefinition (UpdateResourceDefinition'),
    newUpdateResourceDefinition,
    UpdateResourceDefinitionResponse (UpdateResourceDefinitionResponse'),
    newUpdateResourceDefinitionResponse,

    -- ** ListResourceDefinitions (Paginated)
    ListResourceDefinitions (ListResourceDefinitions'),
    newListResourceDefinitions,
    ListResourceDefinitionsResponse (ListResourceDefinitionsResponse'),
    newListResourceDefinitionsResponse,

    -- ** GetDeviceDefinition
    GetDeviceDefinition (GetDeviceDefinition'),
    newGetDeviceDefinition,
    GetDeviceDefinitionResponse (GetDeviceDefinitionResponse'),
    newGetDeviceDefinitionResponse,

    -- ** CreateResourceDefinition
    CreateResourceDefinition (CreateResourceDefinition'),
    newCreateResourceDefinition,
    CreateResourceDefinitionResponse (CreateResourceDefinitionResponse'),
    newCreateResourceDefinitionResponse,

    -- ** GetResourceDefinitionVersion
    GetResourceDefinitionVersion (GetResourceDefinitionVersion'),
    newGetResourceDefinitionVersion,
    GetResourceDefinitionVersionResponse (GetResourceDefinitionVersionResponse'),
    newGetResourceDefinitionVersionResponse,

    -- ** CreateDeviceDefinitionVersion
    CreateDeviceDefinitionVersion (CreateDeviceDefinitionVersion'),
    newCreateDeviceDefinitionVersion,
    CreateDeviceDefinitionVersionResponse (CreateDeviceDefinitionVersionResponse'),
    newCreateDeviceDefinitionVersionResponse,

    -- ** GetGroupVersion
    GetGroupVersion (GetGroupVersion'),
    newGetGroupVersion,
    GetGroupVersionResponse (GetGroupVersionResponse'),
    newGetGroupVersionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** UpdateGroupCertificateConfiguration
    UpdateGroupCertificateConfiguration (UpdateGroupCertificateConfiguration'),
    newUpdateGroupCertificateConfiguration,
    UpdateGroupCertificateConfigurationResponse (UpdateGroupCertificateConfigurationResponse'),
    newUpdateGroupCertificateConfigurationResponse,

    -- ** DeleteLoggerDefinition
    DeleteLoggerDefinition (DeleteLoggerDefinition'),
    newDeleteLoggerDefinition,
    DeleteLoggerDefinitionResponse (DeleteLoggerDefinitionResponse'),
    newDeleteLoggerDefinitionResponse,

    -- ** ListBulkDeployments (Paginated)
    ListBulkDeployments (ListBulkDeployments'),
    newListBulkDeployments,
    ListBulkDeploymentsResponse (ListBulkDeploymentsResponse'),
    newListBulkDeploymentsResponse,

    -- ** CreateConnectorDefinitionVersion
    CreateConnectorDefinitionVersion (CreateConnectorDefinitionVersion'),
    newCreateConnectorDefinitionVersion,
    CreateConnectorDefinitionVersionResponse (CreateConnectorDefinitionVersionResponse'),
    newCreateConnectorDefinitionVersionResponse,

    -- ** CreateSubscriptionDefinitionVersion
    CreateSubscriptionDefinitionVersion (CreateSubscriptionDefinitionVersion'),
    newCreateSubscriptionDefinitionVersion,
    CreateSubscriptionDefinitionVersionResponse (CreateSubscriptionDefinitionVersionResponse'),
    newCreateSubscriptionDefinitionVersionResponse,

    -- ** UpdateLoggerDefinition
    UpdateLoggerDefinition (UpdateLoggerDefinition'),
    newUpdateLoggerDefinition,
    UpdateLoggerDefinitionResponse (UpdateLoggerDefinitionResponse'),
    newUpdateLoggerDefinitionResponse,

    -- ** ListLoggerDefinitions (Paginated)
    ListLoggerDefinitions (ListLoggerDefinitions'),
    newListLoggerDefinitions,
    ListLoggerDefinitionsResponse (ListLoggerDefinitionsResponse'),
    newListLoggerDefinitionsResponse,

    -- ** CreateCoreDefinitionVersion
    CreateCoreDefinitionVersion (CreateCoreDefinitionVersion'),
    newCreateCoreDefinitionVersion,
    CreateCoreDefinitionVersionResponse (CreateCoreDefinitionVersionResponse'),
    newCreateCoreDefinitionVersionResponse,

    -- ** ListDeployments (Paginated)
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetConnectivityInfo
    GetConnectivityInfo (GetConnectivityInfo'),
    newGetConnectivityInfo,
    GetConnectivityInfoResponse (GetConnectivityInfoResponse'),
    newGetConnectivityInfoResponse,

    -- ** ListSubscriptionDefinitionVersions (Paginated)
    ListSubscriptionDefinitionVersions (ListSubscriptionDefinitionVersions'),
    newListSubscriptionDefinitionVersions,
    ListSubscriptionDefinitionVersionsResponse (ListSubscriptionDefinitionVersionsResponse'),
    newListSubscriptionDefinitionVersionsResponse,

    -- ** ListCoreDefinitionVersions (Paginated)
    ListCoreDefinitionVersions (ListCoreDefinitionVersions'),
    newListCoreDefinitionVersions,
    ListCoreDefinitionVersionsResponse (ListCoreDefinitionVersionsResponse'),
    newListCoreDefinitionVersionsResponse,

    -- ** ListConnectorDefinitionVersions (Paginated)
    ListConnectorDefinitionVersions (ListConnectorDefinitionVersions'),
    newListConnectorDefinitionVersions,
    ListConnectorDefinitionVersionsResponse (ListConnectorDefinitionVersionsResponse'),
    newListConnectorDefinitionVersionsResponse,

    -- ** CreateSoftwareUpdateJob
    CreateSoftwareUpdateJob (CreateSoftwareUpdateJob'),
    newCreateSoftwareUpdateJob,
    CreateSoftwareUpdateJobResponse (CreateSoftwareUpdateJobResponse'),
    newCreateSoftwareUpdateJobResponse,

    -- ** CreateCoreDefinition
    CreateCoreDefinition (CreateCoreDefinition'),
    newCreateCoreDefinition,
    CreateCoreDefinitionResponse (CreateCoreDefinitionResponse'),
    newCreateCoreDefinitionResponse,

    -- ** CreateConnectorDefinition
    CreateConnectorDefinition (CreateConnectorDefinition'),
    newCreateConnectorDefinition,
    CreateConnectorDefinitionResponse (CreateConnectorDefinitionResponse'),
    newCreateConnectorDefinitionResponse,

    -- ** GetAssociatedRole
    GetAssociatedRole (GetAssociatedRole'),
    newGetAssociatedRole,
    GetAssociatedRoleResponse (GetAssociatedRoleResponse'),
    newGetAssociatedRoleResponse,

    -- ** UpdateConnectivityInfo
    UpdateConnectivityInfo (UpdateConnectivityInfo'),
    newUpdateConnectivityInfo,
    UpdateConnectivityInfoResponse (UpdateConnectivityInfoResponse'),
    newUpdateConnectivityInfoResponse,

    -- ** CreateSubscriptionDefinition
    CreateSubscriptionDefinition (CreateSubscriptionDefinition'),
    newCreateSubscriptionDefinition,
    CreateSubscriptionDefinitionResponse (CreateSubscriptionDefinitionResponse'),
    newCreateSubscriptionDefinitionResponse,

    -- ** DisassociateRoleFromGroup
    DisassociateRoleFromGroup (DisassociateRoleFromGroup'),
    newDisassociateRoleFromGroup,
    DisassociateRoleFromGroupResponse (DisassociateRoleFromGroupResponse'),
    newDisassociateRoleFromGroupResponse,

    -- ** ListCoreDefinitions (Paginated)
    ListCoreDefinitions (ListCoreDefinitions'),
    newListCoreDefinitions,
    ListCoreDefinitionsResponse (ListCoreDefinitionsResponse'),
    newListCoreDefinitionsResponse,

    -- ** ListConnectorDefinitions (Paginated)
    ListConnectorDefinitions (ListConnectorDefinitions'),
    newListConnectorDefinitions,
    ListConnectorDefinitionsResponse (ListConnectorDefinitionsResponse'),
    newListConnectorDefinitionsResponse,

    -- ** CreateGroupCertificateAuthority
    CreateGroupCertificateAuthority (CreateGroupCertificateAuthority'),
    newCreateGroupCertificateAuthority,
    CreateGroupCertificateAuthorityResponse (CreateGroupCertificateAuthorityResponse'),
    newCreateGroupCertificateAuthorityResponse,

    -- ** ListGroupCertificateAuthorities
    ListGroupCertificateAuthorities (ListGroupCertificateAuthorities'),
    newListGroupCertificateAuthorities,
    ListGroupCertificateAuthoritiesResponse (ListGroupCertificateAuthoritiesResponse'),
    newListGroupCertificateAuthoritiesResponse,

    -- ** DeleteConnectorDefinition
    DeleteConnectorDefinition (DeleteConnectorDefinition'),
    newDeleteConnectorDefinition,
    DeleteConnectorDefinitionResponse (DeleteConnectorDefinitionResponse'),
    newDeleteConnectorDefinitionResponse,

    -- ** GetLoggerDefinition
    GetLoggerDefinition (GetLoggerDefinition'),
    newGetLoggerDefinition,
    GetLoggerDefinitionResponse (GetLoggerDefinitionResponse'),
    newGetLoggerDefinitionResponse,

    -- ** UpdateConnectorDefinition
    UpdateConnectorDefinition (UpdateConnectorDefinition'),
    newUpdateConnectorDefinition,
    UpdateConnectorDefinitionResponse (UpdateConnectorDefinitionResponse'),
    newUpdateConnectorDefinitionResponse,

    -- ** CreateLoggerDefinitionVersion
    CreateLoggerDefinitionVersion (CreateLoggerDefinitionVersion'),
    newCreateLoggerDefinitionVersion,
    CreateLoggerDefinitionVersionResponse (CreateLoggerDefinitionVersionResponse'),
    newCreateLoggerDefinitionVersionResponse,

    -- ** ResetDeployments
    ResetDeployments (ResetDeployments'),
    newResetDeployments,
    ResetDeploymentsResponse (ResetDeploymentsResponse'),
    newResetDeploymentsResponse,

    -- ** ListSubscriptionDefinitions (Paginated)
    ListSubscriptionDefinitions (ListSubscriptionDefinitions'),
    newListSubscriptionDefinitions,
    ListSubscriptionDefinitionsResponse (ListSubscriptionDefinitionsResponse'),
    newListSubscriptionDefinitionsResponse,

    -- ** ListGroupVersions (Paginated)
    ListGroupVersions (ListGroupVersions'),
    newListGroupVersions,
    ListGroupVersionsResponse (ListGroupVersionsResponse'),
    newListGroupVersionsResponse,

    -- ** DeleteDeviceDefinition
    DeleteDeviceDefinition (DeleteDeviceDefinition'),
    newDeleteDeviceDefinition,
    DeleteDeviceDefinitionResponse (DeleteDeviceDefinitionResponse'),
    newDeleteDeviceDefinitionResponse,

    -- ** UpdateDeviceDefinition
    UpdateDeviceDefinition (UpdateDeviceDefinition'),
    newUpdateDeviceDefinition,
    UpdateDeviceDefinitionResponse (UpdateDeviceDefinitionResponse'),
    newUpdateDeviceDefinitionResponse,

    -- ** ListDeviceDefinitions (Paginated)
    ListDeviceDefinitions (ListDeviceDefinitions'),
    newListDeviceDefinitions,
    ListDeviceDefinitionsResponse (ListDeviceDefinitionsResponse'),
    newListDeviceDefinitionsResponse,

    -- ** DisassociateServiceRoleFromAccount
    DisassociateServiceRoleFromAccount (DisassociateServiceRoleFromAccount'),
    newDisassociateServiceRoleFromAccount,
    DisassociateServiceRoleFromAccountResponse (DisassociateServiceRoleFromAccountResponse'),
    newDisassociateServiceRoleFromAccountResponse,

    -- ** ListResourceDefinitionVersions (Paginated)
    ListResourceDefinitionVersions (ListResourceDefinitionVersions'),
    newListResourceDefinitionVersions,
    ListResourceDefinitionVersionsResponse (ListResourceDefinitionVersionsResponse'),
    newListResourceDefinitionVersionsResponse,

    -- ** GetResourceDefinition
    GetResourceDefinition (GetResourceDefinition'),
    newGetResourceDefinition,
    GetResourceDefinitionResponse (GetResourceDefinitionResponse'),
    newGetResourceDefinitionResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** CreateDeviceDefinition
    CreateDeviceDefinition (CreateDeviceDefinition'),
    newCreateDeviceDefinition,
    CreateDeviceDefinitionResponse (CreateDeviceDefinitionResponse'),
    newCreateDeviceDefinitionResponse,

    -- ** GetDeviceDefinitionVersion
    GetDeviceDefinitionVersion (GetDeviceDefinitionVersion'),
    newGetDeviceDefinitionVersion,
    GetDeviceDefinitionVersionResponse (GetDeviceDefinitionVersionResponse'),
    newGetDeviceDefinitionVersionResponse,

    -- ** CreateResourceDefinitionVersion
    CreateResourceDefinitionVersion (CreateResourceDefinitionVersion'),
    newCreateResourceDefinitionVersion,
    CreateResourceDefinitionVersionResponse (CreateResourceDefinitionVersionResponse'),
    newCreateResourceDefinitionVersionResponse,

    -- ** CreateGroupVersion
    CreateGroupVersion (CreateGroupVersion'),
    newCreateGroupVersion,
    CreateGroupVersionResponse (CreateGroupVersionResponse'),
    newCreateGroupVersionResponse,

    -- ** ListBulkDeploymentDetailedReports (Paginated)
    ListBulkDeploymentDetailedReports (ListBulkDeploymentDetailedReports'),
    newListBulkDeploymentDetailedReports,
    ListBulkDeploymentDetailedReportsResponse (ListBulkDeploymentDetailedReportsResponse'),
    newListBulkDeploymentDetailedReportsResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteFunctionDefinition
    DeleteFunctionDefinition (DeleteFunctionDefinition'),
    newDeleteFunctionDefinition,
    DeleteFunctionDefinitionResponse (DeleteFunctionDefinitionResponse'),
    newDeleteFunctionDefinitionResponse,

    -- ** UpdateFunctionDefinition
    UpdateFunctionDefinition (UpdateFunctionDefinition'),
    newUpdateFunctionDefinition,
    UpdateFunctionDefinitionResponse (UpdateFunctionDefinitionResponse'),
    newUpdateFunctionDefinitionResponse,

    -- ** ListFunctionDefinitions (Paginated)
    ListFunctionDefinitions (ListFunctionDefinitions'),
    newListFunctionDefinitions,
    ListFunctionDefinitionsResponse (ListFunctionDefinitionsResponse'),
    newListFunctionDefinitionsResponse,

    -- ** GetFunctionDefinitionVersion
    GetFunctionDefinitionVersion (GetFunctionDefinitionVersion'),
    newGetFunctionDefinitionVersion,
    GetFunctionDefinitionVersionResponse (GetFunctionDefinitionVersionResponse'),
    newGetFunctionDefinitionVersionResponse,

    -- ** GetBulkDeploymentStatus
    GetBulkDeploymentStatus (GetBulkDeploymentStatus'),
    newGetBulkDeploymentStatus,
    GetBulkDeploymentStatusResponse (GetBulkDeploymentStatusResponse'),
    newGetBulkDeploymentStatusResponse,

    -- ** GetDeploymentStatus
    GetDeploymentStatus (GetDeploymentStatus'),
    newGetDeploymentStatus,
    GetDeploymentStatusResponse (GetDeploymentStatusResponse'),
    newGetDeploymentStatusResponse,

    -- ** CreateFunctionDefinition
    CreateFunctionDefinition (CreateFunctionDefinition'),
    newCreateFunctionDefinition,
    CreateFunctionDefinitionResponse (CreateFunctionDefinitionResponse'),
    newCreateFunctionDefinitionResponse,

    -- ** GetSubscriptionDefinition
    GetSubscriptionDefinition (GetSubscriptionDefinition'),
    newGetSubscriptionDefinition,
    GetSubscriptionDefinitionResponse (GetSubscriptionDefinitionResponse'),
    newGetSubscriptionDefinitionResponse,

    -- ** GetConnectorDefinition
    GetConnectorDefinition (GetConnectorDefinition'),
    newGetConnectorDefinition,
    GetConnectorDefinitionResponse (GetConnectorDefinitionResponse'),
    newGetConnectorDefinitionResponse,

    -- ** GetCoreDefinition
    GetCoreDefinition (GetCoreDefinition'),
    newGetCoreDefinition,
    GetCoreDefinitionResponse (GetCoreDefinitionResponse'),
    newGetCoreDefinitionResponse,

    -- ** GetGroupCertificateAuthority
    GetGroupCertificateAuthority (GetGroupCertificateAuthority'),
    newGetGroupCertificateAuthority,
    GetGroupCertificateAuthorityResponse (GetGroupCertificateAuthorityResponse'),
    newGetGroupCertificateAuthorityResponse,

    -- ** GetServiceRoleForAccount
    GetServiceRoleForAccount (GetServiceRoleForAccount'),
    newGetServiceRoleForAccount,
    GetServiceRoleForAccountResponse (GetServiceRoleForAccountResponse'),
    newGetServiceRoleForAccountResponse,

    -- ** GetLoggerDefinitionVersion
    GetLoggerDefinitionVersion (GetLoggerDefinitionVersion'),
    newGetLoggerDefinitionVersion,
    GetLoggerDefinitionVersionResponse (GetLoggerDefinitionVersionResponse'),
    newGetLoggerDefinitionVersionResponse,

    -- ** CreateLoggerDefinition
    CreateLoggerDefinition (CreateLoggerDefinition'),
    newCreateLoggerDefinition,
    CreateLoggerDefinitionResponse (CreateLoggerDefinitionResponse'),
    newCreateLoggerDefinitionResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetSubscriptionDefinitionVersion
    GetSubscriptionDefinitionVersion (GetSubscriptionDefinitionVersion'),
    newGetSubscriptionDefinitionVersion,
    GetSubscriptionDefinitionVersionResponse (GetSubscriptionDefinitionVersionResponse'),
    newGetSubscriptionDefinitionVersionResponse,

    -- ** ListLoggerDefinitionVersions (Paginated)
    ListLoggerDefinitionVersions (ListLoggerDefinitionVersions'),
    newListLoggerDefinitionVersions,
    ListLoggerDefinitionVersionsResponse (ListLoggerDefinitionVersionsResponse'),
    newListLoggerDefinitionVersionsResponse,

    -- ** GetCoreDefinitionVersion
    GetCoreDefinitionVersion (GetCoreDefinitionVersion'),
    newGetCoreDefinitionVersion,
    GetCoreDefinitionVersionResponse (GetCoreDefinitionVersionResponse'),
    newGetCoreDefinitionVersionResponse,

    -- ** GetConnectorDefinitionVersion
    GetConnectorDefinitionVersion (GetConnectorDefinitionVersion'),
    newGetConnectorDefinitionVersion,
    GetConnectorDefinitionVersionResponse (GetConnectorDefinitionVersionResponse'),
    newGetConnectorDefinitionVersionResponse,

    -- * Types

    -- ** BulkDeploymentStatus
    BulkDeploymentStatus (..),

    -- ** ConfigurationSyncStatus
    ConfigurationSyncStatus (..),

    -- ** DeploymentType
    DeploymentType (..),

    -- ** EncodingType
    EncodingType (..),

    -- ** FunctionIsolationMode
    FunctionIsolationMode (..),

    -- ** LoggerComponent
    LoggerComponent (..),

    -- ** LoggerLevel
    LoggerLevel (..),

    -- ** LoggerType
    LoggerType (..),

    -- ** Permission
    Permission (..),

    -- ** SoftwareToUpdate
    SoftwareToUpdate (..),

    -- ** Telemetry
    Telemetry (..),

    -- ** UpdateAgentLogLevel
    UpdateAgentLogLevel (..),

    -- ** UpdateTargetsArchitecture
    UpdateTargetsArchitecture (..),

    -- ** UpdateTargetsOperatingSystem
    UpdateTargetsOperatingSystem (..),

    -- ** BulkDeployment
    BulkDeployment (BulkDeployment'),
    newBulkDeployment,

    -- ** BulkDeploymentMetrics
    BulkDeploymentMetrics (BulkDeploymentMetrics'),
    newBulkDeploymentMetrics,

    -- ** BulkDeploymentResult
    BulkDeploymentResult (BulkDeploymentResult'),
    newBulkDeploymentResult,

    -- ** ConnectivityInfo
    ConnectivityInfo (ConnectivityInfo'),
    newConnectivityInfo,

    -- ** Connector
    Connector (Connector'),
    newConnector,

    -- ** ConnectorDefinitionVersion
    ConnectorDefinitionVersion (ConnectorDefinitionVersion'),
    newConnectorDefinitionVersion,

    -- ** Core
    Core (Core'),
    newCore,

    -- ** CoreDefinitionVersion
    CoreDefinitionVersion (CoreDefinitionVersion'),
    newCoreDefinitionVersion,

    -- ** DefinitionInformation
    DefinitionInformation (DefinitionInformation'),
    newDefinitionInformation,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** Device
    Device (Device'),
    newDevice,

    -- ** DeviceDefinitionVersion
    DeviceDefinitionVersion (DeviceDefinitionVersion'),
    newDeviceDefinitionVersion,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** Function
    Function (Function'),
    newFunction,

    -- ** FunctionConfiguration
    FunctionConfiguration (FunctionConfiguration'),
    newFunctionConfiguration,

    -- ** FunctionConfigurationEnvironment
    FunctionConfigurationEnvironment (FunctionConfigurationEnvironment'),
    newFunctionConfigurationEnvironment,

    -- ** FunctionDefaultConfig
    FunctionDefaultConfig (FunctionDefaultConfig'),
    newFunctionDefaultConfig,

    -- ** FunctionDefaultExecutionConfig
    FunctionDefaultExecutionConfig (FunctionDefaultExecutionConfig'),
    newFunctionDefaultExecutionConfig,

    -- ** FunctionDefinitionVersion
    FunctionDefinitionVersion (FunctionDefinitionVersion'),
    newFunctionDefinitionVersion,

    -- ** FunctionExecutionConfig
    FunctionExecutionConfig (FunctionExecutionConfig'),
    newFunctionExecutionConfig,

    -- ** FunctionRunAsConfig
    FunctionRunAsConfig (FunctionRunAsConfig'),
    newFunctionRunAsConfig,

    -- ** GreengrassLogger
    GreengrassLogger (GreengrassLogger'),
    newGreengrassLogger,

    -- ** GroupCertificateAuthorityProperties
    GroupCertificateAuthorityProperties (GroupCertificateAuthorityProperties'),
    newGroupCertificateAuthorityProperties,

    -- ** GroupInformation
    GroupInformation (GroupInformation'),
    newGroupInformation,

    -- ** GroupOwnerSetting
    GroupOwnerSetting (GroupOwnerSetting'),
    newGroupOwnerSetting,

    -- ** GroupVersion
    GroupVersion (GroupVersion'),
    newGroupVersion,

    -- ** LocalDeviceResourceData
    LocalDeviceResourceData (LocalDeviceResourceData'),
    newLocalDeviceResourceData,

    -- ** LocalVolumeResourceData
    LocalVolumeResourceData (LocalVolumeResourceData'),
    newLocalVolumeResourceData,

    -- ** LoggerDefinitionVersion
    LoggerDefinitionVersion (LoggerDefinitionVersion'),
    newLoggerDefinitionVersion,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceAccessPolicy
    ResourceAccessPolicy (ResourceAccessPolicy'),
    newResourceAccessPolicy,

    -- ** ResourceDataContainer
    ResourceDataContainer (ResourceDataContainer'),
    newResourceDataContainer,

    -- ** ResourceDefinitionVersion
    ResourceDefinitionVersion (ResourceDefinitionVersion'),
    newResourceDefinitionVersion,

    -- ** ResourceDownloadOwnerSetting
    ResourceDownloadOwnerSetting (ResourceDownloadOwnerSetting'),
    newResourceDownloadOwnerSetting,

    -- ** RuntimeConfiguration
    RuntimeConfiguration (RuntimeConfiguration'),
    newRuntimeConfiguration,

    -- ** S3MachineLearningModelResourceData
    S3MachineLearningModelResourceData (S3MachineLearningModelResourceData'),
    newS3MachineLearningModelResourceData,

    -- ** SageMakerMachineLearningModelResourceData
    SageMakerMachineLearningModelResourceData (SageMakerMachineLearningModelResourceData'),
    newSageMakerMachineLearningModelResourceData,

    -- ** SecretsManagerSecretResourceData
    SecretsManagerSecretResourceData (SecretsManagerSecretResourceData'),
    newSecretsManagerSecretResourceData,

    -- ** Subscription
    Subscription (Subscription'),
    newSubscription,

    -- ** SubscriptionDefinitionVersion
    SubscriptionDefinitionVersion (SubscriptionDefinitionVersion'),
    newSubscriptionDefinitionVersion,

    -- ** TelemetryConfiguration
    TelemetryConfiguration (TelemetryConfiguration'),
    newTelemetryConfiguration,

    -- ** TelemetryConfigurationUpdate
    TelemetryConfigurationUpdate (TelemetryConfigurationUpdate'),
    newTelemetryConfigurationUpdate,

    -- ** VersionInformation
    VersionInformation (VersionInformation'),
    newVersionInformation,
  )
where

import Network.AWS.Greengrass.AssociateRoleToGroup
import Network.AWS.Greengrass.AssociateServiceRoleToAccount
import Network.AWS.Greengrass.CreateConnectorDefinition
import Network.AWS.Greengrass.CreateConnectorDefinitionVersion
import Network.AWS.Greengrass.CreateCoreDefinition
import Network.AWS.Greengrass.CreateCoreDefinitionVersion
import Network.AWS.Greengrass.CreateDeployment
import Network.AWS.Greengrass.CreateDeviceDefinition
import Network.AWS.Greengrass.CreateDeviceDefinitionVersion
import Network.AWS.Greengrass.CreateFunctionDefinition
import Network.AWS.Greengrass.CreateFunctionDefinitionVersion
import Network.AWS.Greengrass.CreateGroup
import Network.AWS.Greengrass.CreateGroupCertificateAuthority
import Network.AWS.Greengrass.CreateGroupVersion
import Network.AWS.Greengrass.CreateLoggerDefinition
import Network.AWS.Greengrass.CreateLoggerDefinitionVersion
import Network.AWS.Greengrass.CreateResourceDefinition
import Network.AWS.Greengrass.CreateResourceDefinitionVersion
import Network.AWS.Greengrass.CreateSoftwareUpdateJob
import Network.AWS.Greengrass.CreateSubscriptionDefinition
import Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
import Network.AWS.Greengrass.DeleteConnectorDefinition
import Network.AWS.Greengrass.DeleteCoreDefinition
import Network.AWS.Greengrass.DeleteDeviceDefinition
import Network.AWS.Greengrass.DeleteFunctionDefinition
import Network.AWS.Greengrass.DeleteGroup
import Network.AWS.Greengrass.DeleteLoggerDefinition
import Network.AWS.Greengrass.DeleteResourceDefinition
import Network.AWS.Greengrass.DeleteSubscriptionDefinition
import Network.AWS.Greengrass.DisassociateRoleFromGroup
import Network.AWS.Greengrass.DisassociateServiceRoleFromAccount
import Network.AWS.Greengrass.GetAssociatedRole
import Network.AWS.Greengrass.GetBulkDeploymentStatus
import Network.AWS.Greengrass.GetConnectivityInfo
import Network.AWS.Greengrass.GetConnectorDefinition
import Network.AWS.Greengrass.GetConnectorDefinitionVersion
import Network.AWS.Greengrass.GetCoreDefinition
import Network.AWS.Greengrass.GetCoreDefinitionVersion
import Network.AWS.Greengrass.GetDeploymentStatus
import Network.AWS.Greengrass.GetDeviceDefinition
import Network.AWS.Greengrass.GetDeviceDefinitionVersion
import Network.AWS.Greengrass.GetFunctionDefinition
import Network.AWS.Greengrass.GetFunctionDefinitionVersion
import Network.AWS.Greengrass.GetGroup
import Network.AWS.Greengrass.GetGroupCertificateAuthority
import Network.AWS.Greengrass.GetGroupCertificateConfiguration
import Network.AWS.Greengrass.GetGroupVersion
import Network.AWS.Greengrass.GetLoggerDefinition
import Network.AWS.Greengrass.GetLoggerDefinitionVersion
import Network.AWS.Greengrass.GetResourceDefinition
import Network.AWS.Greengrass.GetResourceDefinitionVersion
import Network.AWS.Greengrass.GetServiceRoleForAccount
import Network.AWS.Greengrass.GetSubscriptionDefinition
import Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
import Network.AWS.Greengrass.GetThingRuntimeConfiguration
import Network.AWS.Greengrass.Lens
import Network.AWS.Greengrass.ListBulkDeploymentDetailedReports
import Network.AWS.Greengrass.ListBulkDeployments
import Network.AWS.Greengrass.ListConnectorDefinitionVersions
import Network.AWS.Greengrass.ListConnectorDefinitions
import Network.AWS.Greengrass.ListCoreDefinitionVersions
import Network.AWS.Greengrass.ListCoreDefinitions
import Network.AWS.Greengrass.ListDeployments
import Network.AWS.Greengrass.ListDeviceDefinitionVersions
import Network.AWS.Greengrass.ListDeviceDefinitions
import Network.AWS.Greengrass.ListFunctionDefinitionVersions
import Network.AWS.Greengrass.ListFunctionDefinitions
import Network.AWS.Greengrass.ListGroupCertificateAuthorities
import Network.AWS.Greengrass.ListGroupVersions
import Network.AWS.Greengrass.ListGroups
import Network.AWS.Greengrass.ListLoggerDefinitionVersions
import Network.AWS.Greengrass.ListLoggerDefinitions
import Network.AWS.Greengrass.ListResourceDefinitionVersions
import Network.AWS.Greengrass.ListResourceDefinitions
import Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
import Network.AWS.Greengrass.ListSubscriptionDefinitions
import Network.AWS.Greengrass.ListTagsForResource
import Network.AWS.Greengrass.ResetDeployments
import Network.AWS.Greengrass.StartBulkDeployment
import Network.AWS.Greengrass.StopBulkDeployment
import Network.AWS.Greengrass.TagResource
import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.UntagResource
import Network.AWS.Greengrass.UpdateConnectivityInfo
import Network.AWS.Greengrass.UpdateConnectorDefinition
import Network.AWS.Greengrass.UpdateCoreDefinition
import Network.AWS.Greengrass.UpdateDeviceDefinition
import Network.AWS.Greengrass.UpdateFunctionDefinition
import Network.AWS.Greengrass.UpdateGroup
import Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
import Network.AWS.Greengrass.UpdateLoggerDefinition
import Network.AWS.Greengrass.UpdateResourceDefinition
import Network.AWS.Greengrass.UpdateSubscriptionDefinition
import Network.AWS.Greengrass.UpdateThingRuntimeConfiguration
import Network.AWS.Greengrass.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Greengrass'.

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
