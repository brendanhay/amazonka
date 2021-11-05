{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Greengrass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-06-07@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Greengrass seamlessly extends AWS onto physical devices so they
-- can act locally on the data they generate, while still using the cloud
-- for management, analytics, and durable storage. AWS IoT Greengrass
-- ensures your devices can respond quickly to local events and operate
-- with intermittent connectivity. AWS IoT Greengrass minimizes the cost of
-- transmitting data to the cloud by allowing you to author AWS Lambda
-- functions that execute locally.
module Amazonka.Greengrass
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetGroupCertificateConfiguration
    GetGroupCertificateConfiguration (GetGroupCertificateConfiguration'),
    newGetGroupCertificateConfiguration,
    GetGroupCertificateConfigurationResponse (GetGroupCertificateConfigurationResponse'),
    newGetGroupCertificateConfigurationResponse,

    -- ** ListGroupVersions (Paginated)
    ListGroupVersions (ListGroupVersions'),
    newListGroupVersions,
    ListGroupVersionsResponse (ListGroupVersionsResponse'),
    newListGroupVersionsResponse,

    -- ** ListFunctionDefinitionVersions (Paginated)
    ListFunctionDefinitionVersions (ListFunctionDefinitionVersions'),
    newListFunctionDefinitionVersions,
    ListFunctionDefinitionVersionsResponse (ListFunctionDefinitionVersionsResponse'),
    newListFunctionDefinitionVersionsResponse,

    -- ** ListDeviceDefinitions (Paginated)
    ListDeviceDefinitions (ListDeviceDefinitions'),
    newListDeviceDefinitions,
    ListDeviceDefinitionsResponse (ListDeviceDefinitionsResponse'),
    newListDeviceDefinitionsResponse,

    -- ** AssociateRoleToGroup
    AssociateRoleToGroup (AssociateRoleToGroup'),
    newAssociateRoleToGroup,
    AssociateRoleToGroupResponse (AssociateRoleToGroupResponse'),
    newAssociateRoleToGroupResponse,

    -- ** UpdateCoreDefinition
    UpdateCoreDefinition (UpdateCoreDefinition'),
    newUpdateCoreDefinition,
    UpdateCoreDefinitionResponse (UpdateCoreDefinitionResponse'),
    newUpdateCoreDefinitionResponse,

    -- ** DeleteCoreDefinition
    DeleteCoreDefinition (DeleteCoreDefinition'),
    newDeleteCoreDefinition,
    DeleteCoreDefinitionResponse (DeleteCoreDefinitionResponse'),
    newDeleteCoreDefinitionResponse,

    -- ** GetLoggerDefinition
    GetLoggerDefinition (GetLoggerDefinition'),
    newGetLoggerDefinition,
    GetLoggerDefinitionResponse (GetLoggerDefinitionResponse'),
    newGetLoggerDefinitionResponse,

    -- ** ListGroupCertificateAuthorities
    ListGroupCertificateAuthorities (ListGroupCertificateAuthorities'),
    newListGroupCertificateAuthorities,
    ListGroupCertificateAuthoritiesResponse (ListGroupCertificateAuthoritiesResponse'),
    newListGroupCertificateAuthoritiesResponse,

    -- ** DisassociateRoleFromGroup
    DisassociateRoleFromGroup (DisassociateRoleFromGroup'),
    newDisassociateRoleFromGroup,
    DisassociateRoleFromGroupResponse (DisassociateRoleFromGroupResponse'),
    newDisassociateRoleFromGroupResponse,

    -- ** UpdateSubscriptionDefinition
    UpdateSubscriptionDefinition (UpdateSubscriptionDefinition'),
    newUpdateSubscriptionDefinition,
    UpdateSubscriptionDefinitionResponse (UpdateSubscriptionDefinitionResponse'),
    newUpdateSubscriptionDefinitionResponse,

    -- ** DeleteSubscriptionDefinition
    DeleteSubscriptionDefinition (DeleteSubscriptionDefinition'),
    newDeleteSubscriptionDefinition,
    DeleteSubscriptionDefinitionResponse (DeleteSubscriptionDefinitionResponse'),
    newDeleteSubscriptionDefinitionResponse,

    -- ** ListCoreDefinitions (Paginated)
    ListCoreDefinitions (ListCoreDefinitions'),
    newListCoreDefinitions,
    ListCoreDefinitionsResponse (ListCoreDefinitionsResponse'),
    newListCoreDefinitionsResponse,

    -- ** ListSubscriptionDefinitions (Paginated)
    ListSubscriptionDefinitions (ListSubscriptionDefinitions'),
    newListSubscriptionDefinitions,
    ListSubscriptionDefinitionsResponse (ListSubscriptionDefinitionsResponse'),
    newListSubscriptionDefinitionsResponse,

    -- ** CreateGroupCertificateAuthority
    CreateGroupCertificateAuthority (CreateGroupCertificateAuthority'),
    newCreateGroupCertificateAuthority,
    CreateGroupCertificateAuthorityResponse (CreateGroupCertificateAuthorityResponse'),
    newCreateGroupCertificateAuthorityResponse,

    -- ** DeleteConnectorDefinition
    DeleteConnectorDefinition (DeleteConnectorDefinition'),
    newDeleteConnectorDefinition,
    DeleteConnectorDefinitionResponse (DeleteConnectorDefinitionResponse'),
    newDeleteConnectorDefinitionResponse,

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

    -- ** CreateCoreDefinition
    CreateCoreDefinition (CreateCoreDefinition'),
    newCreateCoreDefinition,
    CreateCoreDefinitionResponse (CreateCoreDefinitionResponse'),
    newCreateCoreDefinitionResponse,

    -- ** GetConnectorDefinitionVersion
    GetConnectorDefinitionVersion (GetConnectorDefinitionVersion'),
    newGetConnectorDefinitionVersion,
    GetConnectorDefinitionVersionResponse (GetConnectorDefinitionVersionResponse'),
    newGetConnectorDefinitionVersionResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetGroupCertificateAuthority
    GetGroupCertificateAuthority (GetGroupCertificateAuthority'),
    newGetGroupCertificateAuthority,
    GetGroupCertificateAuthorityResponse (GetGroupCertificateAuthorityResponse'),
    newGetGroupCertificateAuthorityResponse,

    -- ** GetLoggerDefinitionVersion
    GetLoggerDefinitionVersion (GetLoggerDefinitionVersion'),
    newGetLoggerDefinitionVersion,
    GetLoggerDefinitionVersionResponse (GetLoggerDefinitionVersionResponse'),
    newGetLoggerDefinitionVersionResponse,

    -- ** GetServiceRoleForAccount
    GetServiceRoleForAccount (GetServiceRoleForAccount'),
    newGetServiceRoleForAccount,
    GetServiceRoleForAccountResponse (GetServiceRoleForAccountResponse'),
    newGetServiceRoleForAccountResponse,

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

    -- ** CreateLoggerDefinition
    CreateLoggerDefinition (CreateLoggerDefinition'),
    newCreateLoggerDefinition,
    CreateLoggerDefinitionResponse (CreateLoggerDefinitionResponse'),
    newCreateLoggerDefinitionResponse,

    -- ** GetConnectivityInfo
    GetConnectivityInfo (GetConnectivityInfo'),
    newGetConnectivityInfo,
    GetConnectivityInfoResponse (GetConnectivityInfoResponse'),
    newGetConnectivityInfoResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** DeleteLoggerDefinition
    DeleteLoggerDefinition (DeleteLoggerDefinition'),
    newDeleteLoggerDefinition,
    DeleteLoggerDefinitionResponse (DeleteLoggerDefinitionResponse'),
    newDeleteLoggerDefinitionResponse,

    -- ** UpdateLoggerDefinition
    UpdateLoggerDefinition (UpdateLoggerDefinition'),
    newUpdateLoggerDefinition,
    UpdateLoggerDefinitionResponse (UpdateLoggerDefinitionResponse'),
    newUpdateLoggerDefinitionResponse,

    -- ** GetSubscriptionDefinition
    GetSubscriptionDefinition (GetSubscriptionDefinition'),
    newGetSubscriptionDefinition,
    GetSubscriptionDefinitionResponse (GetSubscriptionDefinitionResponse'),
    newGetSubscriptionDefinitionResponse,

    -- ** GetCoreDefinition
    GetCoreDefinition (GetCoreDefinition'),
    newGetCoreDefinition,
    GetCoreDefinitionResponse (GetCoreDefinitionResponse'),
    newGetCoreDefinitionResponse,

    -- ** CreateConnectorDefinitionVersion
    CreateConnectorDefinitionVersion (CreateConnectorDefinitionVersion'),
    newCreateConnectorDefinitionVersion,
    CreateConnectorDefinitionVersionResponse (CreateConnectorDefinitionVersionResponse'),
    newCreateConnectorDefinitionVersionResponse,

    -- ** GetDeploymentStatus
    GetDeploymentStatus (GetDeploymentStatus'),
    newGetDeploymentStatus,
    GetDeploymentStatusResponse (GetDeploymentStatusResponse'),
    newGetDeploymentStatusResponse,

    -- ** GetBulkDeploymentStatus
    GetBulkDeploymentStatus (GetBulkDeploymentStatus'),
    newGetBulkDeploymentStatus,
    GetBulkDeploymentStatusResponse (GetBulkDeploymentStatusResponse'),
    newGetBulkDeploymentStatusResponse,

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

    -- ** UpdateFunctionDefinition
    UpdateFunctionDefinition (UpdateFunctionDefinition'),
    newUpdateFunctionDefinition,
    UpdateFunctionDefinitionResponse (UpdateFunctionDefinitionResponse'),
    newUpdateFunctionDefinitionResponse,

    -- ** DeleteFunctionDefinition
    DeleteFunctionDefinition (DeleteFunctionDefinition'),
    newDeleteFunctionDefinition,
    DeleteFunctionDefinitionResponse (DeleteFunctionDefinitionResponse'),
    newDeleteFunctionDefinitionResponse,

    -- ** ListResourceDefinitions (Paginated)
    ListResourceDefinitions (ListResourceDefinitions'),
    newListResourceDefinitions,
    ListResourceDefinitionsResponse (ListResourceDefinitionsResponse'),
    newListResourceDefinitionsResponse,

    -- ** StopBulkDeployment
    StopBulkDeployment (StopBulkDeployment'),
    newStopBulkDeployment,
    StopBulkDeploymentResponse (StopBulkDeploymentResponse'),
    newStopBulkDeploymentResponse,

    -- ** CreateResourceDefinitionVersion
    CreateResourceDefinitionVersion (CreateResourceDefinitionVersion'),
    newCreateResourceDefinitionVersion,
    CreateResourceDefinitionVersionResponse (CreateResourceDefinitionVersionResponse'),
    newCreateResourceDefinitionVersionResponse,

    -- ** GetResourceDefinition
    GetResourceDefinition (GetResourceDefinition'),
    newGetResourceDefinition,
    GetResourceDefinitionResponse (GetResourceDefinitionResponse'),
    newGetResourceDefinitionResponse,

    -- ** ListResourceDefinitionVersions (Paginated)
    ListResourceDefinitionVersions (ListResourceDefinitionVersions'),
    newListResourceDefinitionVersions,
    ListResourceDefinitionVersionsResponse (ListResourceDefinitionVersionsResponse'),
    newListResourceDefinitionVersionsResponse,

    -- ** DisassociateServiceRoleFromAccount
    DisassociateServiceRoleFromAccount (DisassociateServiceRoleFromAccount'),
    newDisassociateServiceRoleFromAccount,
    DisassociateServiceRoleFromAccountResponse (DisassociateServiceRoleFromAccountResponse'),
    newDisassociateServiceRoleFromAccountResponse,

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

    -- ** AssociateServiceRoleToAccount
    AssociateServiceRoleToAccount (AssociateServiceRoleToAccount'),
    newAssociateServiceRoleToAccount,
    AssociateServiceRoleToAccountResponse (AssociateServiceRoleToAccountResponse'),
    newAssociateServiceRoleToAccountResponse,

    -- ** ResetDeployments
    ResetDeployments (ResetDeployments'),
    newResetDeployments,
    ResetDeploymentsResponse (ResetDeploymentsResponse'),
    newResetDeploymentsResponse,

    -- ** ListConnectorDefinitions (Paginated)
    ListConnectorDefinitions (ListConnectorDefinitions'),
    newListConnectorDefinitions,
    ListConnectorDefinitionsResponse (ListConnectorDefinitionsResponse'),
    newListConnectorDefinitionsResponse,

    -- ** GetSubscriptionDefinitionVersion
    GetSubscriptionDefinitionVersion (GetSubscriptionDefinitionVersion'),
    newGetSubscriptionDefinitionVersion,
    GetSubscriptionDefinitionVersionResponse (GetSubscriptionDefinitionVersionResponse'),
    newGetSubscriptionDefinitionVersionResponse,

    -- ** GetAssociatedRole
    GetAssociatedRole (GetAssociatedRole'),
    newGetAssociatedRole,
    GetAssociatedRoleResponse (GetAssociatedRoleResponse'),
    newGetAssociatedRoleResponse,

    -- ** ListLoggerDefinitionVersions (Paginated)
    ListLoggerDefinitionVersions (ListLoggerDefinitionVersions'),
    newListLoggerDefinitionVersions,
    ListLoggerDefinitionVersionsResponse (ListLoggerDefinitionVersionsResponse'),
    newListLoggerDefinitionVersionsResponse,

    -- ** CreateConnectorDefinition
    CreateConnectorDefinition (CreateConnectorDefinition'),
    newCreateConnectorDefinition,
    CreateConnectorDefinitionResponse (CreateConnectorDefinitionResponse'),
    newCreateConnectorDefinitionResponse,

    -- ** GetCoreDefinitionVersion
    GetCoreDefinitionVersion (GetCoreDefinitionVersion'),
    newGetCoreDefinitionVersion,
    GetCoreDefinitionVersionResponse (GetCoreDefinitionVersionResponse'),
    newGetCoreDefinitionVersionResponse,

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

    -- ** CreateCoreDefinitionVersion
    CreateCoreDefinitionVersion (CreateCoreDefinitionVersion'),
    newCreateCoreDefinitionVersion,
    CreateCoreDefinitionVersionResponse (CreateCoreDefinitionVersionResponse'),
    newCreateCoreDefinitionVersionResponse,

    -- ** ListBulkDeployments (Paginated)
    ListBulkDeployments (ListBulkDeployments'),
    newListBulkDeployments,
    ListBulkDeploymentsResponse (ListBulkDeploymentsResponse'),
    newListBulkDeploymentsResponse,

    -- ** ListDeployments (Paginated)
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** GetConnectorDefinition
    GetConnectorDefinition (GetConnectorDefinition'),
    newGetConnectorDefinition,
    GetConnectorDefinitionResponse (GetConnectorDefinitionResponse'),
    newGetConnectorDefinitionResponse,

    -- ** ListLoggerDefinitions (Paginated)
    ListLoggerDefinitions (ListLoggerDefinitions'),
    newListLoggerDefinitions,
    ListLoggerDefinitionsResponse (ListLoggerDefinitionsResponse'),
    newListLoggerDefinitionsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateSubscriptionDefinitionVersion
    CreateSubscriptionDefinitionVersion (CreateSubscriptionDefinitionVersion'),
    newCreateSubscriptionDefinitionVersion,
    CreateSubscriptionDefinitionVersionResponse (CreateSubscriptionDefinitionVersionResponse'),
    newCreateSubscriptionDefinitionVersionResponse,

    -- ** GetGroupVersion
    GetGroupVersion (GetGroupVersion'),
    newGetGroupVersion,
    GetGroupVersionResponse (GetGroupVersionResponse'),
    newGetGroupVersionResponse,

    -- ** UpdateGroupCertificateConfiguration
    UpdateGroupCertificateConfiguration (UpdateGroupCertificateConfiguration'),
    newUpdateGroupCertificateConfiguration,
    UpdateGroupCertificateConfigurationResponse (UpdateGroupCertificateConfigurationResponse'),
    newUpdateGroupCertificateConfigurationResponse,

    -- ** GetFunctionDefinitionVersion
    GetFunctionDefinitionVersion (GetFunctionDefinitionVersion'),
    newGetFunctionDefinitionVersion,
    GetFunctionDefinitionVersionResponse (GetFunctionDefinitionVersionResponse'),
    newGetFunctionDefinitionVersionResponse,

    -- ** GetDeviceDefinition
    GetDeviceDefinition (GetDeviceDefinition'),
    newGetDeviceDefinition,
    GetDeviceDefinitionResponse (GetDeviceDefinitionResponse'),
    newGetDeviceDefinitionResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateFunctionDefinition
    CreateFunctionDefinition (CreateFunctionDefinition'),
    newCreateFunctionDefinition,
    CreateFunctionDefinitionResponse (CreateFunctionDefinitionResponse'),
    newCreateFunctionDefinitionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateDeviceDefinitionVersion
    CreateDeviceDefinitionVersion (CreateDeviceDefinitionVersion'),
    newCreateDeviceDefinitionVersion,
    CreateDeviceDefinitionVersionResponse (CreateDeviceDefinitionVersionResponse'),
    newCreateDeviceDefinitionVersionResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListBulkDeploymentDetailedReports (Paginated)
    ListBulkDeploymentDetailedReports (ListBulkDeploymentDetailedReports'),
    newListBulkDeploymentDetailedReports,
    ListBulkDeploymentDetailedReportsResponse (ListBulkDeploymentDetailedReportsResponse'),
    newListBulkDeploymentDetailedReportsResponse,

    -- ** GetThingRuntimeConfiguration
    GetThingRuntimeConfiguration (GetThingRuntimeConfiguration'),
    newGetThingRuntimeConfiguration,
    GetThingRuntimeConfigurationResponse (GetThingRuntimeConfigurationResponse'),
    newGetThingRuntimeConfigurationResponse,

    -- ** DeleteResourceDefinition
    DeleteResourceDefinition (DeleteResourceDefinition'),
    newDeleteResourceDefinition,
    DeleteResourceDefinitionResponse (DeleteResourceDefinitionResponse'),
    newDeleteResourceDefinitionResponse,

    -- ** UpdateResourceDefinition
    UpdateResourceDefinition (UpdateResourceDefinition'),
    newUpdateResourceDefinition,
    UpdateResourceDefinitionResponse (UpdateResourceDefinitionResponse'),
    newUpdateResourceDefinitionResponse,

    -- ** ListDeviceDefinitionVersions (Paginated)
    ListDeviceDefinitionVersions (ListDeviceDefinitionVersions'),
    newListDeviceDefinitionVersions,
    ListDeviceDefinitionVersionsResponse (ListDeviceDefinitionVersionsResponse'),
    newListDeviceDefinitionVersionsResponse,

    -- ** ListFunctionDefinitions (Paginated)
    ListFunctionDefinitions (ListFunctionDefinitions'),
    newListFunctionDefinitions,
    ListFunctionDefinitionsResponse (ListFunctionDefinitionsResponse'),
    newListFunctionDefinitionsResponse,

    -- ** GetFunctionDefinition
    GetFunctionDefinition (GetFunctionDefinition'),
    newGetFunctionDefinition,
    GetFunctionDefinitionResponse (GetFunctionDefinitionResponse'),
    newGetFunctionDefinitionResponse,

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

    -- ** CreateGroupVersion
    CreateGroupVersion (CreateGroupVersion'),
    newCreateGroupVersion,
    CreateGroupVersionResponse (CreateGroupVersionResponse'),
    newCreateGroupVersionResponse,

    -- ** CreateFunctionDefinitionVersion
    CreateFunctionDefinitionVersion (CreateFunctionDefinitionVersion'),
    newCreateFunctionDefinitionVersion,
    CreateFunctionDefinitionVersionResponse (CreateFunctionDefinitionVersionResponse'),
    newCreateFunctionDefinitionVersionResponse,

    -- ** StartBulkDeployment
    StartBulkDeployment (StartBulkDeployment'),
    newStartBulkDeployment,
    StartBulkDeploymentResponse (StartBulkDeploymentResponse'),
    newStartBulkDeploymentResponse,

    -- ** UpdateThingRuntimeConfiguration
    UpdateThingRuntimeConfiguration (UpdateThingRuntimeConfiguration'),
    newUpdateThingRuntimeConfiguration,
    UpdateThingRuntimeConfigurationResponse (UpdateThingRuntimeConfigurationResponse'),
    newUpdateThingRuntimeConfigurationResponse,

    -- ** GetDeviceDefinitionVersion
    GetDeviceDefinitionVersion (GetDeviceDefinitionVersion'),
    newGetDeviceDefinitionVersion,
    GetDeviceDefinitionVersionResponse (GetDeviceDefinitionVersionResponse'),
    newGetDeviceDefinitionVersionResponse,

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

import Amazonka.Greengrass.AssociateRoleToGroup
import Amazonka.Greengrass.AssociateServiceRoleToAccount
import Amazonka.Greengrass.CreateConnectorDefinition
import Amazonka.Greengrass.CreateConnectorDefinitionVersion
import Amazonka.Greengrass.CreateCoreDefinition
import Amazonka.Greengrass.CreateCoreDefinitionVersion
import Amazonka.Greengrass.CreateDeployment
import Amazonka.Greengrass.CreateDeviceDefinition
import Amazonka.Greengrass.CreateDeviceDefinitionVersion
import Amazonka.Greengrass.CreateFunctionDefinition
import Amazonka.Greengrass.CreateFunctionDefinitionVersion
import Amazonka.Greengrass.CreateGroup
import Amazonka.Greengrass.CreateGroupCertificateAuthority
import Amazonka.Greengrass.CreateGroupVersion
import Amazonka.Greengrass.CreateLoggerDefinition
import Amazonka.Greengrass.CreateLoggerDefinitionVersion
import Amazonka.Greengrass.CreateResourceDefinition
import Amazonka.Greengrass.CreateResourceDefinitionVersion
import Amazonka.Greengrass.CreateSoftwareUpdateJob
import Amazonka.Greengrass.CreateSubscriptionDefinition
import Amazonka.Greengrass.CreateSubscriptionDefinitionVersion
import Amazonka.Greengrass.DeleteConnectorDefinition
import Amazonka.Greengrass.DeleteCoreDefinition
import Amazonka.Greengrass.DeleteDeviceDefinition
import Amazonka.Greengrass.DeleteFunctionDefinition
import Amazonka.Greengrass.DeleteGroup
import Amazonka.Greengrass.DeleteLoggerDefinition
import Amazonka.Greengrass.DeleteResourceDefinition
import Amazonka.Greengrass.DeleteSubscriptionDefinition
import Amazonka.Greengrass.DisassociateRoleFromGroup
import Amazonka.Greengrass.DisassociateServiceRoleFromAccount
import Amazonka.Greengrass.GetAssociatedRole
import Amazonka.Greengrass.GetBulkDeploymentStatus
import Amazonka.Greengrass.GetConnectivityInfo
import Amazonka.Greengrass.GetConnectorDefinition
import Amazonka.Greengrass.GetConnectorDefinitionVersion
import Amazonka.Greengrass.GetCoreDefinition
import Amazonka.Greengrass.GetCoreDefinitionVersion
import Amazonka.Greengrass.GetDeploymentStatus
import Amazonka.Greengrass.GetDeviceDefinition
import Amazonka.Greengrass.GetDeviceDefinitionVersion
import Amazonka.Greengrass.GetFunctionDefinition
import Amazonka.Greengrass.GetFunctionDefinitionVersion
import Amazonka.Greengrass.GetGroup
import Amazonka.Greengrass.GetGroupCertificateAuthority
import Amazonka.Greengrass.GetGroupCertificateConfiguration
import Amazonka.Greengrass.GetGroupVersion
import Amazonka.Greengrass.GetLoggerDefinition
import Amazonka.Greengrass.GetLoggerDefinitionVersion
import Amazonka.Greengrass.GetResourceDefinition
import Amazonka.Greengrass.GetResourceDefinitionVersion
import Amazonka.Greengrass.GetServiceRoleForAccount
import Amazonka.Greengrass.GetSubscriptionDefinition
import Amazonka.Greengrass.GetSubscriptionDefinitionVersion
import Amazonka.Greengrass.GetThingRuntimeConfiguration
import Amazonka.Greengrass.Lens
import Amazonka.Greengrass.ListBulkDeploymentDetailedReports
import Amazonka.Greengrass.ListBulkDeployments
import Amazonka.Greengrass.ListConnectorDefinitionVersions
import Amazonka.Greengrass.ListConnectorDefinitions
import Amazonka.Greengrass.ListCoreDefinitionVersions
import Amazonka.Greengrass.ListCoreDefinitions
import Amazonka.Greengrass.ListDeployments
import Amazonka.Greengrass.ListDeviceDefinitionVersions
import Amazonka.Greengrass.ListDeviceDefinitions
import Amazonka.Greengrass.ListFunctionDefinitionVersions
import Amazonka.Greengrass.ListFunctionDefinitions
import Amazonka.Greengrass.ListGroupCertificateAuthorities
import Amazonka.Greengrass.ListGroupVersions
import Amazonka.Greengrass.ListGroups
import Amazonka.Greengrass.ListLoggerDefinitionVersions
import Amazonka.Greengrass.ListLoggerDefinitions
import Amazonka.Greengrass.ListResourceDefinitionVersions
import Amazonka.Greengrass.ListResourceDefinitions
import Amazonka.Greengrass.ListSubscriptionDefinitionVersions
import Amazonka.Greengrass.ListSubscriptionDefinitions
import Amazonka.Greengrass.ListTagsForResource
import Amazonka.Greengrass.ResetDeployments
import Amazonka.Greengrass.StartBulkDeployment
import Amazonka.Greengrass.StopBulkDeployment
import Amazonka.Greengrass.TagResource
import Amazonka.Greengrass.Types
import Amazonka.Greengrass.UntagResource
import Amazonka.Greengrass.UpdateConnectivityInfo
import Amazonka.Greengrass.UpdateConnectorDefinition
import Amazonka.Greengrass.UpdateCoreDefinition
import Amazonka.Greengrass.UpdateDeviceDefinition
import Amazonka.Greengrass.UpdateFunctionDefinition
import Amazonka.Greengrass.UpdateGroup
import Amazonka.Greengrass.UpdateGroupCertificateConfiguration
import Amazonka.Greengrass.UpdateLoggerDefinition
import Amazonka.Greengrass.UpdateResourceDefinition
import Amazonka.Greengrass.UpdateSubscriptionDefinition
import Amazonka.Greengrass.UpdateThingRuntimeConfiguration
import Amazonka.Greengrass.Waiters

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
