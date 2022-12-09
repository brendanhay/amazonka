{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Greengrass
-- Copyright   : (c) 2013-2022 Brendan Hay
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

    -- ** BadRequestException
    _BadRequestException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateRoleToGroup
    AssociateRoleToGroup (AssociateRoleToGroup'),
    newAssociateRoleToGroup,
    AssociateRoleToGroupResponse (AssociateRoleToGroupResponse'),
    newAssociateRoleToGroupResponse,

    -- ** AssociateServiceRoleToAccount
    AssociateServiceRoleToAccount (AssociateServiceRoleToAccount'),
    newAssociateServiceRoleToAccount,
    AssociateServiceRoleToAccountResponse (AssociateServiceRoleToAccountResponse'),
    newAssociateServiceRoleToAccountResponse,

    -- ** CreateConnectorDefinition
    CreateConnectorDefinition (CreateConnectorDefinition'),
    newCreateConnectorDefinition,
    CreateConnectorDefinitionResponse (CreateConnectorDefinitionResponse'),
    newCreateConnectorDefinitionResponse,

    -- ** CreateConnectorDefinitionVersion
    CreateConnectorDefinitionVersion (CreateConnectorDefinitionVersion'),
    newCreateConnectorDefinitionVersion,
    CreateConnectorDefinitionVersionResponse (CreateConnectorDefinitionVersionResponse'),
    newCreateConnectorDefinitionVersionResponse,

    -- ** CreateCoreDefinition
    CreateCoreDefinition (CreateCoreDefinition'),
    newCreateCoreDefinition,
    CreateCoreDefinitionResponse (CreateCoreDefinitionResponse'),
    newCreateCoreDefinitionResponse,

    -- ** CreateCoreDefinitionVersion
    CreateCoreDefinitionVersion (CreateCoreDefinitionVersion'),
    newCreateCoreDefinitionVersion,
    CreateCoreDefinitionVersionResponse (CreateCoreDefinitionVersionResponse'),
    newCreateCoreDefinitionVersionResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** CreateDeviceDefinition
    CreateDeviceDefinition (CreateDeviceDefinition'),
    newCreateDeviceDefinition,
    CreateDeviceDefinitionResponse (CreateDeviceDefinitionResponse'),
    newCreateDeviceDefinitionResponse,

    -- ** CreateDeviceDefinitionVersion
    CreateDeviceDefinitionVersion (CreateDeviceDefinitionVersion'),
    newCreateDeviceDefinitionVersion,
    CreateDeviceDefinitionVersionResponse (CreateDeviceDefinitionVersionResponse'),
    newCreateDeviceDefinitionVersionResponse,

    -- ** CreateFunctionDefinition
    CreateFunctionDefinition (CreateFunctionDefinition'),
    newCreateFunctionDefinition,
    CreateFunctionDefinitionResponse (CreateFunctionDefinitionResponse'),
    newCreateFunctionDefinitionResponse,

    -- ** CreateFunctionDefinitionVersion
    CreateFunctionDefinitionVersion (CreateFunctionDefinitionVersion'),
    newCreateFunctionDefinitionVersion,
    CreateFunctionDefinitionVersionResponse (CreateFunctionDefinitionVersionResponse'),
    newCreateFunctionDefinitionVersionResponse,

    -- ** CreateGroup
    CreateGroup (CreateGroup'),
    newCreateGroup,
    CreateGroupResponse (CreateGroupResponse'),
    newCreateGroupResponse,

    -- ** CreateGroupCertificateAuthority
    CreateGroupCertificateAuthority (CreateGroupCertificateAuthority'),
    newCreateGroupCertificateAuthority,
    CreateGroupCertificateAuthorityResponse (CreateGroupCertificateAuthorityResponse'),
    newCreateGroupCertificateAuthorityResponse,

    -- ** CreateGroupVersion
    CreateGroupVersion (CreateGroupVersion'),
    newCreateGroupVersion,
    CreateGroupVersionResponse (CreateGroupVersionResponse'),
    newCreateGroupVersionResponse,

    -- ** CreateLoggerDefinition
    CreateLoggerDefinition (CreateLoggerDefinition'),
    newCreateLoggerDefinition,
    CreateLoggerDefinitionResponse (CreateLoggerDefinitionResponse'),
    newCreateLoggerDefinitionResponse,

    -- ** CreateLoggerDefinitionVersion
    CreateLoggerDefinitionVersion (CreateLoggerDefinitionVersion'),
    newCreateLoggerDefinitionVersion,
    CreateLoggerDefinitionVersionResponse (CreateLoggerDefinitionVersionResponse'),
    newCreateLoggerDefinitionVersionResponse,

    -- ** CreateResourceDefinition
    CreateResourceDefinition (CreateResourceDefinition'),
    newCreateResourceDefinition,
    CreateResourceDefinitionResponse (CreateResourceDefinitionResponse'),
    newCreateResourceDefinitionResponse,

    -- ** CreateResourceDefinitionVersion
    CreateResourceDefinitionVersion (CreateResourceDefinitionVersion'),
    newCreateResourceDefinitionVersion,
    CreateResourceDefinitionVersionResponse (CreateResourceDefinitionVersionResponse'),
    newCreateResourceDefinitionVersionResponse,

    -- ** CreateSoftwareUpdateJob
    CreateSoftwareUpdateJob (CreateSoftwareUpdateJob'),
    newCreateSoftwareUpdateJob,
    CreateSoftwareUpdateJobResponse (CreateSoftwareUpdateJobResponse'),
    newCreateSoftwareUpdateJobResponse,

    -- ** CreateSubscriptionDefinition
    CreateSubscriptionDefinition (CreateSubscriptionDefinition'),
    newCreateSubscriptionDefinition,
    CreateSubscriptionDefinitionResponse (CreateSubscriptionDefinitionResponse'),
    newCreateSubscriptionDefinitionResponse,

    -- ** CreateSubscriptionDefinitionVersion
    CreateSubscriptionDefinitionVersion (CreateSubscriptionDefinitionVersion'),
    newCreateSubscriptionDefinitionVersion,
    CreateSubscriptionDefinitionVersionResponse (CreateSubscriptionDefinitionVersionResponse'),
    newCreateSubscriptionDefinitionVersionResponse,

    -- ** DeleteConnectorDefinition
    DeleteConnectorDefinition (DeleteConnectorDefinition'),
    newDeleteConnectorDefinition,
    DeleteConnectorDefinitionResponse (DeleteConnectorDefinitionResponse'),
    newDeleteConnectorDefinitionResponse,

    -- ** DeleteCoreDefinition
    DeleteCoreDefinition (DeleteCoreDefinition'),
    newDeleteCoreDefinition,
    DeleteCoreDefinitionResponse (DeleteCoreDefinitionResponse'),
    newDeleteCoreDefinitionResponse,

    -- ** DeleteDeviceDefinition
    DeleteDeviceDefinition (DeleteDeviceDefinition'),
    newDeleteDeviceDefinition,
    DeleteDeviceDefinitionResponse (DeleteDeviceDefinitionResponse'),
    newDeleteDeviceDefinitionResponse,

    -- ** DeleteFunctionDefinition
    DeleteFunctionDefinition (DeleteFunctionDefinition'),
    newDeleteFunctionDefinition,
    DeleteFunctionDefinitionResponse (DeleteFunctionDefinitionResponse'),
    newDeleteFunctionDefinitionResponse,

    -- ** DeleteGroup
    DeleteGroup (DeleteGroup'),
    newDeleteGroup,
    DeleteGroupResponse (DeleteGroupResponse'),
    newDeleteGroupResponse,

    -- ** DeleteLoggerDefinition
    DeleteLoggerDefinition (DeleteLoggerDefinition'),
    newDeleteLoggerDefinition,
    DeleteLoggerDefinitionResponse (DeleteLoggerDefinitionResponse'),
    newDeleteLoggerDefinitionResponse,

    -- ** DeleteResourceDefinition
    DeleteResourceDefinition (DeleteResourceDefinition'),
    newDeleteResourceDefinition,
    DeleteResourceDefinitionResponse (DeleteResourceDefinitionResponse'),
    newDeleteResourceDefinitionResponse,

    -- ** DeleteSubscriptionDefinition
    DeleteSubscriptionDefinition (DeleteSubscriptionDefinition'),
    newDeleteSubscriptionDefinition,
    DeleteSubscriptionDefinitionResponse (DeleteSubscriptionDefinitionResponse'),
    newDeleteSubscriptionDefinitionResponse,

    -- ** DisassociateRoleFromGroup
    DisassociateRoleFromGroup (DisassociateRoleFromGroup'),
    newDisassociateRoleFromGroup,
    DisassociateRoleFromGroupResponse (DisassociateRoleFromGroupResponse'),
    newDisassociateRoleFromGroupResponse,

    -- ** DisassociateServiceRoleFromAccount
    DisassociateServiceRoleFromAccount (DisassociateServiceRoleFromAccount'),
    newDisassociateServiceRoleFromAccount,
    DisassociateServiceRoleFromAccountResponse (DisassociateServiceRoleFromAccountResponse'),
    newDisassociateServiceRoleFromAccountResponse,

    -- ** GetAssociatedRole
    GetAssociatedRole (GetAssociatedRole'),
    newGetAssociatedRole,
    GetAssociatedRoleResponse (GetAssociatedRoleResponse'),
    newGetAssociatedRoleResponse,

    -- ** GetBulkDeploymentStatus
    GetBulkDeploymentStatus (GetBulkDeploymentStatus'),
    newGetBulkDeploymentStatus,
    GetBulkDeploymentStatusResponse (GetBulkDeploymentStatusResponse'),
    newGetBulkDeploymentStatusResponse,

    -- ** GetConnectivityInfo
    GetConnectivityInfo (GetConnectivityInfo'),
    newGetConnectivityInfo,
    GetConnectivityInfoResponse (GetConnectivityInfoResponse'),
    newGetConnectivityInfoResponse,

    -- ** GetConnectorDefinition
    GetConnectorDefinition (GetConnectorDefinition'),
    newGetConnectorDefinition,
    GetConnectorDefinitionResponse (GetConnectorDefinitionResponse'),
    newGetConnectorDefinitionResponse,

    -- ** GetConnectorDefinitionVersion
    GetConnectorDefinitionVersion (GetConnectorDefinitionVersion'),
    newGetConnectorDefinitionVersion,
    GetConnectorDefinitionVersionResponse (GetConnectorDefinitionVersionResponse'),
    newGetConnectorDefinitionVersionResponse,

    -- ** GetCoreDefinition
    GetCoreDefinition (GetCoreDefinition'),
    newGetCoreDefinition,
    GetCoreDefinitionResponse (GetCoreDefinitionResponse'),
    newGetCoreDefinitionResponse,

    -- ** GetCoreDefinitionVersion
    GetCoreDefinitionVersion (GetCoreDefinitionVersion'),
    newGetCoreDefinitionVersion,
    GetCoreDefinitionVersionResponse (GetCoreDefinitionVersionResponse'),
    newGetCoreDefinitionVersionResponse,

    -- ** GetDeploymentStatus
    GetDeploymentStatus (GetDeploymentStatus'),
    newGetDeploymentStatus,
    GetDeploymentStatusResponse (GetDeploymentStatusResponse'),
    newGetDeploymentStatusResponse,

    -- ** GetDeviceDefinition
    GetDeviceDefinition (GetDeviceDefinition'),
    newGetDeviceDefinition,
    GetDeviceDefinitionResponse (GetDeviceDefinitionResponse'),
    newGetDeviceDefinitionResponse,

    -- ** GetDeviceDefinitionVersion
    GetDeviceDefinitionVersion (GetDeviceDefinitionVersion'),
    newGetDeviceDefinitionVersion,
    GetDeviceDefinitionVersionResponse (GetDeviceDefinitionVersionResponse'),
    newGetDeviceDefinitionVersionResponse,

    -- ** GetFunctionDefinition
    GetFunctionDefinition (GetFunctionDefinition'),
    newGetFunctionDefinition,
    GetFunctionDefinitionResponse (GetFunctionDefinitionResponse'),
    newGetFunctionDefinitionResponse,

    -- ** GetFunctionDefinitionVersion
    GetFunctionDefinitionVersion (GetFunctionDefinitionVersion'),
    newGetFunctionDefinitionVersion,
    GetFunctionDefinitionVersionResponse (GetFunctionDefinitionVersionResponse'),
    newGetFunctionDefinitionVersionResponse,

    -- ** GetGroup
    GetGroup (GetGroup'),
    newGetGroup,
    GetGroupResponse (GetGroupResponse'),
    newGetGroupResponse,

    -- ** GetGroupCertificateAuthority
    GetGroupCertificateAuthority (GetGroupCertificateAuthority'),
    newGetGroupCertificateAuthority,
    GetGroupCertificateAuthorityResponse (GetGroupCertificateAuthorityResponse'),
    newGetGroupCertificateAuthorityResponse,

    -- ** GetGroupCertificateConfiguration
    GetGroupCertificateConfiguration (GetGroupCertificateConfiguration'),
    newGetGroupCertificateConfiguration,
    GetGroupCertificateConfigurationResponse (GetGroupCertificateConfigurationResponse'),
    newGetGroupCertificateConfigurationResponse,

    -- ** GetGroupVersion
    GetGroupVersion (GetGroupVersion'),
    newGetGroupVersion,
    GetGroupVersionResponse (GetGroupVersionResponse'),
    newGetGroupVersionResponse,

    -- ** GetLoggerDefinition
    GetLoggerDefinition (GetLoggerDefinition'),
    newGetLoggerDefinition,
    GetLoggerDefinitionResponse (GetLoggerDefinitionResponse'),
    newGetLoggerDefinitionResponse,

    -- ** GetLoggerDefinitionVersion
    GetLoggerDefinitionVersion (GetLoggerDefinitionVersion'),
    newGetLoggerDefinitionVersion,
    GetLoggerDefinitionVersionResponse (GetLoggerDefinitionVersionResponse'),
    newGetLoggerDefinitionVersionResponse,

    -- ** GetResourceDefinition
    GetResourceDefinition (GetResourceDefinition'),
    newGetResourceDefinition,
    GetResourceDefinitionResponse (GetResourceDefinitionResponse'),
    newGetResourceDefinitionResponse,

    -- ** GetResourceDefinitionVersion
    GetResourceDefinitionVersion (GetResourceDefinitionVersion'),
    newGetResourceDefinitionVersion,
    GetResourceDefinitionVersionResponse (GetResourceDefinitionVersionResponse'),
    newGetResourceDefinitionVersionResponse,

    -- ** GetServiceRoleForAccount
    GetServiceRoleForAccount (GetServiceRoleForAccount'),
    newGetServiceRoleForAccount,
    GetServiceRoleForAccountResponse (GetServiceRoleForAccountResponse'),
    newGetServiceRoleForAccountResponse,

    -- ** GetSubscriptionDefinition
    GetSubscriptionDefinition (GetSubscriptionDefinition'),
    newGetSubscriptionDefinition,
    GetSubscriptionDefinitionResponse (GetSubscriptionDefinitionResponse'),
    newGetSubscriptionDefinitionResponse,

    -- ** GetSubscriptionDefinitionVersion
    GetSubscriptionDefinitionVersion (GetSubscriptionDefinitionVersion'),
    newGetSubscriptionDefinitionVersion,
    GetSubscriptionDefinitionVersionResponse (GetSubscriptionDefinitionVersionResponse'),
    newGetSubscriptionDefinitionVersionResponse,

    -- ** GetThingRuntimeConfiguration
    GetThingRuntimeConfiguration (GetThingRuntimeConfiguration'),
    newGetThingRuntimeConfiguration,
    GetThingRuntimeConfigurationResponse (GetThingRuntimeConfigurationResponse'),
    newGetThingRuntimeConfigurationResponse,

    -- ** ListBulkDeploymentDetailedReports (Paginated)
    ListBulkDeploymentDetailedReports (ListBulkDeploymentDetailedReports'),
    newListBulkDeploymentDetailedReports,
    ListBulkDeploymentDetailedReportsResponse (ListBulkDeploymentDetailedReportsResponse'),
    newListBulkDeploymentDetailedReportsResponse,

    -- ** ListBulkDeployments (Paginated)
    ListBulkDeployments (ListBulkDeployments'),
    newListBulkDeployments,
    ListBulkDeploymentsResponse (ListBulkDeploymentsResponse'),
    newListBulkDeploymentsResponse,

    -- ** ListConnectorDefinitionVersions (Paginated)
    ListConnectorDefinitionVersions (ListConnectorDefinitionVersions'),
    newListConnectorDefinitionVersions,
    ListConnectorDefinitionVersionsResponse (ListConnectorDefinitionVersionsResponse'),
    newListConnectorDefinitionVersionsResponse,

    -- ** ListConnectorDefinitions (Paginated)
    ListConnectorDefinitions (ListConnectorDefinitions'),
    newListConnectorDefinitions,
    ListConnectorDefinitionsResponse (ListConnectorDefinitionsResponse'),
    newListConnectorDefinitionsResponse,

    -- ** ListCoreDefinitionVersions (Paginated)
    ListCoreDefinitionVersions (ListCoreDefinitionVersions'),
    newListCoreDefinitionVersions,
    ListCoreDefinitionVersionsResponse (ListCoreDefinitionVersionsResponse'),
    newListCoreDefinitionVersionsResponse,

    -- ** ListCoreDefinitions (Paginated)
    ListCoreDefinitions (ListCoreDefinitions'),
    newListCoreDefinitions,
    ListCoreDefinitionsResponse (ListCoreDefinitionsResponse'),
    newListCoreDefinitionsResponse,

    -- ** ListDeployments (Paginated)
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** ListDeviceDefinitionVersions (Paginated)
    ListDeviceDefinitionVersions (ListDeviceDefinitionVersions'),
    newListDeviceDefinitionVersions,
    ListDeviceDefinitionVersionsResponse (ListDeviceDefinitionVersionsResponse'),
    newListDeviceDefinitionVersionsResponse,

    -- ** ListDeviceDefinitions (Paginated)
    ListDeviceDefinitions (ListDeviceDefinitions'),
    newListDeviceDefinitions,
    ListDeviceDefinitionsResponse (ListDeviceDefinitionsResponse'),
    newListDeviceDefinitionsResponse,

    -- ** ListFunctionDefinitionVersions (Paginated)
    ListFunctionDefinitionVersions (ListFunctionDefinitionVersions'),
    newListFunctionDefinitionVersions,
    ListFunctionDefinitionVersionsResponse (ListFunctionDefinitionVersionsResponse'),
    newListFunctionDefinitionVersionsResponse,

    -- ** ListFunctionDefinitions (Paginated)
    ListFunctionDefinitions (ListFunctionDefinitions'),
    newListFunctionDefinitions,
    ListFunctionDefinitionsResponse (ListFunctionDefinitionsResponse'),
    newListFunctionDefinitionsResponse,

    -- ** ListGroupCertificateAuthorities
    ListGroupCertificateAuthorities (ListGroupCertificateAuthorities'),
    newListGroupCertificateAuthorities,
    ListGroupCertificateAuthoritiesResponse (ListGroupCertificateAuthoritiesResponse'),
    newListGroupCertificateAuthoritiesResponse,

    -- ** ListGroupVersions (Paginated)
    ListGroupVersions (ListGroupVersions'),
    newListGroupVersions,
    ListGroupVersionsResponse (ListGroupVersionsResponse'),
    newListGroupVersionsResponse,

    -- ** ListGroups (Paginated)
    ListGroups (ListGroups'),
    newListGroups,
    ListGroupsResponse (ListGroupsResponse'),
    newListGroupsResponse,

    -- ** ListLoggerDefinitionVersions (Paginated)
    ListLoggerDefinitionVersions (ListLoggerDefinitionVersions'),
    newListLoggerDefinitionVersions,
    ListLoggerDefinitionVersionsResponse (ListLoggerDefinitionVersionsResponse'),
    newListLoggerDefinitionVersionsResponse,

    -- ** ListLoggerDefinitions (Paginated)
    ListLoggerDefinitions (ListLoggerDefinitions'),
    newListLoggerDefinitions,
    ListLoggerDefinitionsResponse (ListLoggerDefinitionsResponse'),
    newListLoggerDefinitionsResponse,

    -- ** ListResourceDefinitionVersions (Paginated)
    ListResourceDefinitionVersions (ListResourceDefinitionVersions'),
    newListResourceDefinitionVersions,
    ListResourceDefinitionVersionsResponse (ListResourceDefinitionVersionsResponse'),
    newListResourceDefinitionVersionsResponse,

    -- ** ListResourceDefinitions (Paginated)
    ListResourceDefinitions (ListResourceDefinitions'),
    newListResourceDefinitions,
    ListResourceDefinitionsResponse (ListResourceDefinitionsResponse'),
    newListResourceDefinitionsResponse,

    -- ** ListSubscriptionDefinitionVersions (Paginated)
    ListSubscriptionDefinitionVersions (ListSubscriptionDefinitionVersions'),
    newListSubscriptionDefinitionVersions,
    ListSubscriptionDefinitionVersionsResponse (ListSubscriptionDefinitionVersionsResponse'),
    newListSubscriptionDefinitionVersionsResponse,

    -- ** ListSubscriptionDefinitions (Paginated)
    ListSubscriptionDefinitions (ListSubscriptionDefinitions'),
    newListSubscriptionDefinitions,
    ListSubscriptionDefinitionsResponse (ListSubscriptionDefinitionsResponse'),
    newListSubscriptionDefinitionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ResetDeployments
    ResetDeployments (ResetDeployments'),
    newResetDeployments,
    ResetDeploymentsResponse (ResetDeploymentsResponse'),
    newResetDeploymentsResponse,

    -- ** StartBulkDeployment
    StartBulkDeployment (StartBulkDeployment'),
    newStartBulkDeployment,
    StartBulkDeploymentResponse (StartBulkDeploymentResponse'),
    newStartBulkDeploymentResponse,

    -- ** StopBulkDeployment
    StopBulkDeployment (StopBulkDeployment'),
    newStopBulkDeployment,
    StopBulkDeploymentResponse (StopBulkDeploymentResponse'),
    newStopBulkDeploymentResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateConnectivityInfo
    UpdateConnectivityInfo (UpdateConnectivityInfo'),
    newUpdateConnectivityInfo,
    UpdateConnectivityInfoResponse (UpdateConnectivityInfoResponse'),
    newUpdateConnectivityInfoResponse,

    -- ** UpdateConnectorDefinition
    UpdateConnectorDefinition (UpdateConnectorDefinition'),
    newUpdateConnectorDefinition,
    UpdateConnectorDefinitionResponse (UpdateConnectorDefinitionResponse'),
    newUpdateConnectorDefinitionResponse,

    -- ** UpdateCoreDefinition
    UpdateCoreDefinition (UpdateCoreDefinition'),
    newUpdateCoreDefinition,
    UpdateCoreDefinitionResponse (UpdateCoreDefinitionResponse'),
    newUpdateCoreDefinitionResponse,

    -- ** UpdateDeviceDefinition
    UpdateDeviceDefinition (UpdateDeviceDefinition'),
    newUpdateDeviceDefinition,
    UpdateDeviceDefinitionResponse (UpdateDeviceDefinitionResponse'),
    newUpdateDeviceDefinitionResponse,

    -- ** UpdateFunctionDefinition
    UpdateFunctionDefinition (UpdateFunctionDefinition'),
    newUpdateFunctionDefinition,
    UpdateFunctionDefinitionResponse (UpdateFunctionDefinitionResponse'),
    newUpdateFunctionDefinitionResponse,

    -- ** UpdateGroup
    UpdateGroup (UpdateGroup'),
    newUpdateGroup,
    UpdateGroupResponse (UpdateGroupResponse'),
    newUpdateGroupResponse,

    -- ** UpdateGroupCertificateConfiguration
    UpdateGroupCertificateConfiguration (UpdateGroupCertificateConfiguration'),
    newUpdateGroupCertificateConfiguration,
    UpdateGroupCertificateConfigurationResponse (UpdateGroupCertificateConfigurationResponse'),
    newUpdateGroupCertificateConfigurationResponse,

    -- ** UpdateLoggerDefinition
    UpdateLoggerDefinition (UpdateLoggerDefinition'),
    newUpdateLoggerDefinition,
    UpdateLoggerDefinitionResponse (UpdateLoggerDefinitionResponse'),
    newUpdateLoggerDefinitionResponse,

    -- ** UpdateResourceDefinition
    UpdateResourceDefinition (UpdateResourceDefinition'),
    newUpdateResourceDefinition,
    UpdateResourceDefinitionResponse (UpdateResourceDefinitionResponse'),
    newUpdateResourceDefinitionResponse,

    -- ** UpdateSubscriptionDefinition
    UpdateSubscriptionDefinition (UpdateSubscriptionDefinition'),
    newUpdateSubscriptionDefinition,
    UpdateSubscriptionDefinitionResponse (UpdateSubscriptionDefinitionResponse'),
    newUpdateSubscriptionDefinitionResponse,

    -- ** UpdateThingRuntimeConfiguration
    UpdateThingRuntimeConfiguration (UpdateThingRuntimeConfiguration'),
    newUpdateThingRuntimeConfiguration,
    UpdateThingRuntimeConfigurationResponse (UpdateThingRuntimeConfigurationResponse'),
    newUpdateThingRuntimeConfigurationResponse,

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
