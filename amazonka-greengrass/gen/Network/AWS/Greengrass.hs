{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Greengrass seamlessly extends AWS onto physical devices so they can act locally on the data they generate, while still using the cloud for management, analytics, and durable storage. AWS Greengrass ensures your devices can respond quickly to local events and operate with intermittent connectivity. AWS Greengrass minimizes the cost of transmitting data to the cloud by allowing you to author AWS Lambda functions that execute locally.
module Network.AWS.Greengrass
    (
    -- * Service Configuration
      greengrass

    -- * Errors
    -- $errors

    -- ** InternalServerErrorException
    , _InternalServerErrorException

    -- ** BadRequestException
    , _BadRequestException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetGroupCertificateConfiguration
    , module Network.AWS.Greengrass.GetGroupCertificateConfiguration

    -- ** ListGroupVersions
    , module Network.AWS.Greengrass.ListGroupVersions

    -- ** ListFunctionDefinitionVersions
    , module Network.AWS.Greengrass.ListFunctionDefinitionVersions

    -- ** ListDeviceDefinitions
    , module Network.AWS.Greengrass.ListDeviceDefinitions

    -- ** AssociateRoleToGroup
    , module Network.AWS.Greengrass.AssociateRoleToGroup

    -- ** UpdateCoreDefinition
    , module Network.AWS.Greengrass.UpdateCoreDefinition

    -- ** DeleteCoreDefinition
    , module Network.AWS.Greengrass.DeleteCoreDefinition

    -- ** GetLoggerDefinition
    , module Network.AWS.Greengrass.GetLoggerDefinition

    -- ** ListGroupCertificateAuthorities
    , module Network.AWS.Greengrass.ListGroupCertificateAuthorities

    -- ** DisassociateRoleFromGroup
    , module Network.AWS.Greengrass.DisassociateRoleFromGroup

    -- ** UpdateSubscriptionDefinition
    , module Network.AWS.Greengrass.UpdateSubscriptionDefinition

    -- ** DeleteSubscriptionDefinition
    , module Network.AWS.Greengrass.DeleteSubscriptionDefinition

    -- ** ListCoreDefinitions
    , module Network.AWS.Greengrass.ListCoreDefinitions

    -- ** ListSubscriptionDefinitions
    , module Network.AWS.Greengrass.ListSubscriptionDefinitions

    -- ** CreateGroupCertificateAuthority
    , module Network.AWS.Greengrass.CreateGroupCertificateAuthority

    -- ** CreateLoggerDefinitionVersion
    , module Network.AWS.Greengrass.CreateLoggerDefinitionVersion

    -- ** CreateCoreDefinition
    , module Network.AWS.Greengrass.CreateCoreDefinition

    -- ** UpdateConnectivityInfo
    , module Network.AWS.Greengrass.UpdateConnectivityInfo

    -- ** CreateSubscriptionDefinition
    , module Network.AWS.Greengrass.CreateSubscriptionDefinition

    -- ** GetGroupCertificateAuthority
    , module Network.AWS.Greengrass.GetGroupCertificateAuthority

    -- ** GetLoggerDefinitionVersion
    , module Network.AWS.Greengrass.GetLoggerDefinitionVersion

    -- ** GetServiceRoleForAccount
    , module Network.AWS.Greengrass.GetServiceRoleForAccount

    -- ** CreateSoftwareUpdateJob
    , module Network.AWS.Greengrass.CreateSoftwareUpdateJob

    -- ** CreateLoggerDefinition
    , module Network.AWS.Greengrass.CreateLoggerDefinition

    -- ** GetConnectivityInfo
    , module Network.AWS.Greengrass.GetConnectivityInfo

    -- ** CreateDeployment
    , module Network.AWS.Greengrass.CreateDeployment

    -- ** DeleteLoggerDefinition
    , module Network.AWS.Greengrass.DeleteLoggerDefinition

    -- ** UpdateLoggerDefinition
    , module Network.AWS.Greengrass.UpdateLoggerDefinition

    -- ** GetSubscriptionDefinition
    , module Network.AWS.Greengrass.GetSubscriptionDefinition

    -- ** GetCoreDefinition
    , module Network.AWS.Greengrass.GetCoreDefinition

    -- ** GetDeploymentStatus
    , module Network.AWS.Greengrass.GetDeploymentStatus

    -- ** CreateResourceDefinition
    , module Network.AWS.Greengrass.CreateResourceDefinition

    -- ** GetResourceDefinitionVersion
    , module Network.AWS.Greengrass.GetResourceDefinitionVersion

    -- ** UpdateFunctionDefinition
    , module Network.AWS.Greengrass.UpdateFunctionDefinition

    -- ** DeleteFunctionDefinition
    , module Network.AWS.Greengrass.DeleteFunctionDefinition

    -- ** ListResourceDefinitions
    , module Network.AWS.Greengrass.ListResourceDefinitions

    -- ** CreateResourceDefinitionVersion
    , module Network.AWS.Greengrass.CreateResourceDefinitionVersion

    -- ** GetResourceDefinition
    , module Network.AWS.Greengrass.GetResourceDefinition

    -- ** ListResourceDefinitionVersions
    , module Network.AWS.Greengrass.ListResourceDefinitionVersions

    -- ** DisassociateServiceRoleFromAccount
    , module Network.AWS.Greengrass.DisassociateServiceRoleFromAccount

    -- ** DeleteDeviceDefinition
    , module Network.AWS.Greengrass.DeleteDeviceDefinition

    -- ** UpdateDeviceDefinition
    , module Network.AWS.Greengrass.UpdateDeviceDefinition

    -- ** AssociateServiceRoleToAccount
    , module Network.AWS.Greengrass.AssociateServiceRoleToAccount

    -- ** ResetDeployments
    , module Network.AWS.Greengrass.ResetDeployments

    -- ** GetSubscriptionDefinitionVersion
    , module Network.AWS.Greengrass.GetSubscriptionDefinitionVersion

    -- ** GetAssociatedRole
    , module Network.AWS.Greengrass.GetAssociatedRole

    -- ** ListLoggerDefinitionVersions
    , module Network.AWS.Greengrass.ListLoggerDefinitionVersions

    -- ** GetCoreDefinitionVersion
    , module Network.AWS.Greengrass.GetCoreDefinitionVersion

    -- ** ListSubscriptionDefinitionVersions
    , module Network.AWS.Greengrass.ListSubscriptionDefinitionVersions

    -- ** ListCoreDefinitionVersions
    , module Network.AWS.Greengrass.ListCoreDefinitionVersions

    -- ** CreateCoreDefinitionVersion
    , module Network.AWS.Greengrass.CreateCoreDefinitionVersion

    -- ** ListDeployments
    , module Network.AWS.Greengrass.ListDeployments

    -- ** ListLoggerDefinitions
    , module Network.AWS.Greengrass.ListLoggerDefinitions

    -- ** CreateSubscriptionDefinitionVersion
    , module Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion

    -- ** GetGroupVersion
    , module Network.AWS.Greengrass.GetGroupVersion

    -- ** UpdateGroupCertificateConfiguration
    , module Network.AWS.Greengrass.UpdateGroupCertificateConfiguration

    -- ** GetFunctionDefinitionVersion
    , module Network.AWS.Greengrass.GetFunctionDefinitionVersion

    -- ** GetDeviceDefinition
    , module Network.AWS.Greengrass.GetDeviceDefinition

    -- ** CreateGroup
    , module Network.AWS.Greengrass.CreateGroup

    -- ** CreateFunctionDefinition
    , module Network.AWS.Greengrass.CreateFunctionDefinition

    -- ** CreateDeviceDefinitionVersion
    , module Network.AWS.Greengrass.CreateDeviceDefinitionVersion

    -- ** DeleteGroup
    , module Network.AWS.Greengrass.DeleteGroup

    -- ** UpdateGroup
    , module Network.AWS.Greengrass.UpdateGroup

    -- ** ListGroups
    , module Network.AWS.Greengrass.ListGroups

    -- ** DeleteResourceDefinition
    , module Network.AWS.Greengrass.DeleteResourceDefinition

    -- ** UpdateResourceDefinition
    , module Network.AWS.Greengrass.UpdateResourceDefinition

    -- ** ListDeviceDefinitionVersions
    , module Network.AWS.Greengrass.ListDeviceDefinitionVersions

    -- ** ListFunctionDefinitions
    , module Network.AWS.Greengrass.ListFunctionDefinitions

    -- ** GetFunctionDefinition
    , module Network.AWS.Greengrass.GetFunctionDefinition

    -- ** GetGroup
    , module Network.AWS.Greengrass.GetGroup

    -- ** CreateDeviceDefinition
    , module Network.AWS.Greengrass.CreateDeviceDefinition

    -- ** CreateGroupVersion
    , module Network.AWS.Greengrass.CreateGroupVersion

    -- ** CreateFunctionDefinitionVersion
    , module Network.AWS.Greengrass.CreateFunctionDefinitionVersion

    -- ** GetDeviceDefinitionVersion
    , module Network.AWS.Greengrass.GetDeviceDefinitionVersion

    -- * Types

    -- ** DeploymentType
    , DeploymentType (..)

    -- ** EncodingType
    , EncodingType (..)

    -- ** LoggerComponent
    , LoggerComponent (..)

    -- ** LoggerLevel
    , LoggerLevel (..)

    -- ** LoggerType
    , LoggerType (..)

    -- ** Permission
    , Permission (..)

    -- ** SoftwareToUpdate
    , SoftwareToUpdate (..)

    -- ** UpdateAgentLogLevel
    , UpdateAgentLogLevel (..)

    -- ** UpdateTargetsArchitecture
    , UpdateTargetsArchitecture (..)

    -- ** UpdateTargetsOperatingSystem
    , UpdateTargetsOperatingSystem (..)

    -- ** ConnectivityInfo
    , ConnectivityInfo
    , connectivityInfo
    , ciPortNumber
    , ciId
    , ciMetadata
    , ciHostAddress

    -- ** Core
    , Core
    , core
    , cCertificateARN
    , cThingARN
    , cSyncShadow
    , cId

    -- ** CoreDefinitionVersion
    , CoreDefinitionVersion
    , coreDefinitionVersion
    , cdvCores

    -- ** DefinitionInformation
    , DefinitionInformation
    , definitionInformation
    , diLatestVersionARN
    , diARN
    , diName
    , diCreationTimestamp
    , diId
    , diLatestVersion
    , diLastUpdatedTimestamp

    -- ** Deployment
    , Deployment
    , deployment
    , dDeploymentId
    , dDeploymentARN
    , dCreatedAt
    , dDeploymentType
    , dGroupARN

    -- ** Device
    , Device
    , device
    , dCertificateARN
    , dThingARN
    , dSyncShadow
    , dId

    -- ** DeviceDefinitionVersion
    , DeviceDefinitionVersion
    , deviceDefinitionVersion
    , ddvDevices

    -- ** ErrorDetail
    , ErrorDetail
    , errorDetail
    , edDetailedErrorCode
    , edDetailedErrorMessage

    -- ** Function
    , Function
    , function
    , fFunctionARN
    , fFunctionConfiguration
    , fId

    -- ** FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcMemorySize
    , fcExecArgs
    , fcEnvironment
    , fcExecutable
    , fcPinned
    , fcEncodingType
    , fcTimeout

    -- ** FunctionConfigurationEnvironment
    , FunctionConfigurationEnvironment
    , functionConfigurationEnvironment
    , fceVariables
    , fceResourceAccessPolicies
    , fceAccessSysfs

    -- ** FunctionDefinitionVersion
    , FunctionDefinitionVersion
    , functionDefinitionVersion
    , fdvFunctions

    -- ** GreengrassLogger
    , GreengrassLogger
    , greengrassLogger
    , glSpace
    , glComponent
    , glId
    , glType
    , glLevel

    -- ** GroupCertificateAuthorityProperties
    , GroupCertificateAuthorityProperties
    , groupCertificateAuthorityProperties
    , gcapGroupCertificateAuthorityARN
    , gcapGroupCertificateAuthorityId

    -- ** GroupInformation
    , GroupInformation
    , groupInformation
    , giLatestVersionARN
    , giARN
    , giName
    , giCreationTimestamp
    , giId
    , giLatestVersion
    , giLastUpdatedTimestamp

    -- ** GroupOwnerSetting
    , GroupOwnerSetting
    , groupOwnerSetting
    , gosAutoAddGroupOwner
    , gosGroupOwner

    -- ** GroupVersion
    , GroupVersion
    , groupVersion
    , gvResourceDefinitionVersionARN
    , gvSubscriptionDefinitionVersionARN
    , gvCoreDefinitionVersionARN
    , gvDeviceDefinitionVersionARN
    , gvFunctionDefinitionVersionARN
    , gvLoggerDefinitionVersionARN

    -- ** LocalDeviceResourceData
    , LocalDeviceResourceData
    , localDeviceResourceData
    , ldrdGroupOwnerSetting
    , ldrdSourcePath

    -- ** LocalVolumeResourceData
    , LocalVolumeResourceData
    , localVolumeResourceData
    , lvrdGroupOwnerSetting
    , lvrdDestinationPath
    , lvrdSourcePath

    -- ** LoggerDefinitionVersion
    , LoggerDefinitionVersion
    , loggerDefinitionVersion
    , ldvLoggers

    -- ** Resource
    , Resource
    , resource
    , rResourceDataContainer
    , rName
    , rId

    -- ** ResourceAccessPolicy
    , ResourceAccessPolicy
    , resourceAccessPolicy
    , rapResourceId
    , rapPermission

    -- ** ResourceDataContainer
    , ResourceDataContainer
    , resourceDataContainer
    , rdcS3MachineLearningModelResourceData
    , rdcSageMakerMachineLearningModelResourceData
    , rdcLocalVolumeResourceData
    , rdcLocalDeviceResourceData

    -- ** ResourceDefinitionVersion
    , ResourceDefinitionVersion
    , resourceDefinitionVersion
    , rdvResources

    -- ** S3MachineLearningModelResourceData
    , S3MachineLearningModelResourceData
    , s3MachineLearningModelResourceData
    , smlmrdDestinationPath
    , smlmrdS3URI

    -- ** SageMakerMachineLearningModelResourceData
    , SageMakerMachineLearningModelResourceData
    , sageMakerMachineLearningModelResourceData
    , smmlmrdSageMakerJobARN
    , smmlmrdDestinationPath

    -- ** Subscription
    , Subscription
    , subscription
    , sSubject
    , sSource
    , sId
    , sTarget

    -- ** SubscriptionDefinitionVersion
    , SubscriptionDefinitionVersion
    , subscriptionDefinitionVersion
    , sdvSubscriptions

    -- ** VersionInformation
    , VersionInformation
    , versionInformation
    , viARN
    , viCreationTimestamp
    , viVersion
    , viId
    ) where

import Network.AWS.Greengrass.AssociateRoleToGroup
import Network.AWS.Greengrass.AssociateServiceRoleToAccount
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
import Network.AWS.Greengrass.GetConnectivityInfo
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
import Network.AWS.Greengrass.ListCoreDefinitions
import Network.AWS.Greengrass.ListCoreDefinitionVersions
import Network.AWS.Greengrass.ListDeployments
import Network.AWS.Greengrass.ListDeviceDefinitions
import Network.AWS.Greengrass.ListDeviceDefinitionVersions
import Network.AWS.Greengrass.ListFunctionDefinitions
import Network.AWS.Greengrass.ListFunctionDefinitionVersions
import Network.AWS.Greengrass.ListGroupCertificateAuthorities
import Network.AWS.Greengrass.ListGroups
import Network.AWS.Greengrass.ListGroupVersions
import Network.AWS.Greengrass.ListLoggerDefinitions
import Network.AWS.Greengrass.ListLoggerDefinitionVersions
import Network.AWS.Greengrass.ListResourceDefinitions
import Network.AWS.Greengrass.ListResourceDefinitionVersions
import Network.AWS.Greengrass.ListSubscriptionDefinitions
import Network.AWS.Greengrass.ListSubscriptionDefinitionVersions
import Network.AWS.Greengrass.ResetDeployments
import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.UpdateConnectivityInfo
import Network.AWS.Greengrass.UpdateCoreDefinition
import Network.AWS.Greengrass.UpdateDeviceDefinition
import Network.AWS.Greengrass.UpdateFunctionDefinition
import Network.AWS.Greengrass.UpdateGroup
import Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
import Network.AWS.Greengrass.UpdateLoggerDefinition
import Network.AWS.Greengrass.UpdateResourceDefinition
import Network.AWS.Greengrass.UpdateSubscriptionDefinition
import Network.AWS.Greengrass.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Greengrass'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
