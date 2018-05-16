{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Directory Service__
--
-- AWS Directory Service is a web service that makes it easy for you to setup and run directories in the AWS cloud, or connect your AWS resources with an existing on-premises Microsoft Active Directory. This guide provides detailed information about AWS Directory Service operations, data types, parameters, and errors. For information about AWS Directory Services features, see <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html AWS Directory Service Administration Guide> .
--
module Network.AWS.DirectoryService
    (
    -- * Service Configuration
      directoryService

    -- * Errors
    -- $errors

    -- ** DirectoryUnavailableException
    , _DirectoryUnavailableException

    -- ** AuthenticationFailedException
    , _AuthenticationFailedException

    -- ** InvalidParameterException
    , _InvalidParameterException

    -- ** UnsupportedOperationException
    , _UnsupportedOperationException

    -- ** EntityAlreadyExistsException
    , _EntityAlreadyExistsException

    -- ** DirectoryLimitExceededException
    , _DirectoryLimitExceededException

    -- ** IPRouteLimitExceededException
    , _IPRouteLimitExceededException

    -- ** EntityDoesNotExistException
    , _EntityDoesNotExistException

    -- ** InsufficientPermissionsException
    , _InsufficientPermissionsException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** ServiceException
    , _ServiceException

    -- ** SnapshotLimitExceededException
    , _SnapshotLimitExceededException

    -- ** DomainControllerLimitExceededException
    , _DomainControllerLimitExceededException

    -- ** TagLimitExceededException
    , _TagLimitExceededException

    -- ** ClientException
    , _ClientException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateNumberOfDomainControllers
    , module Network.AWS.DirectoryService.UpdateNumberOfDomainControllers

    -- ** DescribeConditionalForwarders
    , module Network.AWS.DirectoryService.DescribeConditionalForwarders

    -- ** GetSnapshotLimits
    , module Network.AWS.DirectoryService.GetSnapshotLimits

    -- ** RegisterEventTopic
    , module Network.AWS.DirectoryService.RegisterEventTopic

    -- ** ConnectDirectory
    , module Network.AWS.DirectoryService.ConnectDirectory

    -- ** CreateAlias
    , module Network.AWS.DirectoryService.CreateAlias

    -- ** DescribeDirectories
    , module Network.AWS.DirectoryService.DescribeDirectories

    -- ** AddIPRoutes
    , module Network.AWS.DirectoryService.AddIPRoutes

    -- ** ListTagsForResource
    , module Network.AWS.DirectoryService.ListTagsForResource

    -- ** DescribeTrusts
    , module Network.AWS.DirectoryService.DescribeTrusts

    -- ** DeleteTrust
    , module Network.AWS.DirectoryService.DeleteTrust

    -- ** CreateMicrosoftAD
    , module Network.AWS.DirectoryService.CreateMicrosoftAD

    -- ** DeregisterEventTopic
    , module Network.AWS.DirectoryService.DeregisterEventTopic

    -- ** CreateDirectory
    , module Network.AWS.DirectoryService.CreateDirectory

    -- ** RemoveTagsFromResource
    , module Network.AWS.DirectoryService.RemoveTagsFromResource

    -- ** DescribeEventTopics
    , module Network.AWS.DirectoryService.DescribeEventTopics

    -- ** UpdateConditionalForwarder
    , module Network.AWS.DirectoryService.UpdateConditionalForwarder

    -- ** DeleteConditionalForwarder
    , module Network.AWS.DirectoryService.DeleteConditionalForwarder

    -- ** EnableSSO
    , module Network.AWS.DirectoryService.EnableSSO

    -- ** CancelSchemaExtension
    , module Network.AWS.DirectoryService.CancelSchemaExtension

    -- ** EnableRadius
    , module Network.AWS.DirectoryService.EnableRadius

    -- ** ListIPRoutes
    , module Network.AWS.DirectoryService.ListIPRoutes

    -- ** AddTagsToResource
    , module Network.AWS.DirectoryService.AddTagsToResource

    -- ** ListSchemaExtensions
    , module Network.AWS.DirectoryService.ListSchemaExtensions

    -- ** DisableRadius
    , module Network.AWS.DirectoryService.DisableRadius

    -- ** RestoreFromSnapshot
    , module Network.AWS.DirectoryService.RestoreFromSnapshot

    -- ** DescribeDomainControllers (Paginated)
    , module Network.AWS.DirectoryService.DescribeDomainControllers

    -- ** DescribeSnapshots
    , module Network.AWS.DirectoryService.DescribeSnapshots

    -- ** RemoveIPRoutes
    , module Network.AWS.DirectoryService.RemoveIPRoutes

    -- ** DeleteSnapshot
    , module Network.AWS.DirectoryService.DeleteSnapshot

    -- ** StartSchemaExtension
    , module Network.AWS.DirectoryService.StartSchemaExtension

    -- ** CreateTrust
    , module Network.AWS.DirectoryService.CreateTrust

    -- ** DeleteDirectory
    , module Network.AWS.DirectoryService.DeleteDirectory

    -- ** CreateSnapshot
    , module Network.AWS.DirectoryService.CreateSnapshot

    -- ** CreateComputer
    , module Network.AWS.DirectoryService.CreateComputer

    -- ** DisableSSO
    , module Network.AWS.DirectoryService.DisableSSO

    -- ** VerifyTrust
    , module Network.AWS.DirectoryService.VerifyTrust

    -- ** CreateConditionalForwarder
    , module Network.AWS.DirectoryService.CreateConditionalForwarder

    -- ** GetDirectoryLimits
    , module Network.AWS.DirectoryService.GetDirectoryLimits

    -- ** UpdateRadius
    , module Network.AWS.DirectoryService.UpdateRadius

    -- * Types

    -- ** DirectoryEdition
    , DirectoryEdition (..)

    -- ** DirectorySize
    , DirectorySize (..)

    -- ** DirectoryStage
    , DirectoryStage (..)

    -- ** DirectoryType
    , DirectoryType (..)

    -- ** DomainControllerStatus
    , DomainControllerStatus (..)

    -- ** IPRouteStatusMsg
    , IPRouteStatusMsg (..)

    -- ** RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- ** RadiusStatus
    , RadiusStatus (..)

    -- ** ReplicationScope
    , ReplicationScope (..)

    -- ** SchemaExtensionStatus
    , SchemaExtensionStatus (..)

    -- ** SnapshotStatus
    , SnapshotStatus (..)

    -- ** SnapshotType
    , SnapshotType (..)

    -- ** TopicStatus
    , TopicStatus (..)

    -- ** TrustDirection
    , TrustDirection (..)

    -- ** TrustState
    , TrustState (..)

    -- ** TrustType
    , TrustType (..)

    -- ** Attribute
    , Attribute
    , attribute
    , aValue
    , aName

    -- ** Computer
    , Computer
    , computer
    , cComputerId
    , cComputerAttributes
    , cComputerName

    -- ** ConditionalForwarder
    , ConditionalForwarder
    , conditionalForwarder
    , cfDNSIPAddrs
    , cfRemoteDomainName
    , cfReplicationScope

    -- ** DirectoryConnectSettings
    , DirectoryConnectSettings
    , directoryConnectSettings
    , dcsVPCId
    , dcsSubnetIds
    , dcsCustomerDNSIPs
    , dcsCustomerUserName

    -- ** DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription
    , directoryConnectSettingsDescription
    , dcsdCustomerUserName
    , dcsdSubnetIds
    , dcsdVPCId
    , dcsdSecurityGroupId
    , dcsdConnectIPs
    , dcsdAvailabilityZones

    -- ** DirectoryDescription
    , DirectoryDescription
    , directoryDescription
    , ddEdition
    , ddRadiusStatus
    , ddStage
    , ddDirectoryId
    , ddAccessURL
    , ddShortName
    , ddSize
    , ddDesiredNumberOfDomainControllers
    , ddRadiusSettings
    , ddLaunchTime
    , ddAlias
    , ddName
    , ddStageLastUpdatedDateTime
    , ddSSOEnabled
    , ddDNSIPAddrs
    , ddVPCSettings
    , ddType
    , ddStageReason
    , ddConnectSettings
    , ddDescription

    -- ** DirectoryLimits
    , DirectoryLimits
    , directoryLimits
    , dlConnectedDirectoriesCurrentCount
    , dlCloudOnlyMicrosoftADLimitReached
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADLimit
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADCurrentCount

    -- ** DirectoryVPCSettings
    , DirectoryVPCSettings
    , directoryVPCSettings
    , dvsVPCId
    , dvsSubnetIds

    -- ** DirectoryVPCSettingsDescription
    , DirectoryVPCSettingsDescription
    , directoryVPCSettingsDescription
    , dvsdSubnetIds
    , dvsdVPCId
    , dvsdSecurityGroupId
    , dvsdAvailabilityZones

    -- ** DomainController
    , DomainController
    , domainController
    , dcStatus
    , dcDirectoryId
    , dcVPCId
    , dcLaunchTime
    , dcSubnetId
    , dcAvailabilityZone
    , dcStatusLastUpdatedDateTime
    , dcStatusReason
    , dcDNSIPAddr
    , dcDomainControllerId

    -- ** EventTopic
    , EventTopic
    , eventTopic
    , etStatus
    , etDirectoryId
    , etTopicName
    , etTopicARN
    , etCreatedDateTime

    -- ** IPRoute
    , IPRoute
    , ipRoute
    , irCidrIP
    , irDescription

    -- ** IPRouteInfo
    , IPRouteInfo
    , ipRouteInfo
    , iriDirectoryId
    , iriIPRouteStatusReason
    , iriAddedDateTime
    , iriCidrIP
    , iriIPRouteStatusMsg
    , iriDescription

    -- ** RadiusSettings
    , RadiusSettings
    , radiusSettings
    , rsDisplayLabel
    , rsRadiusRetries
    , rsAuthenticationProtocol
    , rsRadiusServers
    , rsUseSameUsername
    , rsSharedSecret
    , rsRadiusTimeout
    , rsRadiusPort

    -- ** SchemaExtensionInfo
    , SchemaExtensionInfo
    , schemaExtensionInfo
    , seiDirectoryId
    , seiSchemaExtensionId
    , seiSchemaExtensionStatusReason
    , seiSchemaExtensionStatus
    , seiDescription
    , seiEndDateTime
    , seiStartDateTime

    -- ** Snapshot
    , Snapshot
    , snapshot
    , sStatus
    , sDirectoryId
    , sStartTime
    , sName
    , sType
    , sSnapshotId

    -- ** SnapshotLimits
    , SnapshotLimits
    , snapshotLimits
    , slManualSnapshotsLimitReached
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit

    -- ** Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- ** Trust
    , Trust
    , trust
    , tDirectoryId
    , tTrustState
    , tLastUpdatedDateTime
    , tTrustDirection
    , tStateLastUpdatedDateTime
    , tTrustType
    , tTrustStateReason
    , tRemoteDomainName
    , tTrustId
    , tCreatedDateTime
    ) where

import Network.AWS.DirectoryService.AddIPRoutes
import Network.AWS.DirectoryService.AddTagsToResource
import Network.AWS.DirectoryService.CancelSchemaExtension
import Network.AWS.DirectoryService.ConnectDirectory
import Network.AWS.DirectoryService.CreateAlias
import Network.AWS.DirectoryService.CreateComputer
import Network.AWS.DirectoryService.CreateConditionalForwarder
import Network.AWS.DirectoryService.CreateDirectory
import Network.AWS.DirectoryService.CreateMicrosoftAD
import Network.AWS.DirectoryService.CreateSnapshot
import Network.AWS.DirectoryService.CreateTrust
import Network.AWS.DirectoryService.DeleteConditionalForwarder
import Network.AWS.DirectoryService.DeleteDirectory
import Network.AWS.DirectoryService.DeleteSnapshot
import Network.AWS.DirectoryService.DeleteTrust
import Network.AWS.DirectoryService.DeregisterEventTopic
import Network.AWS.DirectoryService.DescribeConditionalForwarders
import Network.AWS.DirectoryService.DescribeDirectories
import Network.AWS.DirectoryService.DescribeDomainControllers
import Network.AWS.DirectoryService.DescribeEventTopics
import Network.AWS.DirectoryService.DescribeSnapshots
import Network.AWS.DirectoryService.DescribeTrusts
import Network.AWS.DirectoryService.DisableRadius
import Network.AWS.DirectoryService.DisableSSO
import Network.AWS.DirectoryService.EnableRadius
import Network.AWS.DirectoryService.EnableSSO
import Network.AWS.DirectoryService.GetDirectoryLimits
import Network.AWS.DirectoryService.GetSnapshotLimits
import Network.AWS.DirectoryService.ListIPRoutes
import Network.AWS.DirectoryService.ListSchemaExtensions
import Network.AWS.DirectoryService.ListTagsForResource
import Network.AWS.DirectoryService.RegisterEventTopic
import Network.AWS.DirectoryService.RemoveIPRoutes
import Network.AWS.DirectoryService.RemoveTagsFromResource
import Network.AWS.DirectoryService.RestoreFromSnapshot
import Network.AWS.DirectoryService.StartSchemaExtension
import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.UpdateConditionalForwarder
import Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
import Network.AWS.DirectoryService.UpdateRadius
import Network.AWS.DirectoryService.VerifyTrust
import Network.AWS.DirectoryService.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DirectoryService'.
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
