{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Directory Service__ 
--
-- AWS Directory Service is a web service that makes it easy for you to setup and run directories in the AWS cloud, or connect your AWS resources with an existing on-premises Microsoft Active Directory. This guide provides detailed information about AWS Directory Service operations, data types, parameters, and errors. For information about AWS Directory Services features, see <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html AWS Directory Service Administration Guide> .
module Network.AWS.DirectoryService
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** CertificateLimitExceededException
    , _CertificateLimitExceededException

    -- ** CertificateAlreadyExistsException
    , _CertificateAlreadyExistsException

    -- ** AccessDeniedException
    , _AccessDeniedException

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

    -- ** NoAvailableCertificateException
    , _NoAvailableCertificateException

    -- ** UserDoesNotExistException
    , _UserDoesNotExistException

    -- ** DirectoryLimitExceededException
    , _DirectoryLimitExceededException

    -- ** InvalidLDAPSStatusException
    , _InvalidLDAPSStatusException

    -- ** InvalidCertificateException
    , _InvalidCertificateException

    -- ** CertificateInUseException
    , _CertificateInUseException

    -- ** RegionLimitExceededException
    , _RegionLimitExceededException

    -- ** IpRouteLimitExceededException
    , _IpRouteLimitExceededException

    -- ** ShareLimitExceededException
    , _ShareLimitExceededException

    -- ** EntityDoesNotExistException
    , _EntityDoesNotExistException

    -- ** OrganizationsException
    , _OrganizationsException

    -- ** InvalidTargetException
    , _InvalidTargetException

    -- ** DirectoryAlreadyInRegionException
    , _DirectoryAlreadyInRegionException

    -- ** InsufficientPermissionsException
    , _InsufficientPermissionsException

    -- ** DirectoryNotSharedException
    , _DirectoryNotSharedException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** ServiceException
    , _ServiceException

    -- ** SnapshotLimitExceededException
    , _SnapshotLimitExceededException

    -- ** DomainControllerLimitExceededException
    , _DomainControllerLimitExceededException

    -- ** DirectoryDoesNotExistException
    , _DirectoryDoesNotExistException

    -- ** TagLimitExceededException
    , _TagLimitExceededException

    -- ** ClientException
    , _ClientException

    -- ** DirectoryAlreadySharedException
    , _DirectoryAlreadySharedException

    -- ** CertificateDoesNotExistException
    , _CertificateDoesNotExistException

    -- ** InvalidPasswordException
    , _InvalidPasswordException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ShareDirectory 
    , module Network.AWS.DirectoryService.ShareDirectory

    -- ** UpdateNumberOfDomainControllers 
    , module Network.AWS.DirectoryService.UpdateNumberOfDomainControllers

    -- ** DescribeConditionalForwarders 
    , module Network.AWS.DirectoryService.DescribeConditionalForwarders

    -- ** GetSnapshotLimits 
    , module Network.AWS.DirectoryService.GetSnapshotLimits

    -- ** RegisterEventTopic 
    , module Network.AWS.DirectoryService.RegisterEventTopic

    -- ** RegisterCertificate 
    , module Network.AWS.DirectoryService.RegisterCertificate

    -- ** ConnectDirectory 
    , module Network.AWS.DirectoryService.ConnectDirectory

    -- ** DescribeLDAPSSettings 
    , module Network.AWS.DirectoryService.DescribeLDAPSSettings

    -- ** CreateAlias 
    , module Network.AWS.DirectoryService.CreateAlias

    -- ** DescribeDirectories (Paginated)
    , module Network.AWS.DirectoryService.DescribeDirectories

    -- ** AddIpRoutes 
    , module Network.AWS.DirectoryService.AddIpRoutes

    -- ** ListTagsForResource (Paginated)
    , module Network.AWS.DirectoryService.ListTagsForResource

    -- ** DescribeTrusts (Paginated)
    , module Network.AWS.DirectoryService.DescribeTrusts

    -- ** DeleteTrust 
    , module Network.AWS.DirectoryService.DeleteTrust

    -- ** UpdateTrust 
    , module Network.AWS.DirectoryService.UpdateTrust

    -- ** CreateMicrosoftAD 
    , module Network.AWS.DirectoryService.CreateMicrosoftAD

    -- ** DeregisterEventTopic 
    , module Network.AWS.DirectoryService.DeregisterEventTopic

    -- ** CreateDirectory 
    , module Network.AWS.DirectoryService.CreateDirectory

    -- ** AcceptSharedDirectory 
    , module Network.AWS.DirectoryService.AcceptSharedDirectory

    -- ** CreateLogSubscription 
    , module Network.AWS.DirectoryService.CreateLogSubscription

    -- ** RemoveTagsFromResource 
    , module Network.AWS.DirectoryService.RemoveTagsFromResource

    -- ** DescribeEventTopics 
    , module Network.AWS.DirectoryService.DescribeEventTopics

    -- ** ResetUserPassword 
    , module Network.AWS.DirectoryService.ResetUserPassword

    -- ** UpdateConditionalForwarder 
    , module Network.AWS.DirectoryService.UpdateConditionalForwarder

    -- ** DeleteConditionalForwarder 
    , module Network.AWS.DirectoryService.DeleteConditionalForwarder

    -- ** DisableLDAPS 
    , module Network.AWS.DirectoryService.DisableLDAPS

    -- ** DeleteLogSubscription 
    , module Network.AWS.DirectoryService.DeleteLogSubscription

    -- ** EnableSso 
    , module Network.AWS.DirectoryService.EnableSso

    -- ** CancelSchemaExtension 
    , module Network.AWS.DirectoryService.CancelSchemaExtension

    -- ** ListLogSubscriptions (Paginated)
    , module Network.AWS.DirectoryService.ListLogSubscriptions

    -- ** EnableRadius 
    , module Network.AWS.DirectoryService.EnableRadius

    -- ** ListIpRoutes (Paginated)
    , module Network.AWS.DirectoryService.ListIpRoutes

    -- ** AddTagsToResource 
    , module Network.AWS.DirectoryService.AddTagsToResource

    -- ** ListSchemaExtensions (Paginated)
    , module Network.AWS.DirectoryService.ListSchemaExtensions

    -- ** DisableRadius 
    , module Network.AWS.DirectoryService.DisableRadius

    -- ** ListCertificates 
    , module Network.AWS.DirectoryService.ListCertificates

    -- ** RejectSharedDirectory 
    , module Network.AWS.DirectoryService.RejectSharedDirectory

    -- ** UnshareDirectory 
    , module Network.AWS.DirectoryService.UnshareDirectory

    -- ** RestoreFromSnapshot 
    , module Network.AWS.DirectoryService.RestoreFromSnapshot

    -- ** DescribeDomainControllers (Paginated)
    , module Network.AWS.DirectoryService.DescribeDomainControllers

    -- ** DescribeSnapshots (Paginated)
    , module Network.AWS.DirectoryService.DescribeSnapshots

    -- ** RemoveIpRoutes 
    , module Network.AWS.DirectoryService.RemoveIpRoutes

    -- ** DeleteSnapshot 
    , module Network.AWS.DirectoryService.DeleteSnapshot

    -- ** DeregisterCertificate 
    , module Network.AWS.DirectoryService.DeregisterCertificate

    -- ** StartSchemaExtension 
    , module Network.AWS.DirectoryService.StartSchemaExtension

    -- ** CreateTrust 
    , module Network.AWS.DirectoryService.CreateTrust

    -- ** DeleteDirectory 
    , module Network.AWS.DirectoryService.DeleteDirectory

    -- ** CreateSnapshot 
    , module Network.AWS.DirectoryService.CreateSnapshot

    -- ** DescribeCertificate 
    , module Network.AWS.DirectoryService.DescribeCertificate

    -- ** CreateComputer 
    , module Network.AWS.DirectoryService.CreateComputer

    -- ** DescribeSharedDirectories (Paginated)
    , module Network.AWS.DirectoryService.DescribeSharedDirectories

    -- ** EnableLDAPS 
    , module Network.AWS.DirectoryService.EnableLDAPS

    -- ** DisableSso 
    , module Network.AWS.DirectoryService.DisableSso

    -- ** VerifyTrust 
    , module Network.AWS.DirectoryService.VerifyTrust

    -- ** RemoveRegion 
    , module Network.AWS.DirectoryService.RemoveRegion

    -- ** CreateConditionalForwarder 
    , module Network.AWS.DirectoryService.CreateConditionalForwarder

    -- ** DescribeRegions 
    , module Network.AWS.DirectoryService.DescribeRegions

    -- ** AddRegion 
    , module Network.AWS.DirectoryService.AddRegion

    -- ** GetDirectoryLimits 
    , module Network.AWS.DirectoryService.GetDirectoryLimits

    -- ** UpdateRadius 
    , module Network.AWS.DirectoryService.UpdateRadius

    -- * Types

    -- ** RequestId
    , RequestId (..)

    -- ** DomainController
    , DomainController (..)
    , mkDomainController
    , dcAvailabilityZone
    , dcDirectoryId
    , dcDnsIpAddr
    , dcDomainControllerId
    , dcLaunchTime
    , dcStatus
    , dcStatusLastUpdatedDateTime
    , dcStatusReason
    , dcSubnetId
    , dcVpcId

    -- ** DirectoryShortName
    , DirectoryShortName (..)

    -- ** Trust
    , Trust (..)
    , mkTrust
    , tCreatedDateTime
    , tDirectoryId
    , tLastUpdatedDateTime
    , tRemoteDomainName
    , tSelectiveAuth
    , tStateLastUpdatedDateTime
    , tTrustDirection
    , tTrustId
    , tTrustState
    , tTrustStateReason
    , tTrustType

    -- ** RadiusStatus
    , RadiusStatus (..)

    -- ** TargetId
    , TargetId (..)

    -- ** Attribute
    , Attribute (..)
    , mkAttribute
    , aName
    , aValue

    -- ** Snapshot
    , Snapshot (..)
    , mkSnapshot
    , sDirectoryId
    , sName
    , sSnapshotId
    , sStartTime
    , sStatus
    , sType

    -- ** RadiusDisplayLabel
    , RadiusDisplayLabel (..)

    -- ** DirectoryId
    , DirectoryId (..)

    -- ** DirectoryLimits
    , DirectoryLimits (..)
    , mkDirectoryLimits
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesLimitReached
    , dlCloudOnlyMicrosoftADCurrentCount
    , dlCloudOnlyMicrosoftADLimit
    , dlCloudOnlyMicrosoftADLimitReached
    , dlConnectedDirectoriesCurrentCount
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached

    -- ** AccessUrl
    , AccessUrl (..)

    -- ** DirectoryVpcSettingsDescription
    , DirectoryVpcSettingsDescription (..)
    , mkDirectoryVpcSettingsDescription
    , dvsdAvailabilityZones
    , dvsdSecurityGroupId
    , dvsdSubnetIds
    , dvsdVpcId

    -- ** Computer
    , Computer (..)
    , mkComputer
    , cComputerAttributes
    , cComputerId
    , cComputerName

    -- ** RegionName
    , RegionName (..)

    -- ** ResourceId
    , ResourceId (..)

    -- ** DirectoryDescription
    , DirectoryDescription (..)
    , mkDirectoryDescription
    , ddAccessUrl
    , ddAlias
    , ddConnectSettings
    , ddDescription
    , ddDesiredNumberOfDomainControllers
    , ddDirectoryId
    , ddDnsIpAddrs
    , ddEdition
    , ddLaunchTime
    , ddName
    , ddOwnerDirectoryDescription
    , ddRadiusSettings
    , ddRadiusStatus
    , ddRegionsInfo
    , ddShareMethod
    , ddShareNotes
    , ddShareStatus
    , ddShortName
    , ddSize
    , ddSsoEnabled
    , ddStage
    , ddStageLastUpdatedDateTime
    , ddStageReason
    , ddType
    , ddVpcSettings

    -- ** Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- ** CustomerUserName
    , CustomerUserName (..)

    -- ** IpAddr
    , IpAddr (..)

    -- ** OrganizationalUnitDN
    , OrganizationalUnitDN (..)

    -- ** CertificateState
    , CertificateState (..)

    -- ** SchemaExtensionId
    , SchemaExtensionId (..)

    -- ** RegionsInfo
    , RegionsInfo (..)
    , mkRegionsInfo
    , riAdditionalRegions
    , riPrimaryRegion

    -- ** DirectoryStage
    , DirectoryStage (..)

    -- ** DirectoryEdition
    , DirectoryEdition (..)

    -- ** EventTopic
    , EventTopic (..)
    , mkEventTopic
    , etCreatedDateTime
    , etDirectoryId
    , etStatus
    , etTopicArn
    , etTopicName

    -- ** TopicStatus
    , TopicStatus (..)

    -- ** RadiusSettings
    , RadiusSettings (..)
    , mkRadiusSettings
    , rsAuthenticationProtocol
    , rsDisplayLabel
    , rsRadiusPort
    , rsRadiusRetries
    , rsRadiusServers
    , rsRadiusTimeout
    , rsSharedSecret
    , rsUseSameUsername

    -- ** CertificateId
    , CertificateId (..)

    -- ** VpcId
    , VpcId (..)

    -- ** SnapshotStatus
    , SnapshotStatus (..)

    -- ** SchemaExtensionStatusReason
    , SchemaExtensionStatusReason (..)

    -- ** CertificateCN
    , CertificateCN (..)

    -- ** RegionType
    , RegionType (..)

    -- ** DomainControllerStatus
    , DomainControllerStatus (..)

    -- ** IpRouteStatusReason
    , IpRouteStatusReason (..)

    -- ** SharedDirectory
    , SharedDirectory (..)
    , mkSharedDirectory
    , sdCreatedDateTime
    , sdLastUpdatedDateTime
    , sdOwnerAccountId
    , sdOwnerDirectoryId
    , sdShareMethod
    , sdShareNotes
    , sdShareStatus
    , sdSharedAccountId
    , sdSharedDirectoryId

    -- ** TrustState
    , TrustState (..)

    -- ** RegionDescription
    , RegionDescription (..)
    , mkRegionDescription
    , rdDesiredNumberOfDomainControllers
    , rdDirectoryId
    , rdLastUpdatedDateTime
    , rdLaunchTime
    , rdRegionName
    , rdRegionType
    , rdStatus
    , rdStatusLastUpdatedDateTime
    , rdVpcSettings

    -- ** SchemaExtensionInfo
    , SchemaExtensionInfo (..)
    , mkSchemaExtensionInfo
    , seiDescription
    , seiDirectoryId
    , seiEndDateTime
    , seiSchemaExtensionId
    , seiSchemaExtensionStatus
    , seiSchemaExtensionStatusReason
    , seiStartDateTime

    -- ** CertificateStateReason
    , CertificateStateReason (..)

    -- ** UserName
    , UserName (..)

    -- ** IpRouteInfo
    , IpRouteInfo (..)
    , mkIpRouteInfo
    , iriAddedDateTime
    , iriCidrIp
    , iriDescription
    , iriDirectoryId
    , iriIpRouteStatusMsg
    , iriIpRouteStatusReason

    -- ** TopicName
    , TopicName (..)

    -- ** RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- ** SubnetId
    , SubnetId (..)

    -- ** ConnectPassword
    , ConnectPassword (..)

    -- ** CustomerId
    , CustomerId (..)

    -- ** TrustDirection
    , TrustDirection (..)

    -- ** DirectoryType
    , DirectoryType (..)

    -- ** TargetType
    , TargetType (..)

    -- ** AliasName
    , AliasName (..)

    -- ** ShareStatus
    , ShareStatus (..)

    -- ** LogGroupName
    , LogGroupName (..)

    -- ** SecurityGroupId
    , SecurityGroupId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** TopicArn
    , TopicArn (..)

    -- ** SnapshotType
    , SnapshotType (..)

    -- ** ShareTarget
    , ShareTarget (..)
    , mkShareTarget
    , stId
    , stType

    -- ** SchemaExtensionStatus
    , SchemaExtensionStatus (..)

    -- ** DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription (..)
    , mkDirectoryConnectSettingsDescription
    , dcsdAvailabilityZones
    , dcsdConnectIps
    , dcsdCustomerUserName
    , dcsdSecurityGroupId
    , dcsdSubnetIds
    , dcsdVpcId

    -- ** DirectorySize
    , DirectorySize (..)

    -- ** TrustType
    , TrustType (..)

    -- ** UnshareTarget
    , UnshareTarget (..)
    , mkUnshareTarget
    , utId
    , utType

    -- ** AvailabilityZone
    , AvailabilityZone (..)

    -- ** TrustStateReason
    , TrustStateReason (..)

    -- ** Password
    , Password (..)

    -- ** LogSubscription
    , LogSubscription (..)
    , mkLogSubscription
    , lsDirectoryId
    , lsLogGroupName
    , lsSubscriptionCreatedDateTime

    -- ** ConditionalForwarder
    , ConditionalForwarder (..)
    , mkConditionalForwarder
    , cfDnsIpAddrs
    , cfRemoteDomainName
    , cfReplicationScope

    -- ** DirectoryConnectSettings
    , DirectoryConnectSettings (..)
    , mkDirectoryConnectSettings
    , dcsVpcId
    , dcsSubnetIds
    , dcsCustomerDnsIps
    , dcsCustomerUserName

    -- ** ShareMethod
    , ShareMethod (..)

    -- ** LDAPSSettingInfo
    , LDAPSSettingInfo (..)
    , mkLDAPSSettingInfo
    , ldapssiLDAPSStatus
    , ldapssiLDAPSStatusReason
    , ldapssiLastUpdatedDateTime

    -- ** LDAPSStatusReason
    , LDAPSStatusReason (..)

    -- ** SelectiveAuth
    , SelectiveAuth (..)

    -- ** SnapshotLimits
    , SnapshotLimits (..)
    , mkSnapshotLimits
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit
    , slManualSnapshotsLimitReached

    -- ** LDAPSType
    , LDAPSType (..)

    -- ** CidrIp
    , CidrIp (..)

    -- ** IpRouteStatusMsg
    , IpRouteStatusMsg (..)

    -- ** Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateId
    , cCommonName
    , cExpiryDateTime
    , cRegisteredDateTime
    , cState
    , cStateReason

    -- ** CertificateData
    , CertificateData (..)

    -- ** TagKey
    , TagKey (..)

    -- ** Server
    , Server (..)

    -- ** IpRoute
    , IpRoute (..)
    , mkIpRoute
    , irCidrIp
    , irDescription

    -- ** ComputerName
    , ComputerName (..)

    -- ** StageReason
    , StageReason (..)

    -- ** RemoteDomainName
    , RemoteDomainName (..)

    -- ** CertificateInfo
    , CertificateInfo (..)
    , mkCertificateInfo
    , ciCertificateId
    , ciCommonName
    , ciExpiryDateTime
    , ciState

    -- ** TrustPassword
    , TrustPassword (..)

    -- ** LDAPSStatus
    , LDAPSStatus (..)

    -- ** LdifContent
    , LdifContent (..)

    -- ** OwnerDirectoryDescription
    , OwnerDirectoryDescription (..)
    , mkOwnerDirectoryDescription
    , oddAccountId
    , oddDirectoryId
    , oddDnsIpAddrs
    , oddRadiusSettings
    , oddRadiusStatus
    , oddVpcSettings

    -- ** Description
    , Description (..)

    -- ** ReplicationScope
    , ReplicationScope (..)

    -- ** DirectoryName
    , DirectoryName (..)

    -- ** TrustId
    , TrustId (..)

    -- ** SnapshotId
    , SnapshotId (..)

    -- ** DirectoryVpcSettings
    , DirectoryVpcSettings (..)
    , mkDirectoryVpcSettings
    , dvsVpcId
    , dvsSubnetIds

    -- ** DomainControllerId
    , DomainControllerId (..)

    -- ** DnsIpAddr
    , DnsIpAddr (..)

    -- ** StatusReason
    , StatusReason (..)

    -- ** Name
    , Name (..)

    -- ** Value
    , Value (..)

    -- ** ComputerId
    , ComputerId (..)

    -- ** Alias
    , Alias (..)

    -- ** ShareNotes
    , ShareNotes (..)

    -- ** Key
    , Key (..)

    -- ** NewPassword
    , NewPassword (..)

    -- ** SharedSecret
    , SharedSecret (..)

    -- ** OwnerAccountId
    , OwnerAccountId (..)

    -- ** SharedAccountId
    , SharedAccountId (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Waiters
import Network.AWS.DirectoryService.ShareDirectory
import Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
import Network.AWS.DirectoryService.DescribeConditionalForwarders
import Network.AWS.DirectoryService.GetSnapshotLimits
import Network.AWS.DirectoryService.RegisterEventTopic
import Network.AWS.DirectoryService.RegisterCertificate
import Network.AWS.DirectoryService.ConnectDirectory
import Network.AWS.DirectoryService.DescribeLDAPSSettings
import Network.AWS.DirectoryService.CreateAlias
import Network.AWS.DirectoryService.DescribeDirectories
import Network.AWS.DirectoryService.AddIpRoutes
import Network.AWS.DirectoryService.ListTagsForResource
import Network.AWS.DirectoryService.DescribeTrusts
import Network.AWS.DirectoryService.DeleteTrust
import Network.AWS.DirectoryService.UpdateTrust
import Network.AWS.DirectoryService.CreateMicrosoftAD
import Network.AWS.DirectoryService.DeregisterEventTopic
import Network.AWS.DirectoryService.CreateDirectory
import Network.AWS.DirectoryService.AcceptSharedDirectory
import Network.AWS.DirectoryService.CreateLogSubscription
import Network.AWS.DirectoryService.RemoveTagsFromResource
import Network.AWS.DirectoryService.DescribeEventTopics
import Network.AWS.DirectoryService.ResetUserPassword
import Network.AWS.DirectoryService.UpdateConditionalForwarder
import Network.AWS.DirectoryService.DeleteConditionalForwarder
import Network.AWS.DirectoryService.DisableLDAPS
import Network.AWS.DirectoryService.DeleteLogSubscription
import Network.AWS.DirectoryService.EnableSso
import Network.AWS.DirectoryService.CancelSchemaExtension
import Network.AWS.DirectoryService.ListLogSubscriptions
import Network.AWS.DirectoryService.EnableRadius
import Network.AWS.DirectoryService.ListIpRoutes
import Network.AWS.DirectoryService.AddTagsToResource
import Network.AWS.DirectoryService.ListSchemaExtensions
import Network.AWS.DirectoryService.DisableRadius
import Network.AWS.DirectoryService.ListCertificates
import Network.AWS.DirectoryService.RejectSharedDirectory
import Network.AWS.DirectoryService.UnshareDirectory
import Network.AWS.DirectoryService.RestoreFromSnapshot
import Network.AWS.DirectoryService.DescribeDomainControllers
import Network.AWS.DirectoryService.DescribeSnapshots
import Network.AWS.DirectoryService.RemoveIpRoutes
import Network.AWS.DirectoryService.DeleteSnapshot
import Network.AWS.DirectoryService.DeregisterCertificate
import Network.AWS.DirectoryService.StartSchemaExtension
import Network.AWS.DirectoryService.CreateTrust
import Network.AWS.DirectoryService.DeleteDirectory
import Network.AWS.DirectoryService.CreateSnapshot
import Network.AWS.DirectoryService.DescribeCertificate
import Network.AWS.DirectoryService.CreateComputer
import Network.AWS.DirectoryService.DescribeSharedDirectories
import Network.AWS.DirectoryService.EnableLDAPS
import Network.AWS.DirectoryService.DisableSso
import Network.AWS.DirectoryService.VerifyTrust
import Network.AWS.DirectoryService.RemoveRegion
import Network.AWS.DirectoryService.CreateConditionalForwarder
import Network.AWS.DirectoryService.DescribeRegions
import Network.AWS.DirectoryService.AddRegion
import Network.AWS.DirectoryService.GetDirectoryLimits
import Network.AWS.DirectoryService.UpdateRadius
import qualified Network.AWS.Prelude as Lude

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
