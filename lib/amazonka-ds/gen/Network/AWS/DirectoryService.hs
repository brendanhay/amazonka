{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
  ( -- * Service configuration
    directoryServiceService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ShareDirectory
    module Network.AWS.DirectoryService.ShareDirectory,

    -- ** UpdateNumberOfDomainControllers
    module Network.AWS.DirectoryService.UpdateNumberOfDomainControllers,

    -- ** DescribeConditionalForwarders
    module Network.AWS.DirectoryService.DescribeConditionalForwarders,

    -- ** GetSnapshotLimits
    module Network.AWS.DirectoryService.GetSnapshotLimits,

    -- ** RegisterEventTopic
    module Network.AWS.DirectoryService.RegisterEventTopic,

    -- ** RegisterCertificate
    module Network.AWS.DirectoryService.RegisterCertificate,

    -- ** ConnectDirectory
    module Network.AWS.DirectoryService.ConnectDirectory,

    -- ** DescribeLDAPSSettings
    module Network.AWS.DirectoryService.DescribeLDAPSSettings,

    -- ** CreateAlias
    module Network.AWS.DirectoryService.CreateAlias,

    -- ** DescribeDirectories (Paginated)
    module Network.AWS.DirectoryService.DescribeDirectories,

    -- ** AddIPRoutes
    module Network.AWS.DirectoryService.AddIPRoutes,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.DirectoryService.ListTagsForResource,

    -- ** DescribeTrusts (Paginated)
    module Network.AWS.DirectoryService.DescribeTrusts,

    -- ** DeleteTrust
    module Network.AWS.DirectoryService.DeleteTrust,

    -- ** UpdateTrust
    module Network.AWS.DirectoryService.UpdateTrust,

    -- ** CreateMicrosoftAD
    module Network.AWS.DirectoryService.CreateMicrosoftAD,

    -- ** DeregisterEventTopic
    module Network.AWS.DirectoryService.DeregisterEventTopic,

    -- ** CreateDirectory
    module Network.AWS.DirectoryService.CreateDirectory,

    -- ** AcceptSharedDirectory
    module Network.AWS.DirectoryService.AcceptSharedDirectory,

    -- ** CreateLogSubscription
    module Network.AWS.DirectoryService.CreateLogSubscription,

    -- ** RemoveTagsFromResource
    module Network.AWS.DirectoryService.RemoveTagsFromResource,

    -- ** DescribeEventTopics
    module Network.AWS.DirectoryService.DescribeEventTopics,

    -- ** ResetUserPassword
    module Network.AWS.DirectoryService.ResetUserPassword,

    -- ** UpdateConditionalForwarder
    module Network.AWS.DirectoryService.UpdateConditionalForwarder,

    -- ** DeleteConditionalForwarder
    module Network.AWS.DirectoryService.DeleteConditionalForwarder,

    -- ** DisableLDAPS
    module Network.AWS.DirectoryService.DisableLDAPS,

    -- ** DeleteLogSubscription
    module Network.AWS.DirectoryService.DeleteLogSubscription,

    -- ** EnableSSO
    module Network.AWS.DirectoryService.EnableSSO,

    -- ** CancelSchemaExtension
    module Network.AWS.DirectoryService.CancelSchemaExtension,

    -- ** ListLogSubscriptions (Paginated)
    module Network.AWS.DirectoryService.ListLogSubscriptions,

    -- ** EnableRadius
    module Network.AWS.DirectoryService.EnableRadius,

    -- ** ListIPRoutes (Paginated)
    module Network.AWS.DirectoryService.ListIPRoutes,

    -- ** AddTagsToResource
    module Network.AWS.DirectoryService.AddTagsToResource,

    -- ** ListSchemaExtensions (Paginated)
    module Network.AWS.DirectoryService.ListSchemaExtensions,

    -- ** DisableRadius
    module Network.AWS.DirectoryService.DisableRadius,

    -- ** ListCertificates
    module Network.AWS.DirectoryService.ListCertificates,

    -- ** RejectSharedDirectory
    module Network.AWS.DirectoryService.RejectSharedDirectory,

    -- ** UnshareDirectory
    module Network.AWS.DirectoryService.UnshareDirectory,

    -- ** RestoreFromSnapshot
    module Network.AWS.DirectoryService.RestoreFromSnapshot,

    -- ** DescribeDomainControllers (Paginated)
    module Network.AWS.DirectoryService.DescribeDomainControllers,

    -- ** DescribeSnapshots (Paginated)
    module Network.AWS.DirectoryService.DescribeSnapshots,

    -- ** RemoveIPRoutes
    module Network.AWS.DirectoryService.RemoveIPRoutes,

    -- ** DeleteSnapshot
    module Network.AWS.DirectoryService.DeleteSnapshot,

    -- ** DeregisterCertificate
    module Network.AWS.DirectoryService.DeregisterCertificate,

    -- ** StartSchemaExtension
    module Network.AWS.DirectoryService.StartSchemaExtension,

    -- ** CreateTrust
    module Network.AWS.DirectoryService.CreateTrust,

    -- ** DeleteDirectory
    module Network.AWS.DirectoryService.DeleteDirectory,

    -- ** CreateSnapshot
    module Network.AWS.DirectoryService.CreateSnapshot,

    -- ** DescribeCertificate
    module Network.AWS.DirectoryService.DescribeCertificate,

    -- ** CreateComputer
    module Network.AWS.DirectoryService.CreateComputer,

    -- ** DescribeSharedDirectories (Paginated)
    module Network.AWS.DirectoryService.DescribeSharedDirectories,

    -- ** EnableLDAPS
    module Network.AWS.DirectoryService.EnableLDAPS,

    -- ** DisableSSO
    module Network.AWS.DirectoryService.DisableSSO,

    -- ** VerifyTrust
    module Network.AWS.DirectoryService.VerifyTrust,

    -- ** RemoveRegion
    module Network.AWS.DirectoryService.RemoveRegion,

    -- ** CreateConditionalForwarder
    module Network.AWS.DirectoryService.CreateConditionalForwarder,

    -- ** DescribeRegions
    module Network.AWS.DirectoryService.DescribeRegions,

    -- ** AddRegion
    module Network.AWS.DirectoryService.AddRegion,

    -- ** GetDirectoryLimits
    module Network.AWS.DirectoryService.GetDirectoryLimits,

    -- ** UpdateRadius
    module Network.AWS.DirectoryService.UpdateRadius,

    -- * Types

    -- ** CertificateState
    CertificateState (..),

    -- ** DirectoryEdition
    DirectoryEdition (..),

    -- ** DirectorySize
    DirectorySize (..),

    -- ** DirectoryStage
    DirectoryStage (..),

    -- ** DirectoryType
    DirectoryType (..),

    -- ** DomainControllerStatus
    DomainControllerStatus (..),

    -- ** IPRouteStatusMsg
    IPRouteStatusMsg (..),

    -- ** LDAPSStatus
    LDAPSStatus (..),

    -- ** LDAPSType
    LDAPSType (..),

    -- ** RadiusAuthenticationProtocol
    RadiusAuthenticationProtocol (..),

    -- ** RadiusStatus
    RadiusStatus (..),

    -- ** RegionType
    RegionType (..),

    -- ** ReplicationScope
    ReplicationScope (..),

    -- ** SchemaExtensionStatus
    SchemaExtensionStatus (..),

    -- ** SelectiveAuth
    SelectiveAuth (..),

    -- ** ShareMethod
    ShareMethod (..),

    -- ** ShareStatus
    ShareStatus (..),

    -- ** SnapshotStatus
    SnapshotStatus (..),

    -- ** SnapshotType
    SnapshotType (..),

    -- ** TargetType
    TargetType (..),

    -- ** TopicStatus
    TopicStatus (..),

    -- ** TrustDirection
    TrustDirection (..),

    -- ** TrustState
    TrustState (..),

    -- ** TrustType
    TrustType (..),

    -- ** Attribute
    Attribute (..),
    mkAttribute,
    aValue,
    aName,

    -- ** Certificate
    Certificate (..),
    mkCertificate,
    cState,
    cCommonName,
    cCertificateId,
    cExpiryDateTime,
    cRegisteredDateTime,
    cStateReason,

    -- ** CertificateInfo
    CertificateInfo (..),
    mkCertificateInfo,
    ciState,
    ciCommonName,
    ciCertificateId,
    ciExpiryDateTime,

    -- ** Computer
    Computer (..),
    mkComputer,
    cComputerId,
    cComputerAttributes,
    cComputerName,

    -- ** ConditionalForwarder
    ConditionalForwarder (..),
    mkConditionalForwarder,
    cfDNSIPAddrs,
    cfRemoteDomainName,
    cfReplicationScope,

    -- ** DirectoryConnectSettings
    DirectoryConnectSettings (..),
    mkDirectoryConnectSettings,
    dcsVPCId,
    dcsSubnetIds,
    dcsCustomerDNSIPs,
    dcsCustomerUserName,

    -- ** DirectoryConnectSettingsDescription
    DirectoryConnectSettingsDescription (..),
    mkDirectoryConnectSettingsDescription,
    dcsdCustomerUserName,
    dcsdSubnetIds,
    dcsdVPCId,
    dcsdSecurityGroupId,
    dcsdConnectIPs,
    dcsdAvailabilityZones,

    -- ** DirectoryDescription
    DirectoryDescription (..),
    mkDirectoryDescription,
    ddEdition,
    ddRadiusStatus,
    ddStage,
    ddDirectoryId,
    ddAccessURL,
    ddShortName,
    ddRegionsInfo,
    ddSize,
    ddDesiredNumberOfDomainControllers,
    ddRadiusSettings,
    ddLaunchTime,
    ddAlias,
    ddShareStatus,
    ddName,
    ddShareMethod,
    ddStageLastUpdatedDateTime,
    ddSSOEnabled,
    ddDNSIPAddrs,
    ddVPCSettings,
    ddType,
    ddStageReason,
    ddConnectSettings,
    ddOwnerDirectoryDescription,
    ddDescription,
    ddShareNotes,

    -- ** DirectoryLimits
    DirectoryLimits (..),
    mkDirectoryLimits,
    dlConnectedDirectoriesCurrentCount,
    dlCloudOnlyMicrosoftADLimitReached,
    dlConnectedDirectoriesLimit,
    dlConnectedDirectoriesLimitReached,
    dlCloudOnlyMicrosoftADLimit,
    dlCloudOnlyDirectoriesLimit,
    dlCloudOnlyDirectoriesCurrentCount,
    dlCloudOnlyDirectoriesLimitReached,
    dlCloudOnlyMicrosoftADCurrentCount,

    -- ** DirectoryVPCSettings
    DirectoryVPCSettings (..),
    mkDirectoryVPCSettings,
    dvsVPCId,
    dvsSubnetIds,

    -- ** DirectoryVPCSettingsDescription
    DirectoryVPCSettingsDescription (..),
    mkDirectoryVPCSettingsDescription,
    dvsdSubnetIds,
    dvsdVPCId,
    dvsdSecurityGroupId,
    dvsdAvailabilityZones,

    -- ** DomainController
    DomainController (..),
    mkDomainController,
    dcStatus,
    dcDirectoryId,
    dcVPCId,
    dcLaunchTime,
    dcSubnetId,
    dcAvailabilityZone,
    dcStatusLastUpdatedDateTime,
    dcStatusReason,
    dcDNSIPAddr,
    dcDomainControllerId,

    -- ** EventTopic
    EventTopic (..),
    mkEventTopic,
    etStatus,
    etDirectoryId,
    etTopicName,
    etTopicARN,
    etCreatedDateTime,

    -- ** IPRoute
    IPRoute (..),
    mkIPRoute,
    irCidrIP,
    irDescription,

    -- ** IPRouteInfo
    IPRouteInfo (..),
    mkIPRouteInfo,
    iriDirectoryId,
    iriIPRouteStatusReason,
    iriAddedDateTime,
    iriCidrIP,
    iriIPRouteStatusMsg,
    iriDescription,

    -- ** LDAPSSettingInfo
    LDAPSSettingInfo (..),
    mkLDAPSSettingInfo,
    ldapssiLastUpdatedDateTime,
    ldapssiLDAPSStatusReason,
    ldapssiLDAPSStatus,

    -- ** LogSubscription
    LogSubscription (..),
    mkLogSubscription,
    lsDirectoryId,
    lsLogGroupName,
    lsSubscriptionCreatedDateTime,

    -- ** OwnerDirectoryDescription
    OwnerDirectoryDescription (..),
    mkOwnerDirectoryDescription,
    oddRadiusStatus,
    oddDirectoryId,
    oddRadiusSettings,
    oddAccountId,
    oddDNSIPAddrs,
    oddVPCSettings,

    -- ** RadiusSettings
    RadiusSettings (..),
    mkRadiusSettings,
    rsDisplayLabel,
    rsRadiusRetries,
    rsAuthenticationProtocol,
    rsRadiusServers,
    rsUseSameUsername,
    rsSharedSecret,
    rsRadiusTimeout,
    rsRadiusPort,

    -- ** RegionDescription
    RegionDescription (..),
    mkRegionDescription,
    rdStatus,
    rdDirectoryId,
    rdRegionName,
    rdDesiredNumberOfDomainControllers,
    rdRegionType,
    rdLaunchTime,
    rdLastUpdatedDateTime,
    rdStatusLastUpdatedDateTime,
    rdVPCSettings,

    -- ** RegionsInfo
    RegionsInfo (..),
    mkRegionsInfo,
    riPrimaryRegion,
    riAdditionalRegions,

    -- ** SchemaExtensionInfo
    SchemaExtensionInfo (..),
    mkSchemaExtensionInfo,
    seiDirectoryId,
    seiSchemaExtensionId,
    seiSchemaExtensionStatusReason,
    seiSchemaExtensionStatus,
    seiDescription,
    seiEndDateTime,
    seiStartDateTime,

    -- ** ShareTarget
    ShareTarget (..),
    mkShareTarget,
    stId,
    stType,

    -- ** SharedDirectory
    SharedDirectory (..),
    mkSharedDirectory,
    sSharedAccountId,
    sOwnerAccountId,
    sLastUpdatedDateTime,
    sShareStatus,
    sShareMethod,
    sOwnerDirectoryId,
    sSharedDirectoryId,
    sShareNotes,
    sCreatedDateTime,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sStatus,
    sDirectoryId,
    sStartTime,
    sName,
    sType,
    sSnapshotId,

    -- ** SnapshotLimits
    SnapshotLimits (..),
    mkSnapshotLimits,
    slManualSnapshotsLimitReached,
    slManualSnapshotsCurrentCount,
    slManualSnapshotsLimit,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** Trust
    Trust (..),
    mkTrust,
    tDirectoryId,
    tTrustState,
    tLastUpdatedDateTime,
    tTrustDirection,
    tStateLastUpdatedDateTime,
    tTrustType,
    tTrustStateReason,
    tSelectiveAuth,
    tRemoteDomainName,
    tTrustId,
    tCreatedDateTime,

    -- ** UnshareTarget
    UnshareTarget (..),
    mkUnshareTarget,
    utId,
    utType,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import Network.AWS.DirectoryService.AcceptSharedDirectory
import Network.AWS.DirectoryService.AddIPRoutes
import Network.AWS.DirectoryService.AddRegion
import Network.AWS.DirectoryService.AddTagsToResource
import Network.AWS.DirectoryService.CancelSchemaExtension
import Network.AWS.DirectoryService.ConnectDirectory
import Network.AWS.DirectoryService.CreateAlias
import Network.AWS.DirectoryService.CreateComputer
import Network.AWS.DirectoryService.CreateConditionalForwarder
import Network.AWS.DirectoryService.CreateDirectory
import Network.AWS.DirectoryService.CreateLogSubscription
import Network.AWS.DirectoryService.CreateMicrosoftAD
import Network.AWS.DirectoryService.CreateSnapshot
import Network.AWS.DirectoryService.CreateTrust
import Network.AWS.DirectoryService.DeleteConditionalForwarder
import Network.AWS.DirectoryService.DeleteDirectory
import Network.AWS.DirectoryService.DeleteLogSubscription
import Network.AWS.DirectoryService.DeleteSnapshot
import Network.AWS.DirectoryService.DeleteTrust
import Network.AWS.DirectoryService.DeregisterCertificate
import Network.AWS.DirectoryService.DeregisterEventTopic
import Network.AWS.DirectoryService.DescribeCertificate
import Network.AWS.DirectoryService.DescribeConditionalForwarders
import Network.AWS.DirectoryService.DescribeDirectories
import Network.AWS.DirectoryService.DescribeDomainControllers
import Network.AWS.DirectoryService.DescribeEventTopics
import Network.AWS.DirectoryService.DescribeLDAPSSettings
import Network.AWS.DirectoryService.DescribeRegions
import Network.AWS.DirectoryService.DescribeSharedDirectories
import Network.AWS.DirectoryService.DescribeSnapshots
import Network.AWS.DirectoryService.DescribeTrusts
import Network.AWS.DirectoryService.DisableLDAPS
import Network.AWS.DirectoryService.DisableRadius
import Network.AWS.DirectoryService.DisableSSO
import Network.AWS.DirectoryService.EnableLDAPS
import Network.AWS.DirectoryService.EnableRadius
import Network.AWS.DirectoryService.EnableSSO
import Network.AWS.DirectoryService.GetDirectoryLimits
import Network.AWS.DirectoryService.GetSnapshotLimits
import Network.AWS.DirectoryService.ListCertificates
import Network.AWS.DirectoryService.ListIPRoutes
import Network.AWS.DirectoryService.ListLogSubscriptions
import Network.AWS.DirectoryService.ListSchemaExtensions
import Network.AWS.DirectoryService.ListTagsForResource
import Network.AWS.DirectoryService.RegisterCertificate
import Network.AWS.DirectoryService.RegisterEventTopic
import Network.AWS.DirectoryService.RejectSharedDirectory
import Network.AWS.DirectoryService.RemoveIPRoutes
import Network.AWS.DirectoryService.RemoveRegion
import Network.AWS.DirectoryService.RemoveTagsFromResource
import Network.AWS.DirectoryService.ResetUserPassword
import Network.AWS.DirectoryService.RestoreFromSnapshot
import Network.AWS.DirectoryService.ShareDirectory
import Network.AWS.DirectoryService.StartSchemaExtension
import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.UnshareDirectory
import Network.AWS.DirectoryService.UpdateConditionalForwarder
import Network.AWS.DirectoryService.UpdateNumberOfDomainControllers
import Network.AWS.DirectoryService.UpdateRadius
import Network.AWS.DirectoryService.UpdateTrust
import Network.AWS.DirectoryService.VerifyTrust
import Network.AWS.DirectoryService.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DirectoryService'.

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
