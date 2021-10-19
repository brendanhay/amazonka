{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.DirectoryService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-04-16@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Directory Service
--
-- Directory Service is a web service that makes it easy for you to setup
-- and run directories in the Amazon Web Services cloud, or connect your
-- Amazon Web Services resources with an existing self-managed Microsoft
-- Active Directory. This guide provides detailed information about
-- Directory Service operations, data types, parameters, and errors. For
-- information about Directory Services features, see
-- <https://aws.amazon.com/directoryservice/ Directory Service> and the
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html Directory Service Administration Guide>.
--
-- Amazon Web Services provides SDKs that consist of libraries and sample
-- code for various programming languages and platforms (Java, Ruby, .Net,
-- iOS, Android, etc.). The SDKs provide a convenient way to create
-- programmatic access to Directory Service and other Amazon Web Services
-- services. For more information about the Amazon Web Services SDKs,
-- including how to download and install them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
module Network.AWS.DirectoryService
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CertificateLimitExceededException
    _CertificateLimitExceededException,

    -- ** CertificateAlreadyExistsException
    _CertificateAlreadyExistsException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DirectoryUnavailableException
    _DirectoryUnavailableException,

    -- ** AuthenticationFailedException
    _AuthenticationFailedException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** NoAvailableCertificateException
    _NoAvailableCertificateException,

    -- ** UserDoesNotExistException
    _UserDoesNotExistException,

    -- ** DirectoryLimitExceededException
    _DirectoryLimitExceededException,

    -- ** InvalidLDAPSStatusException
    _InvalidLDAPSStatusException,

    -- ** InvalidCertificateException
    _InvalidCertificateException,

    -- ** CertificateInUseException
    _CertificateInUseException,

    -- ** RegionLimitExceededException
    _RegionLimitExceededException,

    -- ** IpRouteLimitExceededException
    _IpRouteLimitExceededException,

    -- ** ShareLimitExceededException
    _ShareLimitExceededException,

    -- ** EntityDoesNotExistException
    _EntityDoesNotExistException,

    -- ** OrganizationsException
    _OrganizationsException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** DirectoryAlreadyInRegionException
    _DirectoryAlreadyInRegionException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** DirectoryNotSharedException
    _DirectoryNotSharedException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** ServiceException
    _ServiceException,

    -- ** SnapshotLimitExceededException
    _SnapshotLimitExceededException,

    -- ** DomainControllerLimitExceededException
    _DomainControllerLimitExceededException,

    -- ** DirectoryDoesNotExistException
    _DirectoryDoesNotExistException,

    -- ** InvalidClientAuthStatusException
    _InvalidClientAuthStatusException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** ClientException
    _ClientException,

    -- ** DirectoryAlreadySharedException
    _DirectoryAlreadySharedException,

    -- ** CertificateDoesNotExistException
    _CertificateDoesNotExistException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ShareDirectory
    ShareDirectory (ShareDirectory'),
    newShareDirectory,
    ShareDirectoryResponse (ShareDirectoryResponse'),
    newShareDirectoryResponse,

    -- ** UpdateNumberOfDomainControllers
    UpdateNumberOfDomainControllers (UpdateNumberOfDomainControllers'),
    newUpdateNumberOfDomainControllers,
    UpdateNumberOfDomainControllersResponse (UpdateNumberOfDomainControllersResponse'),
    newUpdateNumberOfDomainControllersResponse,

    -- ** DescribeConditionalForwarders
    DescribeConditionalForwarders (DescribeConditionalForwarders'),
    newDescribeConditionalForwarders,
    DescribeConditionalForwardersResponse (DescribeConditionalForwardersResponse'),
    newDescribeConditionalForwardersResponse,

    -- ** GetSnapshotLimits
    GetSnapshotLimits (GetSnapshotLimits'),
    newGetSnapshotLimits,
    GetSnapshotLimitsResponse (GetSnapshotLimitsResponse'),
    newGetSnapshotLimitsResponse,

    -- ** RegisterEventTopic
    RegisterEventTopic (RegisterEventTopic'),
    newRegisterEventTopic,
    RegisterEventTopicResponse (RegisterEventTopicResponse'),
    newRegisterEventTopicResponse,

    -- ** RegisterCertificate
    RegisterCertificate (RegisterCertificate'),
    newRegisterCertificate,
    RegisterCertificateResponse (RegisterCertificateResponse'),
    newRegisterCertificateResponse,

    -- ** ConnectDirectory
    ConnectDirectory (ConnectDirectory'),
    newConnectDirectory,
    ConnectDirectoryResponse (ConnectDirectoryResponse'),
    newConnectDirectoryResponse,

    -- ** DescribeLDAPSSettings
    DescribeLDAPSSettings (DescribeLDAPSSettings'),
    newDescribeLDAPSSettings,
    DescribeLDAPSSettingsResponse (DescribeLDAPSSettingsResponse'),
    newDescribeLDAPSSettingsResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** DescribeDirectories (Paginated)
    DescribeDirectories (DescribeDirectories'),
    newDescribeDirectories,
    DescribeDirectoriesResponse (DescribeDirectoriesResponse'),
    newDescribeDirectoriesResponse,

    -- ** AddIpRoutes
    AddIpRoutes (AddIpRoutes'),
    newAddIpRoutes,
    AddIpRoutesResponse (AddIpRoutesResponse'),
    newAddIpRoutesResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DescribeTrusts (Paginated)
    DescribeTrusts (DescribeTrusts'),
    newDescribeTrusts,
    DescribeTrustsResponse (DescribeTrustsResponse'),
    newDescribeTrustsResponse,

    -- ** DeleteTrust
    DeleteTrust (DeleteTrust'),
    newDeleteTrust,
    DeleteTrustResponse (DeleteTrustResponse'),
    newDeleteTrustResponse,

    -- ** UpdateTrust
    UpdateTrust (UpdateTrust'),
    newUpdateTrust,
    UpdateTrustResponse (UpdateTrustResponse'),
    newUpdateTrustResponse,

    -- ** CreateMicrosoftAD
    CreateMicrosoftAD (CreateMicrosoftAD'),
    newCreateMicrosoftAD,
    CreateMicrosoftADResponse (CreateMicrosoftADResponse'),
    newCreateMicrosoftADResponse,

    -- ** DisableClientAuthentication
    DisableClientAuthentication (DisableClientAuthentication'),
    newDisableClientAuthentication,
    DisableClientAuthenticationResponse (DisableClientAuthenticationResponse'),
    newDisableClientAuthenticationResponse,

    -- ** DeregisterEventTopic
    DeregisterEventTopic (DeregisterEventTopic'),
    newDeregisterEventTopic,
    DeregisterEventTopicResponse (DeregisterEventTopicResponse'),
    newDeregisterEventTopicResponse,

    -- ** CreateDirectory
    CreateDirectory (CreateDirectory'),
    newCreateDirectory,
    CreateDirectoryResponse (CreateDirectoryResponse'),
    newCreateDirectoryResponse,

    -- ** AcceptSharedDirectory
    AcceptSharedDirectory (AcceptSharedDirectory'),
    newAcceptSharedDirectory,
    AcceptSharedDirectoryResponse (AcceptSharedDirectoryResponse'),
    newAcceptSharedDirectoryResponse,

    -- ** CreateLogSubscription
    CreateLogSubscription (CreateLogSubscription'),
    newCreateLogSubscription,
    CreateLogSubscriptionResponse (CreateLogSubscriptionResponse'),
    newCreateLogSubscriptionResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** DescribeEventTopics
    DescribeEventTopics (DescribeEventTopics'),
    newDescribeEventTopics,
    DescribeEventTopicsResponse (DescribeEventTopicsResponse'),
    newDescribeEventTopicsResponse,

    -- ** ResetUserPassword
    ResetUserPassword (ResetUserPassword'),
    newResetUserPassword,
    ResetUserPasswordResponse (ResetUserPasswordResponse'),
    newResetUserPasswordResponse,

    -- ** UpdateConditionalForwarder
    UpdateConditionalForwarder (UpdateConditionalForwarder'),
    newUpdateConditionalForwarder,
    UpdateConditionalForwarderResponse (UpdateConditionalForwarderResponse'),
    newUpdateConditionalForwarderResponse,

    -- ** DeleteConditionalForwarder
    DeleteConditionalForwarder (DeleteConditionalForwarder'),
    newDeleteConditionalForwarder,
    DeleteConditionalForwarderResponse (DeleteConditionalForwarderResponse'),
    newDeleteConditionalForwarderResponse,

    -- ** DisableLDAPS
    DisableLDAPS (DisableLDAPS'),
    newDisableLDAPS,
    DisableLDAPSResponse (DisableLDAPSResponse'),
    newDisableLDAPSResponse,

    -- ** DeleteLogSubscription
    DeleteLogSubscription (DeleteLogSubscription'),
    newDeleteLogSubscription,
    DeleteLogSubscriptionResponse (DeleteLogSubscriptionResponse'),
    newDeleteLogSubscriptionResponse,

    -- ** EnableSso
    EnableSso (EnableSso'),
    newEnableSso,
    EnableSsoResponse (EnableSsoResponse'),
    newEnableSsoResponse,

    -- ** CancelSchemaExtension
    CancelSchemaExtension (CancelSchemaExtension'),
    newCancelSchemaExtension,
    CancelSchemaExtensionResponse (CancelSchemaExtensionResponse'),
    newCancelSchemaExtensionResponse,

    -- ** ListLogSubscriptions (Paginated)
    ListLogSubscriptions (ListLogSubscriptions'),
    newListLogSubscriptions,
    ListLogSubscriptionsResponse (ListLogSubscriptionsResponse'),
    newListLogSubscriptionsResponse,

    -- ** EnableRadius
    EnableRadius (EnableRadius'),
    newEnableRadius,
    EnableRadiusResponse (EnableRadiusResponse'),
    newEnableRadiusResponse,

    -- ** ListIpRoutes (Paginated)
    ListIpRoutes (ListIpRoutes'),
    newListIpRoutes,
    ListIpRoutesResponse (ListIpRoutesResponse'),
    newListIpRoutesResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** DescribeClientAuthenticationSettings
    DescribeClientAuthenticationSettings (DescribeClientAuthenticationSettings'),
    newDescribeClientAuthenticationSettings,
    DescribeClientAuthenticationSettingsResponse (DescribeClientAuthenticationSettingsResponse'),
    newDescribeClientAuthenticationSettingsResponse,

    -- ** ListSchemaExtensions (Paginated)
    ListSchemaExtensions (ListSchemaExtensions'),
    newListSchemaExtensions,
    ListSchemaExtensionsResponse (ListSchemaExtensionsResponse'),
    newListSchemaExtensionsResponse,

    -- ** DisableRadius
    DisableRadius (DisableRadius'),
    newDisableRadius,
    DisableRadiusResponse (DisableRadiusResponse'),
    newDisableRadiusResponse,

    -- ** ListCertificates
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** RejectSharedDirectory
    RejectSharedDirectory (RejectSharedDirectory'),
    newRejectSharedDirectory,
    RejectSharedDirectoryResponse (RejectSharedDirectoryResponse'),
    newRejectSharedDirectoryResponse,

    -- ** UnshareDirectory
    UnshareDirectory (UnshareDirectory'),
    newUnshareDirectory,
    UnshareDirectoryResponse (UnshareDirectoryResponse'),
    newUnshareDirectoryResponse,

    -- ** RestoreFromSnapshot
    RestoreFromSnapshot (RestoreFromSnapshot'),
    newRestoreFromSnapshot,
    RestoreFromSnapshotResponse (RestoreFromSnapshotResponse'),
    newRestoreFromSnapshotResponse,

    -- ** DescribeDomainControllers (Paginated)
    DescribeDomainControllers (DescribeDomainControllers'),
    newDescribeDomainControllers,
    DescribeDomainControllersResponse (DescribeDomainControllersResponse'),
    newDescribeDomainControllersResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** RemoveIpRoutes
    RemoveIpRoutes (RemoveIpRoutes'),
    newRemoveIpRoutes,
    RemoveIpRoutesResponse (RemoveIpRoutesResponse'),
    newRemoveIpRoutesResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeregisterCertificate
    DeregisterCertificate (DeregisterCertificate'),
    newDeregisterCertificate,
    DeregisterCertificateResponse (DeregisterCertificateResponse'),
    newDeregisterCertificateResponse,

    -- ** StartSchemaExtension
    StartSchemaExtension (StartSchemaExtension'),
    newStartSchemaExtension,
    StartSchemaExtensionResponse (StartSchemaExtensionResponse'),
    newStartSchemaExtensionResponse,

    -- ** CreateTrust
    CreateTrust (CreateTrust'),
    newCreateTrust,
    CreateTrustResponse (CreateTrustResponse'),
    newCreateTrustResponse,

    -- ** DeleteDirectory
    DeleteDirectory (DeleteDirectory'),
    newDeleteDirectory,
    DeleteDirectoryResponse (DeleteDirectoryResponse'),
    newDeleteDirectoryResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** EnableClientAuthentication
    EnableClientAuthentication (EnableClientAuthentication'),
    newEnableClientAuthentication,
    EnableClientAuthenticationResponse (EnableClientAuthenticationResponse'),
    newEnableClientAuthenticationResponse,

    -- ** CreateComputer
    CreateComputer (CreateComputer'),
    newCreateComputer,
    CreateComputerResponse (CreateComputerResponse'),
    newCreateComputerResponse,

    -- ** DescribeSharedDirectories (Paginated)
    DescribeSharedDirectories (DescribeSharedDirectories'),
    newDescribeSharedDirectories,
    DescribeSharedDirectoriesResponse (DescribeSharedDirectoriesResponse'),
    newDescribeSharedDirectoriesResponse,

    -- ** EnableLDAPS
    EnableLDAPS (EnableLDAPS'),
    newEnableLDAPS,
    EnableLDAPSResponse (EnableLDAPSResponse'),
    newEnableLDAPSResponse,

    -- ** DisableSso
    DisableSso (DisableSso'),
    newDisableSso,
    DisableSsoResponse (DisableSsoResponse'),
    newDisableSsoResponse,

    -- ** VerifyTrust
    VerifyTrust (VerifyTrust'),
    newVerifyTrust,
    VerifyTrustResponse (VerifyTrustResponse'),
    newVerifyTrustResponse,

    -- ** RemoveRegion
    RemoveRegion (RemoveRegion'),
    newRemoveRegion,
    RemoveRegionResponse (RemoveRegionResponse'),
    newRemoveRegionResponse,

    -- ** CreateConditionalForwarder
    CreateConditionalForwarder (CreateConditionalForwarder'),
    newCreateConditionalForwarder,
    CreateConditionalForwarderResponse (CreateConditionalForwarderResponse'),
    newCreateConditionalForwarderResponse,

    -- ** DescribeRegions
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** AddRegion
    AddRegion (AddRegion'),
    newAddRegion,
    AddRegionResponse (AddRegionResponse'),
    newAddRegionResponse,

    -- ** GetDirectoryLimits
    GetDirectoryLimits (GetDirectoryLimits'),
    newGetDirectoryLimits,
    GetDirectoryLimitsResponse (GetDirectoryLimitsResponse'),
    newGetDirectoryLimitsResponse,

    -- ** UpdateRadius
    UpdateRadius (UpdateRadius'),
    newUpdateRadius,
    UpdateRadiusResponse (UpdateRadiusResponse'),
    newUpdateRadiusResponse,

    -- * Types

    -- ** CertificateState
    CertificateState (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** ClientAuthenticationStatus
    ClientAuthenticationStatus (..),

    -- ** ClientAuthenticationType
    ClientAuthenticationType (..),

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

    -- ** IpRouteStatusMsg
    IpRouteStatusMsg (..),

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
    Attribute (Attribute'),
    newAttribute,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CertificateInfo
    CertificateInfo (CertificateInfo'),
    newCertificateInfo,

    -- ** ClientAuthenticationSettingInfo
    ClientAuthenticationSettingInfo (ClientAuthenticationSettingInfo'),
    newClientAuthenticationSettingInfo,

    -- ** ClientCertAuthSettings
    ClientCertAuthSettings (ClientCertAuthSettings'),
    newClientCertAuthSettings,

    -- ** Computer
    Computer (Computer'),
    newComputer,

    -- ** ConditionalForwarder
    ConditionalForwarder (ConditionalForwarder'),
    newConditionalForwarder,

    -- ** DirectoryConnectSettings
    DirectoryConnectSettings (DirectoryConnectSettings'),
    newDirectoryConnectSettings,

    -- ** DirectoryConnectSettingsDescription
    DirectoryConnectSettingsDescription (DirectoryConnectSettingsDescription'),
    newDirectoryConnectSettingsDescription,

    -- ** DirectoryDescription
    DirectoryDescription (DirectoryDescription'),
    newDirectoryDescription,

    -- ** DirectoryLimits
    DirectoryLimits (DirectoryLimits'),
    newDirectoryLimits,

    -- ** DirectoryVpcSettings
    DirectoryVpcSettings (DirectoryVpcSettings'),
    newDirectoryVpcSettings,

    -- ** DirectoryVpcSettingsDescription
    DirectoryVpcSettingsDescription (DirectoryVpcSettingsDescription'),
    newDirectoryVpcSettingsDescription,

    -- ** DomainController
    DomainController (DomainController'),
    newDomainController,

    -- ** EventTopic
    EventTopic (EventTopic'),
    newEventTopic,

    -- ** IpRoute
    IpRoute (IpRoute'),
    newIpRoute,

    -- ** IpRouteInfo
    IpRouteInfo (IpRouteInfo'),
    newIpRouteInfo,

    -- ** LDAPSSettingInfo
    LDAPSSettingInfo (LDAPSSettingInfo'),
    newLDAPSSettingInfo,

    -- ** LogSubscription
    LogSubscription (LogSubscription'),
    newLogSubscription,

    -- ** OwnerDirectoryDescription
    OwnerDirectoryDescription (OwnerDirectoryDescription'),
    newOwnerDirectoryDescription,

    -- ** RadiusSettings
    RadiusSettings (RadiusSettings'),
    newRadiusSettings,

    -- ** RegionDescription
    RegionDescription (RegionDescription'),
    newRegionDescription,

    -- ** RegionsInfo
    RegionsInfo (RegionsInfo'),
    newRegionsInfo,

    -- ** SchemaExtensionInfo
    SchemaExtensionInfo (SchemaExtensionInfo'),
    newSchemaExtensionInfo,

    -- ** ShareTarget
    ShareTarget (ShareTarget'),
    newShareTarget,

    -- ** SharedDirectory
    SharedDirectory (SharedDirectory'),
    newSharedDirectory,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** SnapshotLimits
    SnapshotLimits (SnapshotLimits'),
    newSnapshotLimits,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Trust
    Trust (Trust'),
    newTrust,

    -- ** UnshareTarget
    UnshareTarget (UnshareTarget'),
    newUnshareTarget,
  )
where

import Network.AWS.DirectoryService.AcceptSharedDirectory
import Network.AWS.DirectoryService.AddIpRoutes
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
import Network.AWS.DirectoryService.DescribeClientAuthenticationSettings
import Network.AWS.DirectoryService.DescribeConditionalForwarders
import Network.AWS.DirectoryService.DescribeDirectories
import Network.AWS.DirectoryService.DescribeDomainControllers
import Network.AWS.DirectoryService.DescribeEventTopics
import Network.AWS.DirectoryService.DescribeLDAPSSettings
import Network.AWS.DirectoryService.DescribeRegions
import Network.AWS.DirectoryService.DescribeSharedDirectories
import Network.AWS.DirectoryService.DescribeSnapshots
import Network.AWS.DirectoryService.DescribeTrusts
import Network.AWS.DirectoryService.DisableClientAuthentication
import Network.AWS.DirectoryService.DisableLDAPS
import Network.AWS.DirectoryService.DisableRadius
import Network.AWS.DirectoryService.DisableSso
import Network.AWS.DirectoryService.EnableClientAuthentication
import Network.AWS.DirectoryService.EnableLDAPS
import Network.AWS.DirectoryService.EnableRadius
import Network.AWS.DirectoryService.EnableSso
import Network.AWS.DirectoryService.GetDirectoryLimits
import Network.AWS.DirectoryService.GetSnapshotLimits
import Network.AWS.DirectoryService.Lens
import Network.AWS.DirectoryService.ListCertificates
import Network.AWS.DirectoryService.ListIpRoutes
import Network.AWS.DirectoryService.ListLogSubscriptions
import Network.AWS.DirectoryService.ListSchemaExtensions
import Network.AWS.DirectoryService.ListTagsForResource
import Network.AWS.DirectoryService.RegisterCertificate
import Network.AWS.DirectoryService.RegisterEventTopic
import Network.AWS.DirectoryService.RejectSharedDirectory
import Network.AWS.DirectoryService.RemoveIpRoutes
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
