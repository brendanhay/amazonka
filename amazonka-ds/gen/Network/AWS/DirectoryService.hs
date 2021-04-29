{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service
--
-- AWS Directory Service is a web service that makes it easy for you to
-- setup and run directories in the AWS cloud, or connect your AWS
-- resources with an existing on-premises Microsoft Active Directory. This
-- guide provides detailed information about AWS Directory Service
-- operations, data types, parameters, and errors. For information about
-- AWS Directory Services features, see
-- <https://aws.amazon.com/directoryservice/ AWS Directory Service> and the
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html AWS Directory Service Administration Guide>.
--
-- AWS provides SDKs that consist of libraries and sample code for various
-- programming languages and platforms (Java, Ruby, .Net, iOS, Android,
-- etc.). The SDKs provide a convenient way to create programmatic access
-- to AWS Directory Service and other AWS services. For more information
-- about the AWS SDKs, including how to download and install them, see
-- <http://aws.amazon.com/tools/ Tools for Amazon Web Services>.
module Network.AWS.DirectoryService
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** EntityDoesNotExistException
    _EntityDoesNotExistException,

    -- ** DirectoryAlreadyInRegionException
    _DirectoryAlreadyInRegionException,

    -- ** ShareLimitExceededException
    _ShareLimitExceededException,

    -- ** RegionLimitExceededException
    _RegionLimitExceededException,

    -- ** DirectoryAlreadySharedException
    _DirectoryAlreadySharedException,

    -- ** CertificateInUseException
    _CertificateInUseException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** DomainControllerLimitExceededException
    _DomainControllerLimitExceededException,

    -- ** DirectoryDoesNotExistException
    _DirectoryDoesNotExistException,

    -- ** UserDoesNotExistException
    _UserDoesNotExistException,

    -- ** SnapshotLimitExceededException
    _SnapshotLimitExceededException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** DirectoryNotSharedException
    _DirectoryNotSharedException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** OrganizationsException
    _OrganizationsException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** DirectoryUnavailableException
    _DirectoryUnavailableException,

    -- ** CertificateAlreadyExistsException
    _CertificateAlreadyExistsException,

    -- ** AuthenticationFailedException
    _AuthenticationFailedException,

    -- ** CertificateLimitExceededException
    _CertificateLimitExceededException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** CertificateDoesNotExistException
    _CertificateDoesNotExistException,

    -- ** IpRouteLimitExceededException
    _IpRouteLimitExceededException,

    -- ** ClientException
    _ClientException,

    -- ** InvalidCertificateException
    _InvalidCertificateException,

    -- ** InvalidClientAuthStatusException
    _InvalidClientAuthStatusException,

    -- ** InvalidLDAPSStatusException
    _InvalidLDAPSStatusException,

    -- ** DirectoryLimitExceededException
    _DirectoryLimitExceededException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** ServiceException
    _ServiceException,

    -- ** NoAvailableCertificateException
    _NoAvailableCertificateException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ConnectDirectory
    ConnectDirectory (ConnectDirectory'),
    newConnectDirectory,
    ConnectDirectoryResponse (ConnectDirectoryResponse'),
    newConnectDirectoryResponse,

    -- ** RejectSharedDirectory
    RejectSharedDirectory (RejectSharedDirectory'),
    newRejectSharedDirectory,
    RejectSharedDirectoryResponse (RejectSharedDirectoryResponse'),
    newRejectSharedDirectoryResponse,

    -- ** DisableRadius
    DisableRadius (DisableRadius'),
    newDisableRadius,
    DisableRadiusResponse (DisableRadiusResponse'),
    newDisableRadiusResponse,

    -- ** RegisterEventTopic
    RegisterEventTopic (RegisterEventTopic'),
    newRegisterEventTopic,
    RegisterEventTopicResponse (RegisterEventTopicResponse'),
    newRegisterEventTopicResponse,

    -- ** ShareDirectory
    ShareDirectory (ShareDirectory'),
    newShareDirectory,
    ShareDirectoryResponse (ShareDirectoryResponse'),
    newShareDirectoryResponse,

    -- ** AddRegion
    AddRegion (AddRegion'),
    newAddRegion,
    AddRegionResponse (AddRegionResponse'),
    newAddRegionResponse,

    -- ** ListIpRoutes (Paginated)
    ListIpRoutes (ListIpRoutes'),
    newListIpRoutes,
    ListIpRoutesResponse (ListIpRoutesResponse'),
    newListIpRoutesResponse,

    -- ** EnableRadius
    EnableRadius (EnableRadius'),
    newEnableRadius,
    EnableRadiusResponse (EnableRadiusResponse'),
    newEnableRadiusResponse,

    -- ** ListSchemaExtensions (Paginated)
    ListSchemaExtensions (ListSchemaExtensions'),
    newListSchemaExtensions,
    ListSchemaExtensionsResponse (ListSchemaExtensionsResponse'),
    newListSchemaExtensionsResponse,

    -- ** RemoveRegion
    RemoveRegion (RemoveRegion'),
    newRemoveRegion,
    RemoveRegionResponse (RemoveRegionResponse'),
    newRemoveRegionResponse,

    -- ** DeleteLogSubscription
    DeleteLogSubscription (DeleteLogSubscription'),
    newDeleteLogSubscription,
    DeleteLogSubscriptionResponse (DeleteLogSubscriptionResponse'),
    newDeleteLogSubscriptionResponse,

    -- ** CancelSchemaExtension
    CancelSchemaExtension (CancelSchemaExtension'),
    newCancelSchemaExtension,
    CancelSchemaExtensionResponse (CancelSchemaExtensionResponse'),
    newCancelSchemaExtensionResponse,

    -- ** EnableSso
    EnableSso (EnableSso'),
    newEnableSso,
    EnableSsoResponse (EnableSsoResponse'),
    newEnableSsoResponse,

    -- ** CreateConditionalForwarder
    CreateConditionalForwarder (CreateConditionalForwarder'),
    newCreateConditionalForwarder,
    CreateConditionalForwarderResponse (CreateConditionalForwarderResponse'),
    newCreateConditionalForwarderResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** EnableLDAPS
    EnableLDAPS (EnableLDAPS'),
    newEnableLDAPS,
    EnableLDAPSResponse (EnableLDAPSResponse'),
    newEnableLDAPSResponse,

    -- ** DeleteConditionalForwarder
    DeleteConditionalForwarder (DeleteConditionalForwarder'),
    newDeleteConditionalForwarder,
    DeleteConditionalForwarderResponse (DeleteConditionalForwarderResponse'),
    newDeleteConditionalForwarderResponse,

    -- ** DescribeSharedDirectories (Paginated)
    DescribeSharedDirectories (DescribeSharedDirectories'),
    newDescribeSharedDirectories,
    DescribeSharedDirectoriesResponse (DescribeSharedDirectoriesResponse'),
    newDescribeSharedDirectoriesResponse,

    -- ** UpdateConditionalForwarder
    UpdateConditionalForwarder (UpdateConditionalForwarder'),
    newUpdateConditionalForwarder,
    UpdateConditionalForwarderResponse (UpdateConditionalForwarderResponse'),
    newUpdateConditionalForwarderResponse,

    -- ** VerifyTrust
    VerifyTrust (VerifyTrust'),
    newVerifyTrust,
    VerifyTrustResponse (VerifyTrustResponse'),
    newVerifyTrustResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

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

    -- ** DisableClientAuthentication
    DisableClientAuthentication (DisableClientAuthentication'),
    newDisableClientAuthentication,
    DisableClientAuthenticationResponse (DisableClientAuthenticationResponse'),
    newDisableClientAuthenticationResponse,

    -- ** CreateMicrosoftAD
    CreateMicrosoftAD (CreateMicrosoftAD'),
    newCreateMicrosoftAD,
    CreateMicrosoftADResponse (CreateMicrosoftADResponse'),
    newCreateMicrosoftADResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** RemoveIpRoutes
    RemoveIpRoutes (RemoveIpRoutes'),
    newRemoveIpRoutes,
    RemoveIpRoutesResponse (RemoveIpRoutesResponse'),
    newRemoveIpRoutesResponse,

    -- ** UpdateTrust
    UpdateTrust (UpdateTrust'),
    newUpdateTrust,
    UpdateTrustResponse (UpdateTrustResponse'),
    newUpdateTrustResponse,

    -- ** DeleteTrust
    DeleteTrust (DeleteTrust'),
    newDeleteTrust,
    DeleteTrustResponse (DeleteTrustResponse'),
    newDeleteTrustResponse,

    -- ** CreateDirectory
    CreateDirectory (CreateDirectory'),
    newCreateDirectory,
    CreateDirectoryResponse (CreateDirectoryResponse'),
    newCreateDirectoryResponse,

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

    -- ** DescribeTrusts (Paginated)
    DescribeTrusts (DescribeTrusts'),
    newDescribeTrusts,
    DescribeTrustsResponse (DescribeTrustsResponse'),
    newDescribeTrustsResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** UnshareDirectory
    UnshareDirectory (UnshareDirectory'),
    newUnshareDirectory,
    UnshareDirectoryResponse (UnshareDirectoryResponse'),
    newUnshareDirectoryResponse,

    -- ** RegisterCertificate
    RegisterCertificate (RegisterCertificate'),
    newRegisterCertificate,
    RegisterCertificateResponse (RegisterCertificateResponse'),
    newRegisterCertificateResponse,

    -- ** GetSnapshotLimits
    GetSnapshotLimits (GetSnapshotLimits'),
    newGetSnapshotLimits,
    GetSnapshotLimitsResponse (GetSnapshotLimitsResponse'),
    newGetSnapshotLimitsResponse,

    -- ** UpdateNumberOfDomainControllers
    UpdateNumberOfDomainControllers (UpdateNumberOfDomainControllers'),
    newUpdateNumberOfDomainControllers,
    UpdateNumberOfDomainControllersResponse (UpdateNumberOfDomainControllersResponse'),
    newUpdateNumberOfDomainControllersResponse,

    -- ** ListCertificates
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** DescribeConditionalForwarders
    DescribeConditionalForwarders (DescribeConditionalForwarders'),
    newDescribeConditionalForwarders,
    DescribeConditionalForwardersResponse (DescribeConditionalForwardersResponse'),
    newDescribeConditionalForwardersResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

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

    -- ** DisableLDAPS
    DisableLDAPS (DisableLDAPS'),
    newDisableLDAPS,
    DisableLDAPSResponse (DisableLDAPSResponse'),
    newDisableLDAPSResponse,

    -- ** ListLogSubscriptions (Paginated)
    ListLogSubscriptions (ListLogSubscriptions'),
    newListLogSubscriptions,
    ListLogSubscriptionsResponse (ListLogSubscriptionsResponse'),
    newListLogSubscriptionsResponse,

    -- ** DescribeRegions
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** DisableSso
    DisableSso (DisableSso'),
    newDisableSso,
    DisableSsoResponse (DisableSsoResponse'),
    newDisableSsoResponse,

    -- ** CreateLogSubscription
    CreateLogSubscription (CreateLogSubscription'),
    newCreateLogSubscription,
    CreateLogSubscriptionResponse (CreateLogSubscriptionResponse'),
    newCreateLogSubscriptionResponse,

    -- ** ResetUserPassword
    ResetUserPassword (ResetUserPassword'),
    newResetUserPassword,
    ResetUserPasswordResponse (ResetUserPasswordResponse'),
    newResetUserPasswordResponse,

    -- ** DescribeEventTopics
    DescribeEventTopics (DescribeEventTopics'),
    newDescribeEventTopics,
    DescribeEventTopicsResponse (DescribeEventTopicsResponse'),
    newDescribeEventTopicsResponse,

    -- ** CreateComputer
    CreateComputer (CreateComputer'),
    newCreateComputer,
    CreateComputerResponse (CreateComputerResponse'),
    newCreateComputerResponse,

    -- ** AcceptSharedDirectory
    AcceptSharedDirectory (AcceptSharedDirectory'),
    newAcceptSharedDirectory,
    AcceptSharedDirectoryResponse (AcceptSharedDirectoryResponse'),
    newAcceptSharedDirectoryResponse,

    -- ** EnableClientAuthentication
    EnableClientAuthentication (EnableClientAuthentication'),
    newEnableClientAuthentication,
    EnableClientAuthenticationResponse (EnableClientAuthenticationResponse'),
    newEnableClientAuthenticationResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** StartSchemaExtension
    StartSchemaExtension (StartSchemaExtension'),
    newStartSchemaExtension,
    StartSchemaExtensionResponse (StartSchemaExtensionResponse'),
    newStartSchemaExtensionResponse,

    -- ** DeregisterEventTopic
    DeregisterEventTopic (DeregisterEventTopic'),
    newDeregisterEventTopic,
    DeregisterEventTopicResponse (DeregisterEventTopicResponse'),
    newDeregisterEventTopicResponse,

    -- ** DeregisterCertificate
    DeregisterCertificate (DeregisterCertificate'),
    newDeregisterCertificate,
    DeregisterCertificateResponse (DeregisterCertificateResponse'),
    newDeregisterCertificateResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** AddIpRoutes
    AddIpRoutes (AddIpRoutes'),
    newAddIpRoutes,
    AddIpRoutesResponse (AddIpRoutesResponse'),
    newAddIpRoutesResponse,

    -- ** DescribeDirectories (Paginated)
    DescribeDirectories (DescribeDirectories'),
    newDescribeDirectories,
    DescribeDirectoriesResponse (DescribeDirectoriesResponse'),
    newDescribeDirectoriesResponse,

    -- ** DescribeLDAPSSettings
    DescribeLDAPSSettings (DescribeLDAPSSettings'),
    newDescribeLDAPSSettings,
    DescribeLDAPSSettingsResponse (DescribeLDAPSSettingsResponse'),
    newDescribeLDAPSSettingsResponse,

    -- * Types

    -- ** CertificateState
    CertificateState (..),

    -- ** CertificateType
    CertificateType (..),

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
