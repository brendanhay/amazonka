{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DirectoryService
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.DirectoryService
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AuthenticationFailedException
    _AuthenticationFailedException,

    -- ** CertificateAlreadyExistsException
    _CertificateAlreadyExistsException,

    -- ** CertificateDoesNotExistException
    _CertificateDoesNotExistException,

    -- ** CertificateInUseException
    _CertificateInUseException,

    -- ** CertificateLimitExceededException
    _CertificateLimitExceededException,

    -- ** ClientException
    _ClientException,

    -- ** DirectoryAlreadyInRegionException
    _DirectoryAlreadyInRegionException,

    -- ** DirectoryAlreadySharedException
    _DirectoryAlreadySharedException,

    -- ** DirectoryDoesNotExistException
    _DirectoryDoesNotExistException,

    -- ** DirectoryInDesiredStateException
    _DirectoryInDesiredStateException,

    -- ** DirectoryLimitExceededException
    _DirectoryLimitExceededException,

    -- ** DirectoryNotSharedException
    _DirectoryNotSharedException,

    -- ** DirectoryUnavailableException
    _DirectoryUnavailableException,

    -- ** DomainControllerLimitExceededException
    _DomainControllerLimitExceededException,

    -- ** EntityAlreadyExistsException
    _EntityAlreadyExistsException,

    -- ** EntityDoesNotExistException
    _EntityDoesNotExistException,

    -- ** IncompatibleSettingsException
    _IncompatibleSettingsException,

    -- ** InsufficientPermissionsException
    _InsufficientPermissionsException,

    -- ** InvalidCertificateException
    _InvalidCertificateException,

    -- ** InvalidClientAuthStatusException
    _InvalidClientAuthStatusException,

    -- ** InvalidLDAPSStatusException
    _InvalidLDAPSStatusException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidPasswordException
    _InvalidPasswordException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** IpRouteLimitExceededException
    _IpRouteLimitExceededException,

    -- ** NoAvailableCertificateException
    _NoAvailableCertificateException,

    -- ** OrganizationsException
    _OrganizationsException,

    -- ** RegionLimitExceededException
    _RegionLimitExceededException,

    -- ** ServiceException
    _ServiceException,

    -- ** ShareLimitExceededException
    _ShareLimitExceededException,

    -- ** SnapshotLimitExceededException
    _SnapshotLimitExceededException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- ** UnsupportedSettingsException
    _UnsupportedSettingsException,

    -- ** UserDoesNotExistException
    _UserDoesNotExistException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptSharedDirectory
    AcceptSharedDirectory (AcceptSharedDirectory'),
    newAcceptSharedDirectory,
    AcceptSharedDirectoryResponse (AcceptSharedDirectoryResponse'),
    newAcceptSharedDirectoryResponse,

    -- ** AddIpRoutes
    AddIpRoutes (AddIpRoutes'),
    newAddIpRoutes,
    AddIpRoutesResponse (AddIpRoutesResponse'),
    newAddIpRoutesResponse,

    -- ** AddRegion
    AddRegion (AddRegion'),
    newAddRegion,
    AddRegionResponse (AddRegionResponse'),
    newAddRegionResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    AddTagsToResourceResponse (AddTagsToResourceResponse'),
    newAddTagsToResourceResponse,

    -- ** CancelSchemaExtension
    CancelSchemaExtension (CancelSchemaExtension'),
    newCancelSchemaExtension,
    CancelSchemaExtensionResponse (CancelSchemaExtensionResponse'),
    newCancelSchemaExtensionResponse,

    -- ** ConnectDirectory
    ConnectDirectory (ConnectDirectory'),
    newConnectDirectory,
    ConnectDirectoryResponse (ConnectDirectoryResponse'),
    newConnectDirectoryResponse,

    -- ** CreateAlias
    CreateAlias (CreateAlias'),
    newCreateAlias,
    CreateAliasResponse (CreateAliasResponse'),
    newCreateAliasResponse,

    -- ** CreateComputer
    CreateComputer (CreateComputer'),
    newCreateComputer,
    CreateComputerResponse (CreateComputerResponse'),
    newCreateComputerResponse,

    -- ** CreateConditionalForwarder
    CreateConditionalForwarder (CreateConditionalForwarder'),
    newCreateConditionalForwarder,
    CreateConditionalForwarderResponse (CreateConditionalForwarderResponse'),
    newCreateConditionalForwarderResponse,

    -- ** CreateDirectory
    CreateDirectory (CreateDirectory'),
    newCreateDirectory,
    CreateDirectoryResponse (CreateDirectoryResponse'),
    newCreateDirectoryResponse,

    -- ** CreateLogSubscription
    CreateLogSubscription (CreateLogSubscription'),
    newCreateLogSubscription,
    CreateLogSubscriptionResponse (CreateLogSubscriptionResponse'),
    newCreateLogSubscriptionResponse,

    -- ** CreateMicrosoftAD
    CreateMicrosoftAD (CreateMicrosoftAD'),
    newCreateMicrosoftAD,
    CreateMicrosoftADResponse (CreateMicrosoftADResponse'),
    newCreateMicrosoftADResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateTrust
    CreateTrust (CreateTrust'),
    newCreateTrust,
    CreateTrustResponse (CreateTrustResponse'),
    newCreateTrustResponse,

    -- ** DeleteConditionalForwarder
    DeleteConditionalForwarder (DeleteConditionalForwarder'),
    newDeleteConditionalForwarder,
    DeleteConditionalForwarderResponse (DeleteConditionalForwarderResponse'),
    newDeleteConditionalForwarderResponse,

    -- ** DeleteDirectory
    DeleteDirectory (DeleteDirectory'),
    newDeleteDirectory,
    DeleteDirectoryResponse (DeleteDirectoryResponse'),
    newDeleteDirectoryResponse,

    -- ** DeleteLogSubscription
    DeleteLogSubscription (DeleteLogSubscription'),
    newDeleteLogSubscription,
    DeleteLogSubscriptionResponse (DeleteLogSubscriptionResponse'),
    newDeleteLogSubscriptionResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteTrust
    DeleteTrust (DeleteTrust'),
    newDeleteTrust,
    DeleteTrustResponse (DeleteTrustResponse'),
    newDeleteTrustResponse,

    -- ** DeregisterCertificate
    DeregisterCertificate (DeregisterCertificate'),
    newDeregisterCertificate,
    DeregisterCertificateResponse (DeregisterCertificateResponse'),
    newDeregisterCertificateResponse,

    -- ** DeregisterEventTopic
    DeregisterEventTopic (DeregisterEventTopic'),
    newDeregisterEventTopic,
    DeregisterEventTopicResponse (DeregisterEventTopicResponse'),
    newDeregisterEventTopicResponse,

    -- ** DescribeCertificate
    DescribeCertificate (DescribeCertificate'),
    newDescribeCertificate,
    DescribeCertificateResponse (DescribeCertificateResponse'),
    newDescribeCertificateResponse,

    -- ** DescribeClientAuthenticationSettings (Paginated)
    DescribeClientAuthenticationSettings (DescribeClientAuthenticationSettings'),
    newDescribeClientAuthenticationSettings,
    DescribeClientAuthenticationSettingsResponse (DescribeClientAuthenticationSettingsResponse'),
    newDescribeClientAuthenticationSettingsResponse,

    -- ** DescribeConditionalForwarders
    DescribeConditionalForwarders (DescribeConditionalForwarders'),
    newDescribeConditionalForwarders,
    DescribeConditionalForwardersResponse (DescribeConditionalForwardersResponse'),
    newDescribeConditionalForwardersResponse,

    -- ** DescribeDirectories (Paginated)
    DescribeDirectories (DescribeDirectories'),
    newDescribeDirectories,
    DescribeDirectoriesResponse (DescribeDirectoriesResponse'),
    newDescribeDirectoriesResponse,

    -- ** DescribeDomainControllers (Paginated)
    DescribeDomainControllers (DescribeDomainControllers'),
    newDescribeDomainControllers,
    DescribeDomainControllersResponse (DescribeDomainControllersResponse'),
    newDescribeDomainControllersResponse,

    -- ** DescribeEventTopics
    DescribeEventTopics (DescribeEventTopics'),
    newDescribeEventTopics,
    DescribeEventTopicsResponse (DescribeEventTopicsResponse'),
    newDescribeEventTopicsResponse,

    -- ** DescribeLDAPSSettings (Paginated)
    DescribeLDAPSSettings (DescribeLDAPSSettings'),
    newDescribeLDAPSSettings,
    DescribeLDAPSSettingsResponse (DescribeLDAPSSettingsResponse'),
    newDescribeLDAPSSettingsResponse,

    -- ** DescribeRegions (Paginated)
    DescribeRegions (DescribeRegions'),
    newDescribeRegions,
    DescribeRegionsResponse (DescribeRegionsResponse'),
    newDescribeRegionsResponse,

    -- ** DescribeSettings
    DescribeSettings (DescribeSettings'),
    newDescribeSettings,
    DescribeSettingsResponse (DescribeSettingsResponse'),
    newDescribeSettingsResponse,

    -- ** DescribeSharedDirectories (Paginated)
    DescribeSharedDirectories (DescribeSharedDirectories'),
    newDescribeSharedDirectories,
    DescribeSharedDirectoriesResponse (DescribeSharedDirectoriesResponse'),
    newDescribeSharedDirectoriesResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeTrusts (Paginated)
    DescribeTrusts (DescribeTrusts'),
    newDescribeTrusts,
    DescribeTrustsResponse (DescribeTrustsResponse'),
    newDescribeTrustsResponse,

    -- ** DescribeUpdateDirectory (Paginated)
    DescribeUpdateDirectory (DescribeUpdateDirectory'),
    newDescribeUpdateDirectory,
    DescribeUpdateDirectoryResponse (DescribeUpdateDirectoryResponse'),
    newDescribeUpdateDirectoryResponse,

    -- ** DisableClientAuthentication
    DisableClientAuthentication (DisableClientAuthentication'),
    newDisableClientAuthentication,
    DisableClientAuthenticationResponse (DisableClientAuthenticationResponse'),
    newDisableClientAuthenticationResponse,

    -- ** DisableLDAPS
    DisableLDAPS (DisableLDAPS'),
    newDisableLDAPS,
    DisableLDAPSResponse (DisableLDAPSResponse'),
    newDisableLDAPSResponse,

    -- ** DisableRadius
    DisableRadius (DisableRadius'),
    newDisableRadius,
    DisableRadiusResponse (DisableRadiusResponse'),
    newDisableRadiusResponse,

    -- ** DisableSso
    DisableSso (DisableSso'),
    newDisableSso,
    DisableSsoResponse (DisableSsoResponse'),
    newDisableSsoResponse,

    -- ** EnableClientAuthentication
    EnableClientAuthentication (EnableClientAuthentication'),
    newEnableClientAuthentication,
    EnableClientAuthenticationResponse (EnableClientAuthenticationResponse'),
    newEnableClientAuthenticationResponse,

    -- ** EnableLDAPS
    EnableLDAPS (EnableLDAPS'),
    newEnableLDAPS,
    EnableLDAPSResponse (EnableLDAPSResponse'),
    newEnableLDAPSResponse,

    -- ** EnableRadius
    EnableRadius (EnableRadius'),
    newEnableRadius,
    EnableRadiusResponse (EnableRadiusResponse'),
    newEnableRadiusResponse,

    -- ** EnableSso
    EnableSso (EnableSso'),
    newEnableSso,
    EnableSsoResponse (EnableSsoResponse'),
    newEnableSsoResponse,

    -- ** GetDirectoryLimits
    GetDirectoryLimits (GetDirectoryLimits'),
    newGetDirectoryLimits,
    GetDirectoryLimitsResponse (GetDirectoryLimitsResponse'),
    newGetDirectoryLimitsResponse,

    -- ** GetSnapshotLimits
    GetSnapshotLimits (GetSnapshotLimits'),
    newGetSnapshotLimits,
    GetSnapshotLimitsResponse (GetSnapshotLimitsResponse'),
    newGetSnapshotLimitsResponse,

    -- ** ListCertificates (Paginated)
    ListCertificates (ListCertificates'),
    newListCertificates,
    ListCertificatesResponse (ListCertificatesResponse'),
    newListCertificatesResponse,

    -- ** ListIpRoutes (Paginated)
    ListIpRoutes (ListIpRoutes'),
    newListIpRoutes,
    ListIpRoutesResponse (ListIpRoutesResponse'),
    newListIpRoutesResponse,

    -- ** ListLogSubscriptions (Paginated)
    ListLogSubscriptions (ListLogSubscriptions'),
    newListLogSubscriptions,
    ListLogSubscriptionsResponse (ListLogSubscriptionsResponse'),
    newListLogSubscriptionsResponse,

    -- ** ListSchemaExtensions (Paginated)
    ListSchemaExtensions (ListSchemaExtensions'),
    newListSchemaExtensions,
    ListSchemaExtensionsResponse (ListSchemaExtensionsResponse'),
    newListSchemaExtensionsResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RegisterCertificate
    RegisterCertificate (RegisterCertificate'),
    newRegisterCertificate,
    RegisterCertificateResponse (RegisterCertificateResponse'),
    newRegisterCertificateResponse,

    -- ** RegisterEventTopic
    RegisterEventTopic (RegisterEventTopic'),
    newRegisterEventTopic,
    RegisterEventTopicResponse (RegisterEventTopicResponse'),
    newRegisterEventTopicResponse,

    -- ** RejectSharedDirectory
    RejectSharedDirectory (RejectSharedDirectory'),
    newRejectSharedDirectory,
    RejectSharedDirectoryResponse (RejectSharedDirectoryResponse'),
    newRejectSharedDirectoryResponse,

    -- ** RemoveIpRoutes
    RemoveIpRoutes (RemoveIpRoutes'),
    newRemoveIpRoutes,
    RemoveIpRoutesResponse (RemoveIpRoutesResponse'),
    newRemoveIpRoutesResponse,

    -- ** RemoveRegion
    RemoveRegion (RemoveRegion'),
    newRemoveRegion,
    RemoveRegionResponse (RemoveRegionResponse'),
    newRemoveRegionResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    RemoveTagsFromResourceResponse (RemoveTagsFromResourceResponse'),
    newRemoveTagsFromResourceResponse,

    -- ** ResetUserPassword
    ResetUserPassword (ResetUserPassword'),
    newResetUserPassword,
    ResetUserPasswordResponse (ResetUserPasswordResponse'),
    newResetUserPasswordResponse,

    -- ** RestoreFromSnapshot
    RestoreFromSnapshot (RestoreFromSnapshot'),
    newRestoreFromSnapshot,
    RestoreFromSnapshotResponse (RestoreFromSnapshotResponse'),
    newRestoreFromSnapshotResponse,

    -- ** ShareDirectory
    ShareDirectory (ShareDirectory'),
    newShareDirectory,
    ShareDirectoryResponse (ShareDirectoryResponse'),
    newShareDirectoryResponse,

    -- ** StartSchemaExtension
    StartSchemaExtension (StartSchemaExtension'),
    newStartSchemaExtension,
    StartSchemaExtensionResponse (StartSchemaExtensionResponse'),
    newStartSchemaExtensionResponse,

    -- ** UnshareDirectory
    UnshareDirectory (UnshareDirectory'),
    newUnshareDirectory,
    UnshareDirectoryResponse (UnshareDirectoryResponse'),
    newUnshareDirectoryResponse,

    -- ** UpdateConditionalForwarder
    UpdateConditionalForwarder (UpdateConditionalForwarder'),
    newUpdateConditionalForwarder,
    UpdateConditionalForwarderResponse (UpdateConditionalForwarderResponse'),
    newUpdateConditionalForwarderResponse,

    -- ** UpdateDirectorySetup
    UpdateDirectorySetup (UpdateDirectorySetup'),
    newUpdateDirectorySetup,
    UpdateDirectorySetupResponse (UpdateDirectorySetupResponse'),
    newUpdateDirectorySetupResponse,

    -- ** UpdateNumberOfDomainControllers
    UpdateNumberOfDomainControllers (UpdateNumberOfDomainControllers'),
    newUpdateNumberOfDomainControllers,
    UpdateNumberOfDomainControllersResponse (UpdateNumberOfDomainControllersResponse'),
    newUpdateNumberOfDomainControllersResponse,

    -- ** UpdateRadius
    UpdateRadius (UpdateRadius'),
    newUpdateRadius,
    UpdateRadiusResponse (UpdateRadiusResponse'),
    newUpdateRadiusResponse,

    -- ** UpdateSettings
    UpdateSettings (UpdateSettings'),
    newUpdateSettings,
    UpdateSettingsResponse (UpdateSettingsResponse'),
    newUpdateSettingsResponse,

    -- ** UpdateTrust
    UpdateTrust (UpdateTrust'),
    newUpdateTrust,
    UpdateTrustResponse (UpdateTrustResponse'),
    newUpdateTrustResponse,

    -- ** VerifyTrust
    VerifyTrust (VerifyTrust'),
    newVerifyTrust,
    VerifyTrustResponse (VerifyTrustResponse'),
    newVerifyTrustResponse,

    -- * Types

    -- ** CertificateState
    CertificateState (..),

    -- ** CertificateType
    CertificateType (..),

    -- ** ClientAuthenticationStatus
    ClientAuthenticationStatus (..),

    -- ** ClientAuthenticationType
    ClientAuthenticationType (..),

    -- ** DirectoryConfigurationStatus
    DirectoryConfigurationStatus (..),

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

    -- ** OSVersion
    OSVersion (..),

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

    -- ** UpdateStatus
    UpdateStatus (..),

    -- ** UpdateType
    UpdateType (..),

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

    -- ** OSUpdateSettings
    OSUpdateSettings (OSUpdateSettings'),
    newOSUpdateSettings,

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

    -- ** Setting
    Setting (Setting'),
    newSetting,

    -- ** SettingEntry
    SettingEntry (SettingEntry'),
    newSettingEntry,

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

    -- ** UpdateInfoEntry
    UpdateInfoEntry (UpdateInfoEntry'),
    newUpdateInfoEntry,

    -- ** UpdateValue
    UpdateValue (UpdateValue'),
    newUpdateValue,
  )
where

import Amazonka.DirectoryService.AcceptSharedDirectory
import Amazonka.DirectoryService.AddIpRoutes
import Amazonka.DirectoryService.AddRegion
import Amazonka.DirectoryService.AddTagsToResource
import Amazonka.DirectoryService.CancelSchemaExtension
import Amazonka.DirectoryService.ConnectDirectory
import Amazonka.DirectoryService.CreateAlias
import Amazonka.DirectoryService.CreateComputer
import Amazonka.DirectoryService.CreateConditionalForwarder
import Amazonka.DirectoryService.CreateDirectory
import Amazonka.DirectoryService.CreateLogSubscription
import Amazonka.DirectoryService.CreateMicrosoftAD
import Amazonka.DirectoryService.CreateSnapshot
import Amazonka.DirectoryService.CreateTrust
import Amazonka.DirectoryService.DeleteConditionalForwarder
import Amazonka.DirectoryService.DeleteDirectory
import Amazonka.DirectoryService.DeleteLogSubscription
import Amazonka.DirectoryService.DeleteSnapshot
import Amazonka.DirectoryService.DeleteTrust
import Amazonka.DirectoryService.DeregisterCertificate
import Amazonka.DirectoryService.DeregisterEventTopic
import Amazonka.DirectoryService.DescribeCertificate
import Amazonka.DirectoryService.DescribeClientAuthenticationSettings
import Amazonka.DirectoryService.DescribeConditionalForwarders
import Amazonka.DirectoryService.DescribeDirectories
import Amazonka.DirectoryService.DescribeDomainControllers
import Amazonka.DirectoryService.DescribeEventTopics
import Amazonka.DirectoryService.DescribeLDAPSSettings
import Amazonka.DirectoryService.DescribeRegions
import Amazonka.DirectoryService.DescribeSettings
import Amazonka.DirectoryService.DescribeSharedDirectories
import Amazonka.DirectoryService.DescribeSnapshots
import Amazonka.DirectoryService.DescribeTrusts
import Amazonka.DirectoryService.DescribeUpdateDirectory
import Amazonka.DirectoryService.DisableClientAuthentication
import Amazonka.DirectoryService.DisableLDAPS
import Amazonka.DirectoryService.DisableRadius
import Amazonka.DirectoryService.DisableSso
import Amazonka.DirectoryService.EnableClientAuthentication
import Amazonka.DirectoryService.EnableLDAPS
import Amazonka.DirectoryService.EnableRadius
import Amazonka.DirectoryService.EnableSso
import Amazonka.DirectoryService.GetDirectoryLimits
import Amazonka.DirectoryService.GetSnapshotLimits
import Amazonka.DirectoryService.Lens
import Amazonka.DirectoryService.ListCertificates
import Amazonka.DirectoryService.ListIpRoutes
import Amazonka.DirectoryService.ListLogSubscriptions
import Amazonka.DirectoryService.ListSchemaExtensions
import Amazonka.DirectoryService.ListTagsForResource
import Amazonka.DirectoryService.RegisterCertificate
import Amazonka.DirectoryService.RegisterEventTopic
import Amazonka.DirectoryService.RejectSharedDirectory
import Amazonka.DirectoryService.RemoveIpRoutes
import Amazonka.DirectoryService.RemoveRegion
import Amazonka.DirectoryService.RemoveTagsFromResource
import Amazonka.DirectoryService.ResetUserPassword
import Amazonka.DirectoryService.RestoreFromSnapshot
import Amazonka.DirectoryService.ShareDirectory
import Amazonka.DirectoryService.StartSchemaExtension
import Amazonka.DirectoryService.Types
import Amazonka.DirectoryService.UnshareDirectory
import Amazonka.DirectoryService.UpdateConditionalForwarder
import Amazonka.DirectoryService.UpdateDirectorySetup
import Amazonka.DirectoryService.UpdateNumberOfDomainControllers
import Amazonka.DirectoryService.UpdateRadius
import Amazonka.DirectoryService.UpdateSettings
import Amazonka.DirectoryService.UpdateTrust
import Amazonka.DirectoryService.VerifyTrust
import Amazonka.DirectoryService.Waiters

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
