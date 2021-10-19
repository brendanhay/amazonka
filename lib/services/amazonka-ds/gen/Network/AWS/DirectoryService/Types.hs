{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CertificateLimitExceededException,
    _CertificateAlreadyExistsException,
    _AccessDeniedException,
    _DirectoryUnavailableException,
    _AuthenticationFailedException,
    _InvalidParameterException,
    _UnsupportedOperationException,
    _EntityAlreadyExistsException,
    _NoAvailableCertificateException,
    _UserDoesNotExistException,
    _DirectoryLimitExceededException,
    _InvalidLDAPSStatusException,
    _InvalidCertificateException,
    _CertificateInUseException,
    _RegionLimitExceededException,
    _IpRouteLimitExceededException,
    _ShareLimitExceededException,
    _EntityDoesNotExistException,
    _OrganizationsException,
    _InvalidTargetException,
    _DirectoryAlreadyInRegionException,
    _InsufficientPermissionsException,
    _DirectoryNotSharedException,
    _InvalidNextTokenException,
    _ServiceException,
    _SnapshotLimitExceededException,
    _DomainControllerLimitExceededException,
    _DirectoryDoesNotExistException,
    _InvalidClientAuthStatusException,
    _TagLimitExceededException,
    _ClientException,
    _DirectoryAlreadySharedException,
    _CertificateDoesNotExistException,
    _InvalidPasswordException,

    -- * CertificateState
    CertificateState (..),

    -- * CertificateType
    CertificateType (..),

    -- * ClientAuthenticationStatus
    ClientAuthenticationStatus (..),

    -- * ClientAuthenticationType
    ClientAuthenticationType (..),

    -- * DirectoryEdition
    DirectoryEdition (..),

    -- * DirectorySize
    DirectorySize (..),

    -- * DirectoryStage
    DirectoryStage (..),

    -- * DirectoryType
    DirectoryType (..),

    -- * DomainControllerStatus
    DomainControllerStatus (..),

    -- * IpRouteStatusMsg
    IpRouteStatusMsg (..),

    -- * LDAPSStatus
    LDAPSStatus (..),

    -- * LDAPSType
    LDAPSType (..),

    -- * RadiusAuthenticationProtocol
    RadiusAuthenticationProtocol (..),

    -- * RadiusStatus
    RadiusStatus (..),

    -- * RegionType
    RegionType (..),

    -- * ReplicationScope
    ReplicationScope (..),

    -- * SchemaExtensionStatus
    SchemaExtensionStatus (..),

    -- * SelectiveAuth
    SelectiveAuth (..),

    -- * ShareMethod
    ShareMethod (..),

    -- * ShareStatus
    ShareStatus (..),

    -- * SnapshotStatus
    SnapshotStatus (..),

    -- * SnapshotType
    SnapshotType (..),

    -- * TargetType
    TargetType (..),

    -- * TopicStatus
    TopicStatus (..),

    -- * TrustDirection
    TrustDirection (..),

    -- * TrustState
    TrustState (..),

    -- * TrustType
    TrustType (..),

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_value,
    attribute_name,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_clientCertAuthSettings,
    certificate_state,
    certificate_commonName,
    certificate_certificateId,
    certificate_expiryDateTime,
    certificate_registeredDateTime,
    certificate_type,
    certificate_stateReason,

    -- * CertificateInfo
    CertificateInfo (..),
    newCertificateInfo,
    certificateInfo_state,
    certificateInfo_commonName,
    certificateInfo_certificateId,
    certificateInfo_expiryDateTime,
    certificateInfo_type,

    -- * ClientAuthenticationSettingInfo
    ClientAuthenticationSettingInfo (..),
    newClientAuthenticationSettingInfo,
    clientAuthenticationSettingInfo_status,
    clientAuthenticationSettingInfo_lastUpdatedDateTime,
    clientAuthenticationSettingInfo_type,

    -- * ClientCertAuthSettings
    ClientCertAuthSettings (..),
    newClientCertAuthSettings,
    clientCertAuthSettings_oCSPUrl,

    -- * Computer
    Computer (..),
    newComputer,
    computer_computerId,
    computer_computerAttributes,
    computer_computerName,

    -- * ConditionalForwarder
    ConditionalForwarder (..),
    newConditionalForwarder,
    conditionalForwarder_dnsIpAddrs,
    conditionalForwarder_remoteDomainName,
    conditionalForwarder_replicationScope,

    -- * DirectoryConnectSettings
    DirectoryConnectSettings (..),
    newDirectoryConnectSettings,
    directoryConnectSettings_vpcId,
    directoryConnectSettings_subnetIds,
    directoryConnectSettings_customerDnsIps,
    directoryConnectSettings_customerUserName,

    -- * DirectoryConnectSettingsDescription
    DirectoryConnectSettingsDescription (..),
    newDirectoryConnectSettingsDescription,
    directoryConnectSettingsDescription_customerUserName,
    directoryConnectSettingsDescription_subnetIds,
    directoryConnectSettingsDescription_vpcId,
    directoryConnectSettingsDescription_securityGroupId,
    directoryConnectSettingsDescription_connectIps,
    directoryConnectSettingsDescription_availabilityZones,

    -- * DirectoryDescription
    DirectoryDescription (..),
    newDirectoryDescription,
    directoryDescription_edition,
    directoryDescription_radiusStatus,
    directoryDescription_stage,
    directoryDescription_directoryId,
    directoryDescription_accessUrl,
    directoryDescription_shortName,
    directoryDescription_regionsInfo,
    directoryDescription_size,
    directoryDescription_desiredNumberOfDomainControllers,
    directoryDescription_radiusSettings,
    directoryDescription_launchTime,
    directoryDescription_alias,
    directoryDescription_shareStatus,
    directoryDescription_name,
    directoryDescription_shareMethod,
    directoryDescription_stageLastUpdatedDateTime,
    directoryDescription_ssoEnabled,
    directoryDescription_dnsIpAddrs,
    directoryDescription_vpcSettings,
    directoryDescription_type,
    directoryDescription_stageReason,
    directoryDescription_connectSettings,
    directoryDescription_ownerDirectoryDescription,
    directoryDescription_description,
    directoryDescription_shareNotes,

    -- * DirectoryLimits
    DirectoryLimits (..),
    newDirectoryLimits,
    directoryLimits_connectedDirectoriesCurrentCount,
    directoryLimits_cloudOnlyMicrosoftADLimitReached,
    directoryLimits_connectedDirectoriesLimit,
    directoryLimits_connectedDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADLimit,
    directoryLimits_cloudOnlyDirectoriesLimit,
    directoryLimits_cloudOnlyDirectoriesCurrentCount,
    directoryLimits_cloudOnlyDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADCurrentCount,

    -- * DirectoryVpcSettings
    DirectoryVpcSettings (..),
    newDirectoryVpcSettings,
    directoryVpcSettings_vpcId,
    directoryVpcSettings_subnetIds,

    -- * DirectoryVpcSettingsDescription
    DirectoryVpcSettingsDescription (..),
    newDirectoryVpcSettingsDescription,
    directoryVpcSettingsDescription_subnetIds,
    directoryVpcSettingsDescription_vpcId,
    directoryVpcSettingsDescription_securityGroupId,
    directoryVpcSettingsDescription_availabilityZones,

    -- * DomainController
    DomainController (..),
    newDomainController,
    domainController_status,
    domainController_directoryId,
    domainController_vpcId,
    domainController_launchTime,
    domainController_subnetId,
    domainController_availabilityZone,
    domainController_statusLastUpdatedDateTime,
    domainController_statusReason,
    domainController_dnsIpAddr,
    domainController_domainControllerId,

    -- * EventTopic
    EventTopic (..),
    newEventTopic,
    eventTopic_status,
    eventTopic_directoryId,
    eventTopic_topicName,
    eventTopic_topicArn,
    eventTopic_createdDateTime,

    -- * IpRoute
    IpRoute (..),
    newIpRoute,
    ipRoute_cidrIp,
    ipRoute_description,

    -- * IpRouteInfo
    IpRouteInfo (..),
    newIpRouteInfo,
    ipRouteInfo_directoryId,
    ipRouteInfo_ipRouteStatusReason,
    ipRouteInfo_addedDateTime,
    ipRouteInfo_cidrIp,
    ipRouteInfo_ipRouteStatusMsg,
    ipRouteInfo_description,

    -- * LDAPSSettingInfo
    LDAPSSettingInfo (..),
    newLDAPSSettingInfo,
    lDAPSSettingInfo_lastUpdatedDateTime,
    lDAPSSettingInfo_lDAPSStatusReason,
    lDAPSSettingInfo_lDAPSStatus,

    -- * LogSubscription
    LogSubscription (..),
    newLogSubscription,
    logSubscription_directoryId,
    logSubscription_logGroupName,
    logSubscription_subscriptionCreatedDateTime,

    -- * OwnerDirectoryDescription
    OwnerDirectoryDescription (..),
    newOwnerDirectoryDescription,
    ownerDirectoryDescription_radiusStatus,
    ownerDirectoryDescription_directoryId,
    ownerDirectoryDescription_radiusSettings,
    ownerDirectoryDescription_accountId,
    ownerDirectoryDescription_dnsIpAddrs,
    ownerDirectoryDescription_vpcSettings,

    -- * RadiusSettings
    RadiusSettings (..),
    newRadiusSettings,
    radiusSettings_displayLabel,
    radiusSettings_radiusRetries,
    radiusSettings_authenticationProtocol,
    radiusSettings_radiusServers,
    radiusSettings_useSameUsername,
    radiusSettings_sharedSecret,
    radiusSettings_radiusTimeout,
    radiusSettings_radiusPort,

    -- * RegionDescription
    RegionDescription (..),
    newRegionDescription,
    regionDescription_status,
    regionDescription_directoryId,
    regionDescription_regionName,
    regionDescription_desiredNumberOfDomainControllers,
    regionDescription_regionType,
    regionDescription_launchTime,
    regionDescription_lastUpdatedDateTime,
    regionDescription_statusLastUpdatedDateTime,
    regionDescription_vpcSettings,

    -- * RegionsInfo
    RegionsInfo (..),
    newRegionsInfo,
    regionsInfo_primaryRegion,
    regionsInfo_additionalRegions,

    -- * SchemaExtensionInfo
    SchemaExtensionInfo (..),
    newSchemaExtensionInfo,
    schemaExtensionInfo_directoryId,
    schemaExtensionInfo_schemaExtensionId,
    schemaExtensionInfo_schemaExtensionStatusReason,
    schemaExtensionInfo_schemaExtensionStatus,
    schemaExtensionInfo_description,
    schemaExtensionInfo_endDateTime,
    schemaExtensionInfo_startDateTime,

    -- * ShareTarget
    ShareTarget (..),
    newShareTarget,
    shareTarget_id,
    shareTarget_type,

    -- * SharedDirectory
    SharedDirectory (..),
    newSharedDirectory,
    sharedDirectory_sharedAccountId,
    sharedDirectory_ownerAccountId,
    sharedDirectory_lastUpdatedDateTime,
    sharedDirectory_shareStatus,
    sharedDirectory_shareMethod,
    sharedDirectory_ownerDirectoryId,
    sharedDirectory_sharedDirectoryId,
    sharedDirectory_shareNotes,
    sharedDirectory_createdDateTime,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_status,
    snapshot_directoryId,
    snapshot_startTime,
    snapshot_name,
    snapshot_type,
    snapshot_snapshotId,

    -- * SnapshotLimits
    SnapshotLimits (..),
    newSnapshotLimits,
    snapshotLimits_manualSnapshotsLimitReached,
    snapshotLimits_manualSnapshotsCurrentCount,
    snapshotLimits_manualSnapshotsLimit,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Trust
    Trust (..),
    newTrust,
    trust_directoryId,
    trust_trustState,
    trust_lastUpdatedDateTime,
    trust_trustDirection,
    trust_stateLastUpdatedDateTime,
    trust_trustType,
    trust_trustStateReason,
    trust_selectiveAuth,
    trust_remoteDomainName,
    trust_trustId,
    trust_createdDateTime,

    -- * UnshareTarget
    UnshareTarget (..),
    newUnshareTarget,
    unshareTarget_id,
    unshareTarget_type,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.Attribute
import Network.AWS.DirectoryService.Types.Certificate
import Network.AWS.DirectoryService.Types.CertificateInfo
import Network.AWS.DirectoryService.Types.CertificateState
import Network.AWS.DirectoryService.Types.CertificateType
import Network.AWS.DirectoryService.Types.ClientAuthenticationSettingInfo
import Network.AWS.DirectoryService.Types.ClientAuthenticationStatus
import Network.AWS.DirectoryService.Types.ClientAuthenticationType
import Network.AWS.DirectoryService.Types.ClientCertAuthSettings
import Network.AWS.DirectoryService.Types.Computer
import Network.AWS.DirectoryService.Types.ConditionalForwarder
import Network.AWS.DirectoryService.Types.DirectoryConnectSettings
import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
import Network.AWS.DirectoryService.Types.DirectoryDescription
import Network.AWS.DirectoryService.Types.DirectoryEdition
import Network.AWS.DirectoryService.Types.DirectoryLimits
import Network.AWS.DirectoryService.Types.DirectorySize
import Network.AWS.DirectoryService.Types.DirectoryStage
import Network.AWS.DirectoryService.Types.DirectoryType
import Network.AWS.DirectoryService.Types.DirectoryVpcSettings
import Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
import Network.AWS.DirectoryService.Types.DomainController
import Network.AWS.DirectoryService.Types.DomainControllerStatus
import Network.AWS.DirectoryService.Types.EventTopic
import Network.AWS.DirectoryService.Types.IpRoute
import Network.AWS.DirectoryService.Types.IpRouteInfo
import Network.AWS.DirectoryService.Types.IpRouteStatusMsg
import Network.AWS.DirectoryService.Types.LDAPSSettingInfo
import Network.AWS.DirectoryService.Types.LDAPSStatus
import Network.AWS.DirectoryService.Types.LDAPSType
import Network.AWS.DirectoryService.Types.LogSubscription
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RadiusStatus
import Network.AWS.DirectoryService.Types.RegionDescription
import Network.AWS.DirectoryService.Types.RegionType
import Network.AWS.DirectoryService.Types.RegionsInfo
import Network.AWS.DirectoryService.Types.ReplicationScope
import Network.AWS.DirectoryService.Types.SchemaExtensionInfo
import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
import Network.AWS.DirectoryService.Types.SelectiveAuth
import Network.AWS.DirectoryService.Types.ShareMethod
import Network.AWS.DirectoryService.Types.ShareStatus
import Network.AWS.DirectoryService.Types.ShareTarget
import Network.AWS.DirectoryService.Types.SharedDirectory
import Network.AWS.DirectoryService.Types.Snapshot
import Network.AWS.DirectoryService.Types.SnapshotLimits
import Network.AWS.DirectoryService.Types.SnapshotStatus
import Network.AWS.DirectoryService.Types.SnapshotType
import Network.AWS.DirectoryService.Types.Tag
import Network.AWS.DirectoryService.Types.TargetType
import Network.AWS.DirectoryService.Types.TopicStatus
import Network.AWS.DirectoryService.Types.Trust
import Network.AWS.DirectoryService.Types.TrustDirection
import Network.AWS.DirectoryService.Types.TrustState
import Network.AWS.DirectoryService.Types.TrustType
import Network.AWS.DirectoryService.Types.UnshareTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "DirectoryService",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "ds",
      Core._serviceSigningName = "ds",
      Core._serviceVersion = "2015-04-16",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DirectoryService",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The certificate could not be added because the certificate limit has
-- been reached.
_CertificateLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CertificateLimitExceededException"

-- | The certificate has already been registered into the system.
_CertificateAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "CertificateAlreadyExistsException"

-- | Client authentication is not available in this region at this time.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The specified directory is unavailable or could not be found.
_DirectoryUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryUnavailableException =
  Core._MatchServiceError
    defaultService
    "DirectoryUnavailableException"

-- | An authentication error occurred.
_AuthenticationFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthenticationFailedException =
  Core._MatchServiceError
    defaultService
    "AuthenticationFailedException"

-- | One or more parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The operation is not supported.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | The specified entity already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyExistsException"

-- | Client authentication setup could not be completed because at least one
-- valid certificate must be registered in the system.
_NoAvailableCertificateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoAvailableCertificateException =
  Core._MatchServiceError
    defaultService
    "NoAvailableCertificateException"

-- | The user provided a username that does not exist in your directory.
_UserDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "UserDoesNotExistException"

-- | The maximum number of directories in the region has been reached. You
-- can use the GetDirectoryLimits operation to determine your directory
-- limits in the region.
_DirectoryLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DirectoryLimitExceededException"

-- | The LDAP activities could not be performed because they are limited by
-- the LDAPS status.
_InvalidLDAPSStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLDAPSStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidLDAPSStatusException"

-- | The certificate PEM that was provided has incorrect encoding.
_InvalidCertificateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateException"

-- | The certificate is being used for the LDAP security connection and
-- cannot be removed without disabling LDAP security.
_CertificateInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateInUseException =
  Core._MatchServiceError
    defaultService
    "CertificateInUseException"

-- | You have reached the limit for maximum number of simultaneous Region
-- replications per directory.
_RegionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RegionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RegionLimitExceededException"

-- | The maximum allowed number of IP addresses was exceeded. The default
-- limit is 100 IP address blocks.
_IpRouteLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IpRouteLimitExceededException =
  Core._MatchServiceError
    defaultService
    "IpRouteLimitExceededException"

-- | The maximum number of Amazon Web Services accounts that you can share
-- with this directory has been reached.
_ShareLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ShareLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ShareLimitExceededException"

-- | The specified entity could not be found.
_EntityDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "EntityDoesNotExistException"

-- | Exception encountered while trying to access your Amazon Web Services
-- organization.
_OrganizationsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OrganizationsException =
  Core._MatchServiceError
    defaultService
    "OrganizationsException"

-- | The specified shared target is not valid.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | The Region you specified is the same Region where the Managed Microsoft
-- AD directory was created. Specify a different Region and try again.
_DirectoryAlreadyInRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadyInRegionException =
  Core._MatchServiceError
    defaultService
    "DirectoryAlreadyInRegionException"

-- | The account does not have sufficient permission to perform the
-- operation.
_InsufficientPermissionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientPermissionsException =
  Core._MatchServiceError
    defaultService
    "InsufficientPermissionsException"

-- | The specified directory has not been shared with this Amazon Web
-- Services account.
_DirectoryNotSharedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNotSharedException =
  Core._MatchServiceError
    defaultService
    "DirectoryNotSharedException"

-- | The @NextToken@ value is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | An exception has occurred in Directory Service.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | The maximum number of manual snapshots for the directory has been
-- reached. You can use the GetSnapshotLimits operation to determine the
-- snapshot limits for a directory.
_SnapshotLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotLimitExceededException =
  Core._MatchServiceError
    defaultService
    "SnapshotLimitExceededException"

-- | The maximum allowed number of domain controllers per directory was
-- exceeded. The default limit per directory is 20 domain controllers.
_DomainControllerLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainControllerLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DomainControllerLimitExceededException"

-- | The specified directory does not exist in the system.
_DirectoryDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DirectoryDoesNotExistException"

-- | Client authentication is already enabled.
_InvalidClientAuthStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientAuthStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidClientAuthStatusException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | A client exception has occurred.
_ClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"

-- | The specified directory has already been shared with this Amazon Web
-- Services account.
_DirectoryAlreadySharedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadySharedException =
  Core._MatchServiceError
    defaultService
    "DirectoryAlreadySharedException"

-- | The certificate is not present in the system for describe or deregister
-- activities.
_CertificateDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CertificateDoesNotExistException"

-- | The new password provided by the user does not meet the password
-- complexity requirements defined in your directory.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"
