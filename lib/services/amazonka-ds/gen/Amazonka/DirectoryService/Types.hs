{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DirectoryService.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _EntityAlreadyExistsException,
    _ClientException,
    _CertificateAlreadyExistsException,
    _UnsupportedOperationException,
    _InvalidLDAPSStatusException,
    _OrganizationsException,
    _InvalidCertificateException,
    _InsufficientPermissionsException,
    _IpRouteLimitExceededException,
    _AccessDeniedException,
    _ShareLimitExceededException,
    _EntityDoesNotExistException,
    _UnsupportedSettingsException,
    _InvalidPasswordException,
    _NoAvailableCertificateException,
    _RegionLimitExceededException,
    _DirectoryLimitExceededException,
    _InvalidNextTokenException,
    _DirectoryInDesiredStateException,
    _InvalidClientAuthStatusException,
    _CertificateLimitExceededException,
    _DomainControllerLimitExceededException,
    _TagLimitExceededException,
    _CertificateDoesNotExistException,
    _CertificateInUseException,
    _InvalidTargetException,
    _ServiceException,
    _SnapshotLimitExceededException,
    _DirectoryNotSharedException,
    _DirectoryDoesNotExistException,
    _DirectoryAlreadySharedException,
    _UserDoesNotExistException,
    _DirectoryUnavailableException,
    _AuthenticationFailedException,
    _DirectoryAlreadyInRegionException,
    _IncompatibleSettingsException,
    _InvalidParameterException,

    -- * CertificateState
    CertificateState (..),

    -- * CertificateType
    CertificateType (..),

    -- * ClientAuthenticationStatus
    ClientAuthenticationStatus (..),

    -- * ClientAuthenticationType
    ClientAuthenticationType (..),

    -- * DirectoryConfigurationStatus
    DirectoryConfigurationStatus (..),

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

    -- * OSVersion
    OSVersion (..),

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

    -- * UpdateStatus
    UpdateStatus (..),

    -- * UpdateType
    UpdateType (..),

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_name,
    attribute_value,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_type,
    certificate_registeredDateTime,
    certificate_state,
    certificate_certificateId,
    certificate_commonName,
    certificate_stateReason,
    certificate_clientCertAuthSettings,
    certificate_expiryDateTime,

    -- * CertificateInfo
    CertificateInfo (..),
    newCertificateInfo,
    certificateInfo_type,
    certificateInfo_state,
    certificateInfo_certificateId,
    certificateInfo_commonName,
    certificateInfo_expiryDateTime,

    -- * ClientAuthenticationSettingInfo
    ClientAuthenticationSettingInfo (..),
    newClientAuthenticationSettingInfo,
    clientAuthenticationSettingInfo_type,
    clientAuthenticationSettingInfo_status,
    clientAuthenticationSettingInfo_lastUpdatedDateTime,

    -- * ClientCertAuthSettings
    ClientCertAuthSettings (..),
    newClientCertAuthSettings,
    clientCertAuthSettings_oCSPUrl,

    -- * Computer
    Computer (..),
    newComputer,
    computer_computerAttributes,
    computer_computerName,
    computer_computerId,

    -- * ConditionalForwarder
    ConditionalForwarder (..),
    newConditionalForwarder,
    conditionalForwarder_remoteDomainName,
    conditionalForwarder_replicationScope,
    conditionalForwarder_dnsIpAddrs,

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
    directoryConnectSettingsDescription_connectIps,
    directoryConnectSettingsDescription_availabilityZones,
    directoryConnectSettingsDescription_securityGroupId,
    directoryConnectSettingsDescription_customerUserName,
    directoryConnectSettingsDescription_vpcId,
    directoryConnectSettingsDescription_subnetIds,

    -- * DirectoryDescription
    DirectoryDescription (..),
    newDirectoryDescription,
    directoryDescription_alias,
    directoryDescription_directoryId,
    directoryDescription_name,
    directoryDescription_regionsInfo,
    directoryDescription_type,
    directoryDescription_shareNotes,
    directoryDescription_launchTime,
    directoryDescription_shareStatus,
    directoryDescription_radiusStatus,
    directoryDescription_shareMethod,
    directoryDescription_size,
    directoryDescription_vpcSettings,
    directoryDescription_osVersion,
    directoryDescription_ownerDirectoryDescription,
    directoryDescription_edition,
    directoryDescription_description,
    directoryDescription_ssoEnabled,
    directoryDescription_accessUrl,
    directoryDescription_radiusSettings,
    directoryDescription_stage,
    directoryDescription_desiredNumberOfDomainControllers,
    directoryDescription_connectSettings,
    directoryDescription_stageReason,
    directoryDescription_stageLastUpdatedDateTime,
    directoryDescription_dnsIpAddrs,
    directoryDescription_shortName,

    -- * DirectoryLimits
    DirectoryLimits (..),
    newDirectoryLimits,
    directoryLimits_cloudOnlyDirectoriesLimitReached,
    directoryLimits_cloudOnlyMicrosoftADLimitReached,
    directoryLimits_cloudOnlyMicrosoftADCurrentCount,
    directoryLimits_cloudOnlyMicrosoftADLimit,
    directoryLimits_connectedDirectoriesLimitReached,
    directoryLimits_connectedDirectoriesLimit,
    directoryLimits_connectedDirectoriesCurrentCount,
    directoryLimits_cloudOnlyDirectoriesLimit,
    directoryLimits_cloudOnlyDirectoriesCurrentCount,

    -- * DirectoryVpcSettings
    DirectoryVpcSettings (..),
    newDirectoryVpcSettings,
    directoryVpcSettings_vpcId,
    directoryVpcSettings_subnetIds,

    -- * DirectoryVpcSettingsDescription
    DirectoryVpcSettingsDescription (..),
    newDirectoryVpcSettingsDescription,
    directoryVpcSettingsDescription_availabilityZones,
    directoryVpcSettingsDescription_securityGroupId,
    directoryVpcSettingsDescription_vpcId,
    directoryVpcSettingsDescription_subnetIds,

    -- * DomainController
    DomainController (..),
    newDomainController,
    domainController_statusLastUpdatedDateTime,
    domainController_directoryId,
    domainController_domainControllerId,
    domainController_dnsIpAddr,
    domainController_launchTime,
    domainController_subnetId,
    domainController_statusReason,
    domainController_status,
    domainController_availabilityZone,
    domainController_vpcId,

    -- * EventTopic
    EventTopic (..),
    newEventTopic,
    eventTopic_directoryId,
    eventTopic_createdDateTime,
    eventTopic_topicArn,
    eventTopic_status,
    eventTopic_topicName,

    -- * IpRoute
    IpRoute (..),
    newIpRoute,
    ipRoute_description,
    ipRoute_cidrIp,

    -- * IpRouteInfo
    IpRouteInfo (..),
    newIpRouteInfo,
    ipRouteInfo_ipRouteStatusMsg,
    ipRouteInfo_directoryId,
    ipRouteInfo_description,
    ipRouteInfo_cidrIp,
    ipRouteInfo_addedDateTime,
    ipRouteInfo_ipRouteStatusReason,

    -- * LDAPSSettingInfo
    LDAPSSettingInfo (..),
    newLDAPSSettingInfo,
    lDAPSSettingInfo_lDAPSStatusReason,
    lDAPSSettingInfo_lDAPSStatus,
    lDAPSSettingInfo_lastUpdatedDateTime,

    -- * LogSubscription
    LogSubscription (..),
    newLogSubscription,
    logSubscription_directoryId,
    logSubscription_subscriptionCreatedDateTime,
    logSubscription_logGroupName,

    -- * OSUpdateSettings
    OSUpdateSettings (..),
    newOSUpdateSettings,
    oSUpdateSettings_oSVersion,

    -- * OwnerDirectoryDescription
    OwnerDirectoryDescription (..),
    newOwnerDirectoryDescription,
    ownerDirectoryDescription_directoryId,
    ownerDirectoryDescription_radiusStatus,
    ownerDirectoryDescription_vpcSettings,
    ownerDirectoryDescription_radiusSettings,
    ownerDirectoryDescription_accountId,
    ownerDirectoryDescription_dnsIpAddrs,

    -- * RadiusSettings
    RadiusSettings (..),
    newRadiusSettings,
    radiusSettings_displayLabel,
    radiusSettings_authenticationProtocol,
    radiusSettings_radiusServers,
    radiusSettings_radiusRetries,
    radiusSettings_radiusPort,
    radiusSettings_sharedSecret,
    radiusSettings_useSameUsername,
    radiusSettings_radiusTimeout,

    -- * RegionDescription
    RegionDescription (..),
    newRegionDescription,
    regionDescription_statusLastUpdatedDateTime,
    regionDescription_directoryId,
    regionDescription_launchTime,
    regionDescription_vpcSettings,
    regionDescription_status,
    regionDescription_regionName,
    regionDescription_regionType,
    regionDescription_desiredNumberOfDomainControllers,
    regionDescription_lastUpdatedDateTime,

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
    schemaExtensionInfo_startDateTime,
    schemaExtensionInfo_schemaExtensionStatus,
    schemaExtensionInfo_description,
    schemaExtensionInfo_schemaExtensionStatusReason,
    schemaExtensionInfo_endDateTime,

    -- * Setting
    Setting (..),
    newSetting,
    setting_name,
    setting_value,

    -- * SettingEntry
    SettingEntry (..),
    newSettingEntry,
    settingEntry_requestDetailedStatus,
    settingEntry_name,
    settingEntry_type,
    settingEntry_requestStatusMessage,
    settingEntry_appliedValue,
    settingEntry_requestedValue,
    settingEntry_requestStatus,
    settingEntry_allowedValues,
    settingEntry_lastUpdatedDateTime,
    settingEntry_lastRequestedDateTime,

    -- * ShareTarget
    ShareTarget (..),
    newShareTarget,
    shareTarget_id,
    shareTarget_type,

    -- * SharedDirectory
    SharedDirectory (..),
    newSharedDirectory,
    sharedDirectory_shareNotes,
    sharedDirectory_shareStatus,
    sharedDirectory_shareMethod,
    sharedDirectory_createdDateTime,
    sharedDirectory_ownerDirectoryId,
    sharedDirectory_ownerAccountId,
    sharedDirectory_sharedAccountId,
    sharedDirectory_sharedDirectoryId,
    sharedDirectory_lastUpdatedDateTime,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_directoryId,
    snapshot_name,
    snapshot_type,
    snapshot_snapshotId,
    snapshot_status,
    snapshot_startTime,

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
    trust_remoteDomainName,
    trust_trustType,
    trust_trustId,
    trust_trustState,
    trust_createdDateTime,
    trust_selectiveAuth,
    trust_trustStateReason,
    trust_trustDirection,
    trust_stateLastUpdatedDateTime,
    trust_lastUpdatedDateTime,

    -- * UnshareTarget
    UnshareTarget (..),
    newUnshareTarget,
    unshareTarget_id,
    unshareTarget_type,

    -- * UpdateInfoEntry
    UpdateInfoEntry (..),
    newUpdateInfoEntry,
    updateInfoEntry_initiatedBy,
    updateInfoEntry_previousValue,
    updateInfoEntry_statusReason,
    updateInfoEntry_status,
    updateInfoEntry_region,
    updateInfoEntry_newValue,
    updateInfoEntry_startTime,
    updateInfoEntry_lastUpdatedDateTime,

    -- * UpdateValue
    UpdateValue (..),
    newUpdateValue,
    updateValue_oSUpdateSettings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types.Attribute
import Amazonka.DirectoryService.Types.Certificate
import Amazonka.DirectoryService.Types.CertificateInfo
import Amazonka.DirectoryService.Types.CertificateState
import Amazonka.DirectoryService.Types.CertificateType
import Amazonka.DirectoryService.Types.ClientAuthenticationSettingInfo
import Amazonka.DirectoryService.Types.ClientAuthenticationStatus
import Amazonka.DirectoryService.Types.ClientAuthenticationType
import Amazonka.DirectoryService.Types.ClientCertAuthSettings
import Amazonka.DirectoryService.Types.Computer
import Amazonka.DirectoryService.Types.ConditionalForwarder
import Amazonka.DirectoryService.Types.DirectoryConfigurationStatus
import Amazonka.DirectoryService.Types.DirectoryConnectSettings
import Amazonka.DirectoryService.Types.DirectoryConnectSettingsDescription
import Amazonka.DirectoryService.Types.DirectoryDescription
import Amazonka.DirectoryService.Types.DirectoryEdition
import Amazonka.DirectoryService.Types.DirectoryLimits
import Amazonka.DirectoryService.Types.DirectorySize
import Amazonka.DirectoryService.Types.DirectoryStage
import Amazonka.DirectoryService.Types.DirectoryType
import Amazonka.DirectoryService.Types.DirectoryVpcSettings
import Amazonka.DirectoryService.Types.DirectoryVpcSettingsDescription
import Amazonka.DirectoryService.Types.DomainController
import Amazonka.DirectoryService.Types.DomainControllerStatus
import Amazonka.DirectoryService.Types.EventTopic
import Amazonka.DirectoryService.Types.IpRoute
import Amazonka.DirectoryService.Types.IpRouteInfo
import Amazonka.DirectoryService.Types.IpRouteStatusMsg
import Amazonka.DirectoryService.Types.LDAPSSettingInfo
import Amazonka.DirectoryService.Types.LDAPSStatus
import Amazonka.DirectoryService.Types.LDAPSType
import Amazonka.DirectoryService.Types.LogSubscription
import Amazonka.DirectoryService.Types.OSUpdateSettings
import Amazonka.DirectoryService.Types.OSVersion
import Amazonka.DirectoryService.Types.OwnerDirectoryDescription
import Amazonka.DirectoryService.Types.RadiusAuthenticationProtocol
import Amazonka.DirectoryService.Types.RadiusSettings
import Amazonka.DirectoryService.Types.RadiusStatus
import Amazonka.DirectoryService.Types.RegionDescription
import Amazonka.DirectoryService.Types.RegionType
import Amazonka.DirectoryService.Types.RegionsInfo
import Amazonka.DirectoryService.Types.ReplicationScope
import Amazonka.DirectoryService.Types.SchemaExtensionInfo
import Amazonka.DirectoryService.Types.SchemaExtensionStatus
import Amazonka.DirectoryService.Types.SelectiveAuth
import Amazonka.DirectoryService.Types.Setting
import Amazonka.DirectoryService.Types.SettingEntry
import Amazonka.DirectoryService.Types.ShareMethod
import Amazonka.DirectoryService.Types.ShareStatus
import Amazonka.DirectoryService.Types.ShareTarget
import Amazonka.DirectoryService.Types.SharedDirectory
import Amazonka.DirectoryService.Types.Snapshot
import Amazonka.DirectoryService.Types.SnapshotLimits
import Amazonka.DirectoryService.Types.SnapshotStatus
import Amazonka.DirectoryService.Types.SnapshotType
import Amazonka.DirectoryService.Types.Tag
import Amazonka.DirectoryService.Types.TargetType
import Amazonka.DirectoryService.Types.TopicStatus
import Amazonka.DirectoryService.Types.Trust
import Amazonka.DirectoryService.Types.TrustDirection
import Amazonka.DirectoryService.Types.TrustState
import Amazonka.DirectoryService.Types.TrustType
import Amazonka.DirectoryService.Types.UnshareTarget
import Amazonka.DirectoryService.Types.UpdateInfoEntry
import Amazonka.DirectoryService.Types.UpdateStatus
import Amazonka.DirectoryService.Types.UpdateType
import Amazonka.DirectoryService.Types.UpdateValue
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DirectoryService",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ds",
      Core.signingName = "ds",
      Core.version = "2015-04-16",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DirectoryService",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified entity already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "EntityAlreadyExistsException"

-- | A client exception has occurred.
_ClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"

-- | The certificate has already been registered into the system.
_CertificateAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "CertificateAlreadyExistsException"

-- | The operation is not supported.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException =
  Core._MatchServiceError
    defaultService
    "UnsupportedOperationException"

-- | The LDAP activities could not be performed because they are limited by
-- the LDAPS status.
_InvalidLDAPSStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidLDAPSStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidLDAPSStatusException"

-- | Exception encountered while trying to access your Amazon Web Services
-- organization.
_OrganizationsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OrganizationsException =
  Core._MatchServiceError
    defaultService
    "OrganizationsException"

-- | The certificate PEM that was provided has incorrect encoding.
_InvalidCertificateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateException =
  Core._MatchServiceError
    defaultService
    "InvalidCertificateException"

-- | The account does not have sufficient permission to perform the
-- operation.
_InsufficientPermissionsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientPermissionsException =
  Core._MatchServiceError
    defaultService
    "InsufficientPermissionsException"

-- | The maximum allowed number of IP addresses was exceeded. The default
-- limit is 100 IP address blocks.
_IpRouteLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IpRouteLimitExceededException =
  Core._MatchServiceError
    defaultService
    "IpRouteLimitExceededException"

-- | Client authentication is not available in this region at this time.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

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

-- | The specified directory setting is not supported.
_UnsupportedSettingsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedSettingsException =
  Core._MatchServiceError
    defaultService
    "UnsupportedSettingsException"

-- | The new password provided by the user does not meet the password
-- complexity requirements defined in your directory.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException =
  Core._MatchServiceError
    defaultService
    "InvalidPasswordException"

-- | Client authentication setup could not be completed because at least one
-- valid certificate must be registered in the system.
_NoAvailableCertificateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoAvailableCertificateException =
  Core._MatchServiceError
    defaultService
    "NoAvailableCertificateException"

-- | You have reached the limit for maximum number of simultaneous Region
-- replications per directory.
_RegionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RegionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RegionLimitExceededException"

-- | The maximum number of directories in the region has been reached. You
-- can use the GetDirectoryLimits operation to determine your directory
-- limits in the region.
_DirectoryLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DirectoryLimitExceededException"

-- | The @NextToken@ value is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The directory is already updated to desired update type settings.
_DirectoryInDesiredStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryInDesiredStateException =
  Core._MatchServiceError
    defaultService
    "DirectoryInDesiredStateException"

-- | Client authentication is already enabled.
_InvalidClientAuthStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientAuthStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidClientAuthStatusException"

-- | The certificate could not be added because the certificate limit has
-- been reached.
_CertificateLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CertificateLimitExceededException"

-- | The maximum allowed number of domain controllers per directory was
-- exceeded. The default limit per directory is 20 domain controllers.
_DomainControllerLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainControllerLimitExceededException =
  Core._MatchServiceError
    defaultService
    "DomainControllerLimitExceededException"

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException =
  Core._MatchServiceError
    defaultService
    "TagLimitExceededException"

-- | The certificate is not present in the system for describe or deregister
-- activities.
_CertificateDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CertificateDoesNotExistException"

-- | The certificate is being used for the LDAP security connection and
-- cannot be removed without disabling LDAP security.
_CertificateInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateInUseException =
  Core._MatchServiceError
    defaultService
    "CertificateInUseException"

-- | The specified shared target is not valid.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetException"

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

-- | The specified directory has not been shared with this Amazon Web
-- Services account.
_DirectoryNotSharedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNotSharedException =
  Core._MatchServiceError
    defaultService
    "DirectoryNotSharedException"

-- | The specified directory does not exist in the system.
_DirectoryDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "DirectoryDoesNotExistException"

-- | The specified directory has already been shared with this Amazon Web
-- Services account.
_DirectoryAlreadySharedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadySharedException =
  Core._MatchServiceError
    defaultService
    "DirectoryAlreadySharedException"

-- | The user provided a username that does not exist in your directory.
_UserDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "UserDoesNotExistException"

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

-- | The Region you specified is the same Region where the Managed Microsoft
-- AD directory was created. Specify a different Region and try again.
_DirectoryAlreadyInRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadyInRegionException =
  Core._MatchServiceError
    defaultService
    "DirectoryAlreadyInRegionException"

-- | The specified directory setting is not compatible with other settings.
_IncompatibleSettingsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IncompatibleSettingsException =
  Core._MatchServiceError
    defaultService
    "IncompatibleSettingsException"

-- | One or more parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
