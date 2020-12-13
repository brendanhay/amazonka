-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types
  ( -- * Service configuration
    directoryServiceService,

    -- * Errors

    -- * CertificateState
    CertificateState (..),

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

    -- * IPRouteStatusMsg
    IPRouteStatusMsg (..),

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
    mkAttribute,
    aValue,
    aName,

    -- * Certificate
    Certificate (..),
    mkCertificate,
    cState,
    cCommonName,
    cCertificateId,
    cExpiryDateTime,
    cRegisteredDateTime,
    cStateReason,

    -- * CertificateInfo
    CertificateInfo (..),
    mkCertificateInfo,
    ciState,
    ciCommonName,
    ciCertificateId,
    ciExpiryDateTime,

    -- * Computer
    Computer (..),
    mkComputer,
    cComputerId,
    cComputerAttributes,
    cComputerName,

    -- * ConditionalForwarder
    ConditionalForwarder (..),
    mkConditionalForwarder,
    cfDNSIPAddrs,
    cfRemoteDomainName,
    cfReplicationScope,

    -- * DirectoryConnectSettings
    DirectoryConnectSettings (..),
    mkDirectoryConnectSettings,
    dcsCustomerUserName,
    dcsSubnetIds,
    dcsVPCId,
    dcsCustomerDNSIPs,

    -- * DirectoryConnectSettingsDescription
    DirectoryConnectSettingsDescription (..),
    mkDirectoryConnectSettingsDescription,
    dcsdCustomerUserName,
    dcsdSubnetIds,
    dcsdVPCId,
    dcsdSecurityGroupId,
    dcsdConnectIPs,
    dcsdAvailabilityZones,

    -- * DirectoryDescription
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

    -- * DirectoryLimits
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

    -- * DirectoryVPCSettings
    DirectoryVPCSettings (..),
    mkDirectoryVPCSettings,
    dvsSubnetIds,
    dvsVPCId,

    -- * DirectoryVPCSettingsDescription
    DirectoryVPCSettingsDescription (..),
    mkDirectoryVPCSettingsDescription,
    dvsdSubnetIds,
    dvsdVPCId,
    dvsdSecurityGroupId,
    dvsdAvailabilityZones,

    -- * DomainController
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

    -- * EventTopic
    EventTopic (..),
    mkEventTopic,
    etStatus,
    etDirectoryId,
    etTopicName,
    etTopicARN,
    etCreatedDateTime,

    -- * IPRoute
    IPRoute (..),
    mkIPRoute,
    irCidrIP,
    irDescription,

    -- * IPRouteInfo
    IPRouteInfo (..),
    mkIPRouteInfo,
    iriDirectoryId,
    iriIPRouteStatusReason,
    iriAddedDateTime,
    iriCidrIP,
    iriIPRouteStatusMsg,
    iriDescription,

    -- * LDAPSSettingInfo
    LDAPSSettingInfo (..),
    mkLDAPSSettingInfo,
    ldapssiLastUpdatedDateTime,
    ldapssiLDAPSStatusReason,
    ldapssiLDAPSStatus,

    -- * LogSubscription
    LogSubscription (..),
    mkLogSubscription,
    lsDirectoryId,
    lsLogGroupName,
    lsSubscriptionCreatedDateTime,

    -- * OwnerDirectoryDescription
    OwnerDirectoryDescription (..),
    mkOwnerDirectoryDescription,
    oddRadiusStatus,
    oddDirectoryId,
    oddRadiusSettings,
    oddAccountId,
    oddDNSIPAddrs,
    oddVPCSettings,

    -- * RadiusSettings
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

    -- * RegionDescription
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

    -- * RegionsInfo
    RegionsInfo (..),
    mkRegionsInfo,
    riPrimaryRegion,
    riAdditionalRegions,

    -- * SchemaExtensionInfo
    SchemaExtensionInfo (..),
    mkSchemaExtensionInfo,
    seiDirectoryId,
    seiSchemaExtensionId,
    seiSchemaExtensionStatusReason,
    seiSchemaExtensionStatus,
    seiDescription,
    seiEndDateTime,
    seiStartDateTime,

    -- * ShareTarget
    ShareTarget (..),
    mkShareTarget,
    stId,
    stType,

    -- * SharedDirectory
    SharedDirectory (..),
    mkSharedDirectory,
    sdSharedAccountId,
    sdOwnerAccountId,
    sdLastUpdatedDateTime,
    sdShareStatus,
    sdShareMethod,
    sdOwnerDirectoryId,
    sdSharedDirectoryId,
    sdShareNotes,
    sdCreatedDateTime,

    -- * Snapshot
    Snapshot (..),
    mkSnapshot,
    sStatus,
    sDirectoryId,
    sStartTime,
    sName,
    sType,
    sSnapshotId,

    -- * SnapshotLimits
    SnapshotLimits (..),
    mkSnapshotLimits,
    slManualSnapshotsLimitReached,
    slManualSnapshotsCurrentCount,
    slManualSnapshotsLimit,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Trust
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

    -- * UnshareTarget
    UnshareTarget (..),
    mkUnshareTarget,
    utId,
    utType,
  )
where

import Network.AWS.DirectoryService.Types.Attribute
import Network.AWS.DirectoryService.Types.Certificate
import Network.AWS.DirectoryService.Types.CertificateInfo
import Network.AWS.DirectoryService.Types.CertificateState
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
import Network.AWS.DirectoryService.Types.DirectoryVPCSettings
import Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
import Network.AWS.DirectoryService.Types.DomainController
import Network.AWS.DirectoryService.Types.DomainControllerStatus
import Network.AWS.DirectoryService.Types.EventTopic
import Network.AWS.DirectoryService.Types.IPRoute
import Network.AWS.DirectoryService.Types.IPRouteInfo
import Network.AWS.DirectoryService.Types.IPRouteStatusMsg
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
directoryServiceService :: Lude.Service
directoryServiceService =
  Lude.Service
    { Lude._svcAbbrev = "DirectoryService",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "ds",
      Lude._svcVersion = "2015-04-16",
      Lude._svcEndpoint = Lude.defaultEndpoint directoryServiceService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DirectoryService",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
