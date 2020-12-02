{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types
  ( -- * Service Configuration
    directoryService,

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
    Attribute,
    attribute,
    aValue,
    aName,

    -- * Certificate
    Certificate,
    certificate,
    cState,
    cCommonName,
    cCertificateId,
    cExpiryDateTime,
    cRegisteredDateTime,
    cStateReason,

    -- * CertificateInfo
    CertificateInfo,
    certificateInfo,
    ciState,
    ciCommonName,
    ciCertificateId,
    ciExpiryDateTime,

    -- * Computer
    Computer,
    computer,
    cComputerId,
    cComputerAttributes,
    cComputerName,

    -- * ConditionalForwarder
    ConditionalForwarder,
    conditionalForwarder,
    cfDNSIPAddrs,
    cfRemoteDomainName,
    cfReplicationScope,

    -- * DirectoryConnectSettings
    DirectoryConnectSettings,
    directoryConnectSettings,
    dcsVPCId,
    dcsSubnetIds,
    dcsCustomerDNSIPs,
    dcsCustomerUserName,

    -- * DirectoryConnectSettingsDescription
    DirectoryConnectSettingsDescription,
    directoryConnectSettingsDescription,
    dcsdCustomerUserName,
    dcsdSubnetIds,
    dcsdVPCId,
    dcsdSecurityGroupId,
    dcsdConnectIPs,
    dcsdAvailabilityZones,

    -- * DirectoryDescription
    DirectoryDescription,
    directoryDescription,
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
    DirectoryLimits,
    directoryLimits,
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
    DirectoryVPCSettings,
    directoryVPCSettings,
    dvsVPCId,
    dvsSubnetIds,

    -- * DirectoryVPCSettingsDescription
    DirectoryVPCSettingsDescription,
    directoryVPCSettingsDescription,
    dvsdSubnetIds,
    dvsdVPCId,
    dvsdSecurityGroupId,
    dvsdAvailabilityZones,

    -- * DomainController
    DomainController,
    domainController,
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
    EventTopic,
    eventTopic,
    etStatus,
    etDirectoryId,
    etTopicName,
    etTopicARN,
    etCreatedDateTime,

    -- * IPRoute
    IPRoute,
    ipRoute,
    irCidrIP,
    irDescription,

    -- * IPRouteInfo
    IPRouteInfo,
    ipRouteInfo,
    iriDirectoryId,
    iriIPRouteStatusReason,
    iriAddedDateTime,
    iriCidrIP,
    iriIPRouteStatusMsg,
    iriDescription,

    -- * LDAPSSettingInfo
    LDAPSSettingInfo,
    lDAPSSettingInfo,
    ldapssiLastUpdatedDateTime,
    ldapssiLDAPSStatusReason,
    ldapssiLDAPSStatus,

    -- * LogSubscription
    LogSubscription,
    logSubscription,
    lsDirectoryId,
    lsLogGroupName,
    lsSubscriptionCreatedDateTime,

    -- * OwnerDirectoryDescription
    OwnerDirectoryDescription,
    ownerDirectoryDescription,
    oddRadiusStatus,
    oddDirectoryId,
    oddRadiusSettings,
    oddAccountId,
    oddDNSIPAddrs,
    oddVPCSettings,

    -- * RadiusSettings
    RadiusSettings,
    radiusSettings,
    rsDisplayLabel,
    rsRadiusRetries,
    rsAuthenticationProtocol,
    rsRadiusServers,
    rsUseSameUsername,
    rsSharedSecret,
    rsRadiusTimeout,
    rsRadiusPort,

    -- * RegionDescription
    RegionDescription,
    regionDescription,
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
    RegionsInfo,
    regionsInfo,
    riPrimaryRegion,
    riAdditionalRegions,

    -- * SchemaExtensionInfo
    SchemaExtensionInfo,
    schemaExtensionInfo,
    seiDirectoryId,
    seiSchemaExtensionId,
    seiSchemaExtensionStatusReason,
    seiSchemaExtensionStatus,
    seiDescription,
    seiEndDateTime,
    seiStartDateTime,

    -- * ShareTarget
    ShareTarget,
    shareTarget,
    stId,
    stType,

    -- * SharedDirectory
    SharedDirectory,
    sharedDirectory,
    sSharedAccountId,
    sOwnerAccountId,
    sLastUpdatedDateTime,
    sShareStatus,
    sShareMethod,
    sOwnerDirectoryId,
    sSharedDirectoryId,
    sShareNotes,
    sCreatedDateTime,

    -- * Snapshot
    Snapshot,
    snapshot,
    sStatus,
    sDirectoryId,
    sStartTime,
    sName,
    sType,
    sSnapshotId,

    -- * SnapshotLimits
    SnapshotLimits,
    snapshotLimits,
    slManualSnapshotsLimitReached,
    slManualSnapshotsCurrentCount,
    slManualSnapshotsLimit,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * Trust
    Trust,
    trust,
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
    UnshareTarget,
    unshareTarget,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
directoryService :: Service
directoryService =
  Service
    { _svcAbbrev = "DirectoryService",
      _svcSigner = v4,
      _svcPrefix = "ds",
      _svcVersion = "2015-04-16",
      _svcEndpoint = defaultEndpoint directoryService,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DirectoryService",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
