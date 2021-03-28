-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _CertificateLimitExceededException
    , _CertificateAlreadyExistsException
    , _AccessDeniedException
    , _DirectoryUnavailableException
    , _AuthenticationFailedException
    , _InvalidParameterException
    , _UnsupportedOperationException
    , _EntityAlreadyExistsException
    , _NoAvailableCertificateException
    , _UserDoesNotExistException
    , _DirectoryLimitExceededException
    , _InvalidLDAPSStatusException
    , _InvalidCertificateException
    , _CertificateInUseException
    , _RegionLimitExceededException
    , _IpRouteLimitExceededException
    , _ShareLimitExceededException
    , _EntityDoesNotExistException
    , _OrganizationsException
    , _InvalidTargetException
    , _DirectoryAlreadyInRegionException
    , _InsufficientPermissionsException
    , _DirectoryNotSharedException
    , _InvalidNextTokenException
    , _ServiceException
    , _SnapshotLimitExceededException
    , _DomainControllerLimitExceededException
    , _DirectoryDoesNotExistException
    , _TagLimitExceededException
    , _ClientException
    , _DirectoryAlreadySharedException
    , _CertificateDoesNotExistException
    , _InvalidPasswordException

    -- * RequestId
    , RequestId (..)

    -- * DomainController
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

    -- * DirectoryShortName
    , DirectoryShortName (..)

    -- * Trust
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

    -- * RadiusStatus
    , RadiusStatus (..)

    -- * TargetId
    , TargetId (..)

    -- * Attribute
    , Attribute (..)
    , mkAttribute
    , aName
    , aValue

    -- * Snapshot
    , Snapshot (..)
    , mkSnapshot
    , sDirectoryId
    , sName
    , sSnapshotId
    , sStartTime
    , sStatus
    , sType

    -- * RadiusDisplayLabel
    , RadiusDisplayLabel (..)

    -- * DirectoryId
    , DirectoryId (..)

    -- * DirectoryLimits
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

    -- * AccessUrl
    , AccessUrl (..)

    -- * DirectoryVpcSettingsDescription
    , DirectoryVpcSettingsDescription (..)
    , mkDirectoryVpcSettingsDescription
    , dvsdAvailabilityZones
    , dvsdSecurityGroupId
    , dvsdSubnetIds
    , dvsdVpcId

    -- * Computer
    , Computer (..)
    , mkComputer
    , cComputerAttributes
    , cComputerId
    , cComputerName

    -- * RegionName
    , RegionName (..)

    -- * ResourceId
    , ResourceId (..)

    -- * DirectoryDescription
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

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * CustomerUserName
    , CustomerUserName (..)

    -- * IpAddr
    , IpAddr (..)

    -- * OrganizationalUnitDN
    , OrganizationalUnitDN (..)

    -- * CertificateState
    , CertificateState (..)

    -- * SchemaExtensionId
    , SchemaExtensionId (..)

    -- * RegionsInfo
    , RegionsInfo (..)
    , mkRegionsInfo
    , riAdditionalRegions
    , riPrimaryRegion

    -- * DirectoryStage
    , DirectoryStage (..)

    -- * DirectoryEdition
    , DirectoryEdition (..)

    -- * EventTopic
    , EventTopic (..)
    , mkEventTopic
    , etCreatedDateTime
    , etDirectoryId
    , etStatus
    , etTopicArn
    , etTopicName

    -- * TopicStatus
    , TopicStatus (..)

    -- * RadiusSettings
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

    -- * CertificateId
    , CertificateId (..)

    -- * VpcId
    , VpcId (..)

    -- * SnapshotStatus
    , SnapshotStatus (..)

    -- * SchemaExtensionStatusReason
    , SchemaExtensionStatusReason (..)

    -- * CertificateCN
    , CertificateCN (..)

    -- * RegionType
    , RegionType (..)

    -- * DomainControllerStatus
    , DomainControllerStatus (..)

    -- * IpRouteStatusReason
    , IpRouteStatusReason (..)

    -- * SharedDirectory
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

    -- * TrustState
    , TrustState (..)

    -- * RegionDescription
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

    -- * SchemaExtensionInfo
    , SchemaExtensionInfo (..)
    , mkSchemaExtensionInfo
    , seiDescription
    , seiDirectoryId
    , seiEndDateTime
    , seiSchemaExtensionId
    , seiSchemaExtensionStatus
    , seiSchemaExtensionStatusReason
    , seiStartDateTime

    -- * CertificateStateReason
    , CertificateStateReason (..)

    -- * UserName
    , UserName (..)

    -- * IpRouteInfo
    , IpRouteInfo (..)
    , mkIpRouteInfo
    , iriAddedDateTime
    , iriCidrIp
    , iriDescription
    , iriDirectoryId
    , iriIpRouteStatusMsg
    , iriIpRouteStatusReason

    -- * TopicName
    , TopicName (..)

    -- * RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- * SubnetId
    , SubnetId (..)

    -- * ConnectPassword
    , ConnectPassword (..)

    -- * CustomerId
    , CustomerId (..)

    -- * TrustDirection
    , TrustDirection (..)

    -- * DirectoryType
    , DirectoryType (..)

    -- * TargetType
    , TargetType (..)

    -- * AliasName
    , AliasName (..)

    -- * ShareStatus
    , ShareStatus (..)

    -- * LogGroupName
    , LogGroupName (..)

    -- * SecurityGroupId
    , SecurityGroupId (..)

    -- * NextToken
    , NextToken (..)

    -- * TopicArn
    , TopicArn (..)

    -- * SnapshotType
    , SnapshotType (..)

    -- * ShareTarget
    , ShareTarget (..)
    , mkShareTarget
    , stId
    , stType

    -- * SchemaExtensionStatus
    , SchemaExtensionStatus (..)

    -- * DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription (..)
    , mkDirectoryConnectSettingsDescription
    , dcsdAvailabilityZones
    , dcsdConnectIps
    , dcsdCustomerUserName
    , dcsdSecurityGroupId
    , dcsdSubnetIds
    , dcsdVpcId

    -- * DirectorySize
    , DirectorySize (..)

    -- * TrustType
    , TrustType (..)

    -- * UnshareTarget
    , UnshareTarget (..)
    , mkUnshareTarget
    , utId
    , utType

    -- * AvailabilityZone
    , AvailabilityZone (..)

    -- * TrustStateReason
    , TrustStateReason (..)

    -- * Password
    , Password (..)

    -- * LogSubscription
    , LogSubscription (..)
    , mkLogSubscription
    , lsDirectoryId
    , lsLogGroupName
    , lsSubscriptionCreatedDateTime

    -- * ConditionalForwarder
    , ConditionalForwarder (..)
    , mkConditionalForwarder
    , cfDnsIpAddrs
    , cfRemoteDomainName
    , cfReplicationScope

    -- * DirectoryConnectSettings
    , DirectoryConnectSettings (..)
    , mkDirectoryConnectSettings
    , dcsVpcId
    , dcsSubnetIds
    , dcsCustomerDnsIps
    , dcsCustomerUserName

    -- * ShareMethod
    , ShareMethod (..)

    -- * LDAPSSettingInfo
    , LDAPSSettingInfo (..)
    , mkLDAPSSettingInfo
    , ldapssiLDAPSStatus
    , ldapssiLDAPSStatusReason
    , ldapssiLastUpdatedDateTime

    -- * LDAPSStatusReason
    , LDAPSStatusReason (..)

    -- * SelectiveAuth
    , SelectiveAuth (..)

    -- * SnapshotLimits
    , SnapshotLimits (..)
    , mkSnapshotLimits
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit
    , slManualSnapshotsLimitReached

    -- * LDAPSType
    , LDAPSType (..)

    -- * CidrIp
    , CidrIp (..)

    -- * IpRouteStatusMsg
    , IpRouteStatusMsg (..)

    -- * Certificate
    , Certificate (..)
    , mkCertificate
    , cCertificateId
    , cCommonName
    , cExpiryDateTime
    , cRegisteredDateTime
    , cState
    , cStateReason

    -- * CertificateData
    , CertificateData (..)

    -- * TagKey
    , TagKey (..)

    -- * Server
    , Server (..)

    -- * IpRoute
    , IpRoute (..)
    , mkIpRoute
    , irCidrIp
    , irDescription

    -- * ComputerName
    , ComputerName (..)

    -- * StageReason
    , StageReason (..)

    -- * RemoteDomainName
    , RemoteDomainName (..)

    -- * CertificateInfo
    , CertificateInfo (..)
    , mkCertificateInfo
    , ciCertificateId
    , ciCommonName
    , ciExpiryDateTime
    , ciState

    -- * TrustPassword
    , TrustPassword (..)

    -- * LDAPSStatus
    , LDAPSStatus (..)

    -- * LdifContent
    , LdifContent (..)

    -- * OwnerDirectoryDescription
    , OwnerDirectoryDescription (..)
    , mkOwnerDirectoryDescription
    , oddAccountId
    , oddDirectoryId
    , oddDnsIpAddrs
    , oddRadiusSettings
    , oddRadiusStatus
    , oddVpcSettings

    -- * Description
    , Description (..)

    -- * ReplicationScope
    , ReplicationScope (..)

    -- * DirectoryName
    , DirectoryName (..)

    -- * TrustId
    , TrustId (..)

    -- * SnapshotId
    , SnapshotId (..)

    -- * DirectoryVpcSettings
    , DirectoryVpcSettings (..)
    , mkDirectoryVpcSettings
    , dvsVpcId
    , dvsSubnetIds

    -- * DomainControllerId
    , DomainControllerId (..)

    -- * DnsIpAddr
    , DnsIpAddr (..)

    -- * StatusReason
    , StatusReason (..)

    -- * Name
    , Name (..)

    -- * Value
    , Value (..)

    -- * ComputerId
    , ComputerId (..)

    -- * Alias
    , Alias (..)

    -- * ShareNotes
    , ShareNotes (..)

    -- * Key
    , Key (..)

    -- * NewPassword
    , NewPassword (..)

    -- * SharedSecret
    , SharedSecret (..)

    -- * OwnerAccountId
    , OwnerAccountId (..)

    -- * SharedAccountId
    , SharedAccountId (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.DirectoryService.Types.RequestId
  
import Network.AWS.DirectoryService.Types.DomainController
  
import Network.AWS.DirectoryService.Types.DirectoryShortName
  
  
import Network.AWS.DirectoryService.Types.Trust
  
import Network.AWS.DirectoryService.Types.RadiusStatus
  
import Network.AWS.DirectoryService.Types.TargetId
  
import Network.AWS.DirectoryService.Types.Attribute
  
import Network.AWS.DirectoryService.Types.Snapshot
  
  
import Network.AWS.DirectoryService.Types.RadiusDisplayLabel
  
  
  
  
import Network.AWS.DirectoryService.Types.DirectoryId
  
import Network.AWS.DirectoryService.Types.DirectoryLimits
  
import Network.AWS.DirectoryService.Types.AccessUrl
  
import Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
  
  
  
import Network.AWS.DirectoryService.Types.Computer
  
import Network.AWS.DirectoryService.Types.RegionName
  
import Network.AWS.DirectoryService.Types.ResourceId
  
import Network.AWS.DirectoryService.Types.DirectoryDescription
  
  
import Network.AWS.DirectoryService.Types.Tag
  
import Network.AWS.DirectoryService.Types.CustomerUserName
  
import Network.AWS.DirectoryService.Types.IpAddr
  
  
import Network.AWS.DirectoryService.Types.OrganizationalUnitDN
  
import Network.AWS.DirectoryService.Types.CertificateState
  
import Network.AWS.DirectoryService.Types.SchemaExtensionId
  
import Network.AWS.DirectoryService.Types.RegionsInfo
  
import Network.AWS.DirectoryService.Types.DirectoryStage
  
import Network.AWS.DirectoryService.Types.DirectoryEdition
  
import Network.AWS.DirectoryService.Types.EventTopic
  
import Network.AWS.DirectoryService.Types.TopicStatus
  
  
import Network.AWS.DirectoryService.Types.RadiusSettings
  
import Network.AWS.DirectoryService.Types.CertificateId
  
import Network.AWS.DirectoryService.Types.VpcId
  
import Network.AWS.DirectoryService.Types.SnapshotStatus
  
import Network.AWS.DirectoryService.Types.SchemaExtensionStatusReason
  
import Network.AWS.DirectoryService.Types.CertificateCN
  
import Network.AWS.DirectoryService.Types.RegionType
  
import Network.AWS.DirectoryService.Types.DomainControllerStatus
  
import Network.AWS.DirectoryService.Types.IpRouteStatusReason
  
  
import Network.AWS.DirectoryService.Types.SharedDirectory
  
  
  
import Network.AWS.DirectoryService.Types.TrustState
  
import Network.AWS.DirectoryService.Types.RegionDescription
  
import Network.AWS.DirectoryService.Types.SchemaExtensionInfo
  
import Network.AWS.DirectoryService.Types.CertificateStateReason
  
import Network.AWS.DirectoryService.Types.UserName
  
import Network.AWS.DirectoryService.Types.IpRouteInfo
  
import Network.AWS.DirectoryService.Types.TopicName
  
import Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
  
  
import Network.AWS.DirectoryService.Types.SubnetId
  
import Network.AWS.DirectoryService.Types.ConnectPassword
  
import Network.AWS.DirectoryService.Types.CustomerId
  
  
import Network.AWS.DirectoryService.Types.TrustDirection
  
import Network.AWS.DirectoryService.Types.DirectoryType
  
  
import Network.AWS.DirectoryService.Types.TargetType
  
import Network.AWS.DirectoryService.Types.AliasName
  
import Network.AWS.DirectoryService.Types.ShareStatus
  
import Network.AWS.DirectoryService.Types.LogGroupName
  
import Network.AWS.DirectoryService.Types.SecurityGroupId
  
import Network.AWS.DirectoryService.Types.NextToken
  
import Network.AWS.DirectoryService.Types.TopicArn
  
import Network.AWS.DirectoryService.Types.SnapshotType
  
  
import Network.AWS.DirectoryService.Types.ShareTarget
  
import Network.AWS.DirectoryService.Types.SchemaExtensionStatus
  
import Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
  
import Network.AWS.DirectoryService.Types.DirectorySize
  
import Network.AWS.DirectoryService.Types.TrustType
  
  
import Network.AWS.DirectoryService.Types.UnshareTarget
  
  
import Network.AWS.DirectoryService.Types.AvailabilityZone
  
  
import Network.AWS.DirectoryService.Types.TrustStateReason
  
import Network.AWS.DirectoryService.Types.Password
  
import Network.AWS.DirectoryService.Types.LogSubscription
  
  
import Network.AWS.DirectoryService.Types.ConditionalForwarder
  
import Network.AWS.DirectoryService.Types.DirectoryConnectSettings
  
import Network.AWS.DirectoryService.Types.ShareMethod
  
  
  
import Network.AWS.DirectoryService.Types.LDAPSSettingInfo
  
import Network.AWS.DirectoryService.Types.LDAPSStatusReason
  
import Network.AWS.DirectoryService.Types.SelectiveAuth
  
import Network.AWS.DirectoryService.Types.SnapshotLimits
  
  
  
import Network.AWS.DirectoryService.Types.LDAPSType
  
import Network.AWS.DirectoryService.Types.CidrIp
  
import Network.AWS.DirectoryService.Types.IpRouteStatusMsg
  
import Network.AWS.DirectoryService.Types.Certificate
  
  
import Network.AWS.DirectoryService.Types.CertificateData
  
  
  
import Network.AWS.DirectoryService.Types.TagKey
  
import Network.AWS.DirectoryService.Types.Server
  
import Network.AWS.DirectoryService.Types.IpRoute
  
import Network.AWS.DirectoryService.Types.ComputerName
  
import Network.AWS.DirectoryService.Types.StageReason
  
import Network.AWS.DirectoryService.Types.RemoteDomainName
  
import Network.AWS.DirectoryService.Types.CertificateInfo
  
  
import Network.AWS.DirectoryService.Types.TrustPassword
  
import Network.AWS.DirectoryService.Types.LDAPSStatus
  
import Network.AWS.DirectoryService.Types.LdifContent
  
import Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
  
import Network.AWS.DirectoryService.Types.Description
  
import Network.AWS.DirectoryService.Types.ReplicationScope
  
import Network.AWS.DirectoryService.Types.DirectoryName
  
  
import Network.AWS.DirectoryService.Types.TrustId
  
  
import Network.AWS.DirectoryService.Types.SnapshotId
  
import Network.AWS.DirectoryService.Types.DirectoryVpcSettings
  
  
  
import Network.AWS.DirectoryService.Types.DomainControllerId
  
import Network.AWS.DirectoryService.Types.DnsIpAddr
  
import Network.AWS.DirectoryService.Types.StatusReason
  
import Network.AWS.DirectoryService.Types.Name
  
import Network.AWS.DirectoryService.Types.Value
  
import Network.AWS.DirectoryService.Types.ComputerId
  
import Network.AWS.DirectoryService.Types.Alias
  
import Network.AWS.DirectoryService.Types.ShareNotes
  
import Network.AWS.DirectoryService.Types.Key
  
import Network.AWS.DirectoryService.Types.NewPassword
  
import Network.AWS.DirectoryService.Types.SharedSecret
  
import Network.AWS.DirectoryService.Types.OwnerAccountId
  
import Network.AWS.DirectoryService.Types.SharedAccountId
  

-- | API version @2015-04-16@ of the Amazon Directory Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "DirectoryService",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "ds",
                 Core._svcVersion = "2015-04-16", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "DirectoryService",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The certificate could not be added because the certificate limit has been reached.
_CertificateLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "CertificateLimitExceededException"
{-# INLINEABLE _CertificateLimitExceededException #-}
{-# DEPRECATED _CertificateLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate has already been registered into the system.
_CertificateAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "CertificateAlreadyExistsException"
{-# INLINEABLE _CertificateAlreadyExistsException #-}
{-# DEPRECATED _CertificateAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException
  = Core._MatchServiceError mkServiceConfig "AccessDeniedException"
{-# INLINEABLE _AccessDeniedException #-}
{-# DEPRECATED _AccessDeniedException "Use generic-lens or generic-optics instead"  #-}

-- | The specified directory is unavailable or could not be found.
_DirectoryUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryUnavailableException"
{-# INLINEABLE _DirectoryUnavailableException #-}
{-# DEPRECATED _DirectoryUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | An authentication error occurred.
_AuthenticationFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthenticationFailedException
  = Core._MatchServiceError mkServiceConfig
      "AuthenticationFailedException"
{-# INLINEABLE _AuthenticationFailedException #-}
{-# DEPRECATED _AuthenticationFailedException "Use generic-lens or generic-optics instead"  #-}

-- | One or more parameters are not valid.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | The operation is not supported.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedOperationException"
{-# INLINEABLE _UnsupportedOperationException #-}
{-# DEPRECATED _UnsupportedOperationException "Use generic-lens or generic-optics instead"  #-}

-- | The specified entity already exists.
_EntityAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyExistsException
  = Core._MatchServiceError mkServiceConfig
      "EntityAlreadyExistsException"
{-# INLINEABLE _EntityAlreadyExistsException #-}
{-# DEPRECATED _EntityAlreadyExistsException "Use generic-lens or generic-optics instead"  #-}

-- | The LDAP activities could not be performed because at least one valid certificate must be registered with the system.
_NoAvailableCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoAvailableCertificateException
  = Core._MatchServiceError mkServiceConfig
      "NoAvailableCertificateException"
{-# INLINEABLE _NoAvailableCertificateException #-}
{-# DEPRECATED _NoAvailableCertificateException "Use generic-lens or generic-optics instead"  #-}

-- | The user provided a username that does not exist in your directory.
_UserDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UserDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "UserDoesNotExistException"
{-# INLINEABLE _UserDoesNotExistException #-}
{-# DEPRECATED _UserDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of directories in the region has been reached. You can use the 'GetDirectoryLimits' operation to determine your directory limits in the region.
_DirectoryLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryLimitExceededException"
{-# INLINEABLE _DirectoryLimitExceededException #-}
{-# DEPRECATED _DirectoryLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The LDAP activities could not be performed because they are limited by the LDAPS status.
_InvalidLDAPSStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidLDAPSStatusException
  = Core._MatchServiceError mkServiceConfig
      "InvalidLDAPSStatusException"
{-# INLINEABLE _InvalidLDAPSStatusException #-}
{-# DEPRECATED _InvalidLDAPSStatusException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate PEM that was provided has incorrect encoding.
_InvalidCertificateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCertificateException
  = Core._MatchServiceError mkServiceConfig
      "InvalidCertificateException"
{-# INLINEABLE _InvalidCertificateException #-}
{-# DEPRECATED _InvalidCertificateException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate is being used for the LDAP security connection and cannot be removed without disabling LDAP security.
_CertificateInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateInUseException
  = Core._MatchServiceError mkServiceConfig
      "CertificateInUseException"
{-# INLINEABLE _CertificateInUseException #-}
{-# DEPRECATED _CertificateInUseException "Use generic-lens or generic-optics instead"  #-}

-- | You have reached the limit for maximum number of simultaneous region replications per directory.
_RegionLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RegionLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "RegionLimitExceededException"
{-# INLINEABLE _RegionLimitExceededException #-}
{-# DEPRECATED _RegionLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.
_IpRouteLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IpRouteLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "IpRouteLimitExceededException"
{-# INLINEABLE _IpRouteLimitExceededException #-}
{-# DEPRECATED _IpRouteLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of AWS accounts that you can share with this directory has been reached.
_ShareLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ShareLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "ShareLimitExceededException"
{-# INLINEABLE _ShareLimitExceededException #-}
{-# DEPRECATED _ShareLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified entity could not be found.
_EntityDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "EntityDoesNotExistException"
{-# INLINEABLE _EntityDoesNotExistException #-}
{-# DEPRECATED _EntityDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | Exception encountered while trying to access your AWS organization.
_OrganizationsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationsException
  = Core._MatchServiceError mkServiceConfig "OrganizationsException"
{-# INLINEABLE _OrganizationsException #-}
{-# DEPRECATED _OrganizationsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified shared target is not valid.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException
  = Core._MatchServiceError mkServiceConfig "InvalidTargetException"
{-# INLINEABLE _InvalidTargetException #-}
{-# DEPRECATED _InvalidTargetException "Use generic-lens or generic-optics instead"  #-}

-- | The Region you specified is the same Region where the AWS Managed Microsoft AD directory was created. Specify a different Region and try again.
_DirectoryAlreadyInRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadyInRegionException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryAlreadyInRegionException"
{-# INLINEABLE _DirectoryAlreadyInRegionException #-}
{-# DEPRECATED _DirectoryAlreadyInRegionException "Use generic-lens or generic-optics instead"  #-}

-- | The account does not have sufficient permission to perform the operation.
_InsufficientPermissionsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientPermissionsException
  = Core._MatchServiceError mkServiceConfig
      "InsufficientPermissionsException"
{-# INLINEABLE _InsufficientPermissionsException #-}
{-# DEPRECATED _InsufficientPermissionsException "Use generic-lens or generic-optics instead"  #-}

-- | The specified directory has not been shared with this AWS account.
_DirectoryNotSharedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryNotSharedException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryNotSharedException"
{-# INLINEABLE _DirectoryNotSharedException #-}
{-# DEPRECATED _DirectoryNotSharedException "Use generic-lens or generic-optics instead"  #-}

-- | The @NextToken@ value is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException
  = Core._MatchServiceError mkServiceConfig
      "InvalidNextTokenException"
{-# INLINEABLE _InvalidNextTokenException #-}
{-# DEPRECATED _InvalidNextTokenException "Use generic-lens or generic-optics instead"  #-}

-- | An exception has occurred in AWS Directory Service.
_ServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceException
  = Core._MatchServiceError mkServiceConfig "ServiceException"
{-# INLINEABLE _ServiceException #-}
{-# DEPRECATED _ServiceException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum number of manual snapshots for the directory has been reached. You can use the 'GetSnapshotLimits' operation to determine the snapshot limits for a directory.
_SnapshotLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SnapshotLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "SnapshotLimitExceededException"
{-# INLINEABLE _SnapshotLimitExceededException #-}
{-# DEPRECATED _SnapshotLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.
_DomainControllerLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DomainControllerLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "DomainControllerLimitExceededException"
{-# INLINEABLE _DomainControllerLimitExceededException #-}
{-# DEPRECATED _DomainControllerLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The specified directory does not exist in the system.
_DirectoryDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryDoesNotExistException"
{-# INLINEABLE _DirectoryDoesNotExistException #-}
{-# DEPRECATED _DirectoryDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The maximum allowed number of tags was exceeded.
_TagLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagLimitExceededException
  = Core._MatchServiceError mkServiceConfig
      "TagLimitExceededException"
{-# INLINEABLE _TagLimitExceededException #-}
{-# DEPRECATED _TagLimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | A client exception has occurred.
_ClientException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientException
  = Core._MatchServiceError mkServiceConfig "ClientException"
{-# INLINEABLE _ClientException #-}
{-# DEPRECATED _ClientException "Use generic-lens or generic-optics instead"  #-}

-- | The specified directory has already been shared with this AWS account.
_DirectoryAlreadySharedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryAlreadySharedException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryAlreadySharedException"
{-# INLINEABLE _DirectoryAlreadySharedException #-}
{-# DEPRECATED _DirectoryAlreadySharedException "Use generic-lens or generic-optics instead"  #-}

-- | The certificate is not present in the system for describe or deregister activities.
_CertificateDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CertificateDoesNotExistException
  = Core._MatchServiceError mkServiceConfig
      "CertificateDoesNotExistException"
{-# INLINEABLE _CertificateDoesNotExistException #-}
{-# DEPRECATED _CertificateDoesNotExistException "Use generic-lens or generic-optics instead"  #-}

-- | The new password provided by the user does not meet the password complexity requirements defined in your directory.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException
  = Core._MatchServiceError mkServiceConfig
      "InvalidPasswordException"
{-# INLINEABLE _InvalidPasswordException #-}
{-# DEPRECATED _InvalidPasswordException "Use generic-lens or generic-optics instead"  #-}
