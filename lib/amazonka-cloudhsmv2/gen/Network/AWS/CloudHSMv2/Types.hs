-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _CloudHsmInternalFailureException,
    _CloudHsmServiceException,
    _CloudHsmInvalidRequestException,
    _CloudHsmAccessDeniedException,
    _CloudHsmResourceNotFoundException,
    _CloudHsmTagException,

    -- * PreCoPassword
    PreCoPassword (..),

    -- * StateMessage
    StateMessage (..),

    -- * IpAddress
    IpAddress (..),

    -- * EniId
    EniId (..),

    -- * ResourceId
    ResourceId (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Field
    Field (..),

    -- * Cluster
    Cluster (..),
    mkCluster,
    cBackupPolicy,
    cBackupRetentionPolicy,
    cCertificates,
    cClusterId,
    cCreateTimestamp,
    cHsmType,
    cHsms,
    cPreCoPassword,
    cSecurityGroup,
    cSourceBackupId,
    cState,
    cStateMessage,
    cSubnetMapping,
    cTagList,
    cVpcId,

    -- * BackupRetentionPolicy
    BackupRetentionPolicy (..),
    mkBackupRetentionPolicy,
    brpType,
    brpValue,

    -- * BackupRetentionType
    BackupRetentionType (..),

    -- * ClusterState
    ClusterState (..),

    -- * String
    String (..),

    -- * VpcId
    VpcId (..),

    -- * BackupId
    BackupId (..),

    -- * HsmId
    HsmId (..),

    -- * Hsm
    Hsm (..),
    mkHsm,
    hHsmId,
    hAvailabilityZone,
    hClusterId,
    hEniId,
    hEniIp,
    hState,
    hStateMessage,
    hSubnetId,

    -- * ExternalAz
    ExternalAz (..),

    -- * Backup
    Backup (..),
    mkBackup,
    bBackupId,
    bBackupState,
    bClusterId,
    bCopyTimestamp,
    bCreateTimestamp,
    bDeleteTimestamp,
    bNeverExpires,
    bSourceBackup,
    bSourceCluster,
    bSourceRegion,
    bTagList,

    -- * SubnetId
    SubnetId (..),

    -- * Certificates
    Certificates (..),
    mkCertificates,
    cAwsHardwareCertificate,
    cClusterCertificate,
    cClusterCsr,
    cHsmCertificate,
    cManufacturerHardwareCertificate,

    -- * SecurityGroup
    SecurityGroup (..),

    -- * NextToken
    NextToken (..),

    -- * ClusterId
    ClusterId (..),

    -- * DestinationBackup
    DestinationBackup (..),
    mkDestinationBackup,
    dbCreateTimestamp,
    dbSourceBackup,
    dbSourceCluster,
    dbSourceRegion,

    -- * TagKey
    TagKey (..),

    -- * Region
    Region (..),

    -- * Cert
    Cert (..),

    -- * BackupPolicy
    BackupPolicy (..),

    -- * HsmType
    HsmType (..),

    -- * BackupState
    BackupState (..),

    -- * HsmState
    HsmState (..),

    -- * SourceBackupId
    SourceBackupId (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * AvailabilityZone
    AvailabilityZone (..),

    -- * SourceCluster
    SourceCluster (..),

    -- * SourceRegion
    SourceRegion (..),

    -- * AwsHardwareCertificate
    AwsHardwareCertificate (..),

    -- * ClusterCertificate
    ClusterCertificate (..),

    -- * ClusterCsr
    ClusterCsr (..),

    -- * HsmCertificate
    HsmCertificate (..),

    -- * ManufacturerHardwareCertificate
    ManufacturerHardwareCertificate (..),
  )
where

import Network.AWS.CloudHSMv2.Types.AvailabilityZone
import Network.AWS.CloudHSMv2.Types.AwsHardwareCertificate
import Network.AWS.CloudHSMv2.Types.Backup
import Network.AWS.CloudHSMv2.Types.BackupId
import Network.AWS.CloudHSMv2.Types.BackupPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
import Network.AWS.CloudHSMv2.Types.BackupRetentionType
import Network.AWS.CloudHSMv2.Types.BackupState
import Network.AWS.CloudHSMv2.Types.Cert
import Network.AWS.CloudHSMv2.Types.Certificates
import Network.AWS.CloudHSMv2.Types.Cluster
import Network.AWS.CloudHSMv2.Types.ClusterCertificate
import Network.AWS.CloudHSMv2.Types.ClusterCsr
import Network.AWS.CloudHSMv2.Types.ClusterId
import Network.AWS.CloudHSMv2.Types.ClusterState
import Network.AWS.CloudHSMv2.Types.DestinationBackup
import Network.AWS.CloudHSMv2.Types.EniId
import Network.AWS.CloudHSMv2.Types.ExternalAz
import Network.AWS.CloudHSMv2.Types.Field
import Network.AWS.CloudHSMv2.Types.Hsm
import Network.AWS.CloudHSMv2.Types.HsmCertificate
import Network.AWS.CloudHSMv2.Types.HsmId
import Network.AWS.CloudHSMv2.Types.HsmState
import Network.AWS.CloudHSMv2.Types.HsmType
import Network.AWS.CloudHSMv2.Types.IpAddress
import Network.AWS.CloudHSMv2.Types.Key
import Network.AWS.CloudHSMv2.Types.ManufacturerHardwareCertificate
import Network.AWS.CloudHSMv2.Types.NextToken
import Network.AWS.CloudHSMv2.Types.PreCoPassword
import Network.AWS.CloudHSMv2.Types.Region
import Network.AWS.CloudHSMv2.Types.ResourceId
import Network.AWS.CloudHSMv2.Types.SecurityGroup
import Network.AWS.CloudHSMv2.Types.SourceBackupId
import Network.AWS.CloudHSMv2.Types.SourceCluster
import Network.AWS.CloudHSMv2.Types.SourceRegion
import Network.AWS.CloudHSMv2.Types.StateMessage
import Network.AWS.CloudHSMv2.Types.String
import Network.AWS.CloudHSMv2.Types.SubnetId
import Network.AWS.CloudHSMv2.Types.Tag
import Network.AWS.CloudHSMv2.Types.TagKey
import Network.AWS.CloudHSMv2.Types.Value
import Network.AWS.CloudHSMv2.Types.VpcId
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-28@ of the Amazon CloudHSM V2 SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudHSMv2",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cloudhsmv2",
      Core._svcVersion = "2017-04-28",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CloudHSMv2",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request was rejected because of an AWS CloudHSM internal failure. The request can be retried.
_CloudHsmInternalFailureException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmInternalFailureException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmInternalFailureException"
{-# DEPRECATED _CloudHsmInternalFailureException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because an error occurred.
_CloudHsmServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmServiceException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmServiceException"
{-# DEPRECATED _CloudHsmServiceException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because it is not a valid request.
_CloudHsmInvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmInvalidRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmInvalidRequestException"
{-# DEPRECATED _CloudHsmInvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because the requester does not have permission to perform the requested operation.
_CloudHsmAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmAccessDeniedException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmAccessDeniedException"
{-# DEPRECATED _CloudHsmAccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because it refers to a resource that cannot be found.
_CloudHsmResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmResourceNotFoundException"
{-# DEPRECATED _CloudHsmResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because of a tagging failure. Verify the tag conditions in all applicable policies, and then retry the request.
_CloudHsmTagException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmTagException =
  Core._MatchServiceError mkServiceConfig "CloudHsmTagException"
{-# DEPRECATED _CloudHsmTagException "Use generic-lens or generic-optics instead." #-}
