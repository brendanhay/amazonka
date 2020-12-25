-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidRequestException,
    _CloudHsmServiceException,
    _CloudHsmInternalException,

    -- * ClientArn
    ClientArn (..),

    -- * IamRoleArn
    IamRoleArn (..),

    -- * PaginationToken
    PaginationToken (..),

    -- * ClientLabel
    ClientLabel (..),

    -- * IpAddress
    IpAddress (..),

    -- * EniId
    EniId (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ClientToken
    ClientToken (..),

    -- * HsmSerialNumber
    HsmSerialNumber (..),

    -- * String
    String (..),

    -- * CertificateFingerprint
    CertificateFingerprint (..),

    -- * CloudHsmObjectState
    CloudHsmObjectState (..),

    -- * VpcId
    VpcId (..),

    -- * SubscriptionType
    SubscriptionType (..),

    -- * SubnetId
    SubnetId (..),

    -- * SshKey
    SshKey (..),

    -- * PartitionArn
    PartitionArn (..),

    -- * HsmStatus
    HsmStatus (..),

    -- * PartitionSerial
    PartitionSerial (..),

    -- * Certificate
    Certificate (..),

    -- * TagKey
    TagKey (..),

    -- * ExternalId
    ExternalId (..),

    -- * HapgArn
    HapgArn (..),

    -- * ClientVersion
    ClientVersion (..),

    -- * AZ
    AZ (..),

    -- * HsmArn
    HsmArn (..),

    -- * Label
    Label (..),

    -- * NextToken
    NextToken (..),

    -- * Status
    Status (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * ResourceArn
    ResourceArn (..),

    -- * LastModifiedTimestamp
    LastModifiedTimestamp (..),

    -- * AvailabilityZone
    AvailabilityZone (..),

    -- * ServerCertLastUpdated
    ServerCertLastUpdated (..),

    -- * SshKeyLastUpdated
    SshKeyLastUpdated (..),

    -- * SubscriptionEndDate
    SubscriptionEndDate (..),

    -- * SubscriptionStartDate
    SubscriptionStartDate (..),
  )
where

import Network.AWS.CloudHSM.Types.AZ
import Network.AWS.CloudHSM.Types.AvailabilityZone
import Network.AWS.CloudHSM.Types.Certificate
import Network.AWS.CloudHSM.Types.CertificateFingerprint
import Network.AWS.CloudHSM.Types.ClientArn
import Network.AWS.CloudHSM.Types.ClientLabel
import Network.AWS.CloudHSM.Types.ClientToken
import Network.AWS.CloudHSM.Types.ClientVersion
import Network.AWS.CloudHSM.Types.CloudHsmObjectState
import Network.AWS.CloudHSM.Types.EniId
import Network.AWS.CloudHSM.Types.ExternalId
import Network.AWS.CloudHSM.Types.HapgArn
import Network.AWS.CloudHSM.Types.HsmArn
import Network.AWS.CloudHSM.Types.HsmSerialNumber
import Network.AWS.CloudHSM.Types.HsmStatus
import Network.AWS.CloudHSM.Types.IamRoleArn
import Network.AWS.CloudHSM.Types.IpAddress
import Network.AWS.CloudHSM.Types.Key
import Network.AWS.CloudHSM.Types.Label
import Network.AWS.CloudHSM.Types.LastModifiedTimestamp
import Network.AWS.CloudHSM.Types.NextToken
import Network.AWS.CloudHSM.Types.PaginationToken
import Network.AWS.CloudHSM.Types.PartitionArn
import Network.AWS.CloudHSM.Types.PartitionSerial
import Network.AWS.CloudHSM.Types.ResourceArn
import Network.AWS.CloudHSM.Types.ServerCertLastUpdated
import Network.AWS.CloudHSM.Types.SshKey
import Network.AWS.CloudHSM.Types.SshKeyLastUpdated
import Network.AWS.CloudHSM.Types.Status
import Network.AWS.CloudHSM.Types.String
import Network.AWS.CloudHSM.Types.SubnetId
import Network.AWS.CloudHSM.Types.SubscriptionEndDate
import Network.AWS.CloudHSM.Types.SubscriptionStartDate
import Network.AWS.CloudHSM.Types.SubscriptionType
import Network.AWS.CloudHSM.Types.Tag
import Network.AWS.CloudHSM.Types.TagKey
import Network.AWS.CloudHSM.Types.Value
import Network.AWS.CloudHSM.Types.VpcId
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-05-30@ of the Amazon CloudHSM SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CloudHSM",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "cloudhsm",
      Core._svcVersion = "2014-05-30",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CloudHSM",
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

-- | Indicates that one or more of the request parameters are not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that an exception occurred in the AWS CloudHSM service.
_CloudHsmServiceException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmServiceException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmServiceException"
{-# DEPRECATED _CloudHsmServiceException "Use generic-lens or generic-optics instead." #-}

-- | Indicates that an internal error occurred.
_CloudHsmInternalException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CloudHsmInternalException =
  Core._MatchServiceError
    mkServiceConfig
    "CloudHsmInternalException"
{-# DEPRECATED _CloudHsmInternalException "Use generic-lens or generic-optics instead." #-}
