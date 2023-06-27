{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpenSearchServerless.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearchServerless.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalServerException,
    _OcuLimitExceededException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ValidationException,

    -- * AccessPolicyType
    AccessPolicyType (..),

    -- * CollectionStatus
    CollectionStatus (..),

    -- * CollectionType
    CollectionType (..),

    -- * SecurityConfigType
    SecurityConfigType (..),

    -- * SecurityPolicyType
    SecurityPolicyType (..),

    -- * VpcEndpointStatus
    VpcEndpointStatus (..),

    -- * AccessPolicyDetail
    AccessPolicyDetail (..),
    newAccessPolicyDetail,
    accessPolicyDetail_createdDate,
    accessPolicyDetail_description,
    accessPolicyDetail_lastModifiedDate,
    accessPolicyDetail_name,
    accessPolicyDetail_policy,
    accessPolicyDetail_policyVersion,
    accessPolicyDetail_type,

    -- * AccessPolicyStats
    AccessPolicyStats (..),
    newAccessPolicyStats,
    accessPolicyStats_dataPolicyCount,

    -- * AccessPolicySummary
    AccessPolicySummary (..),
    newAccessPolicySummary,
    accessPolicySummary_createdDate,
    accessPolicySummary_description,
    accessPolicySummary_lastModifiedDate,
    accessPolicySummary_name,
    accessPolicySummary_policyVersion,
    accessPolicySummary_type,

    -- * AccountSettingsDetail
    AccountSettingsDetail (..),
    newAccountSettingsDetail,
    accountSettingsDetail_capacityLimits,

    -- * CapacityLimits
    CapacityLimits (..),
    newCapacityLimits,
    capacityLimits_maxIndexingCapacityInOCU,
    capacityLimits_maxSearchCapacityInOCU,

    -- * CollectionDetail
    CollectionDetail (..),
    newCollectionDetail,
    collectionDetail_arn,
    collectionDetail_collectionEndpoint,
    collectionDetail_createdDate,
    collectionDetail_dashboardEndpoint,
    collectionDetail_description,
    collectionDetail_id,
    collectionDetail_kmsKeyArn,
    collectionDetail_lastModifiedDate,
    collectionDetail_name,
    collectionDetail_status,
    collectionDetail_type,

    -- * CollectionErrorDetail
    CollectionErrorDetail (..),
    newCollectionErrorDetail,
    collectionErrorDetail_errorCode,
    collectionErrorDetail_errorMessage,
    collectionErrorDetail_id,
    collectionErrorDetail_name,

    -- * CollectionFilters
    CollectionFilters (..),
    newCollectionFilters,
    collectionFilters_name,
    collectionFilters_status,

    -- * CollectionSummary
    CollectionSummary (..),
    newCollectionSummary,
    collectionSummary_arn,
    collectionSummary_id,
    collectionSummary_name,
    collectionSummary_status,

    -- * CreateCollectionDetail
    CreateCollectionDetail (..),
    newCreateCollectionDetail,
    createCollectionDetail_arn,
    createCollectionDetail_createdDate,
    createCollectionDetail_description,
    createCollectionDetail_id,
    createCollectionDetail_kmsKeyArn,
    createCollectionDetail_lastModifiedDate,
    createCollectionDetail_name,
    createCollectionDetail_status,
    createCollectionDetail_type,

    -- * CreateVpcEndpointDetail
    CreateVpcEndpointDetail (..),
    newCreateVpcEndpointDetail,
    createVpcEndpointDetail_id,
    createVpcEndpointDetail_name,
    createVpcEndpointDetail_status,

    -- * DeleteCollectionDetail
    DeleteCollectionDetail (..),
    newDeleteCollectionDetail,
    deleteCollectionDetail_id,
    deleteCollectionDetail_name,
    deleteCollectionDetail_status,

    -- * DeleteVpcEndpointDetail
    DeleteVpcEndpointDetail (..),
    newDeleteVpcEndpointDetail,
    deleteVpcEndpointDetail_id,
    deleteVpcEndpointDetail_name,
    deleteVpcEndpointDetail_status,

    -- * Document
    Document (..),
    newDocument,

    -- * SamlConfigOptions
    SamlConfigOptions (..),
    newSamlConfigOptions,
    samlConfigOptions_groupAttribute,
    samlConfigOptions_sessionTimeout,
    samlConfigOptions_userAttribute,
    samlConfigOptions_metadata,

    -- * SecurityConfigDetail
    SecurityConfigDetail (..),
    newSecurityConfigDetail,
    securityConfigDetail_configVersion,
    securityConfigDetail_createdDate,
    securityConfigDetail_description,
    securityConfigDetail_id,
    securityConfigDetail_lastModifiedDate,
    securityConfigDetail_samlOptions,
    securityConfigDetail_type,

    -- * SecurityConfigStats
    SecurityConfigStats (..),
    newSecurityConfigStats,
    securityConfigStats_samlConfigCount,

    -- * SecurityConfigSummary
    SecurityConfigSummary (..),
    newSecurityConfigSummary,
    securityConfigSummary_configVersion,
    securityConfigSummary_createdDate,
    securityConfigSummary_description,
    securityConfigSummary_id,
    securityConfigSummary_lastModifiedDate,
    securityConfigSummary_type,

    -- * SecurityPolicyDetail
    SecurityPolicyDetail (..),
    newSecurityPolicyDetail,
    securityPolicyDetail_createdDate,
    securityPolicyDetail_description,
    securityPolicyDetail_lastModifiedDate,
    securityPolicyDetail_name,
    securityPolicyDetail_policy,
    securityPolicyDetail_policyVersion,
    securityPolicyDetail_type,

    -- * SecurityPolicyStats
    SecurityPolicyStats (..),
    newSecurityPolicyStats,
    securityPolicyStats_encryptionPolicyCount,
    securityPolicyStats_networkPolicyCount,

    -- * SecurityPolicySummary
    SecurityPolicySummary (..),
    newSecurityPolicySummary,
    securityPolicySummary_createdDate,
    securityPolicySummary_description,
    securityPolicySummary_lastModifiedDate,
    securityPolicySummary_name,
    securityPolicySummary_policyVersion,
    securityPolicySummary_type,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpdateCollectionDetail
    UpdateCollectionDetail (..),
    newUpdateCollectionDetail,
    updateCollectionDetail_arn,
    updateCollectionDetail_createdDate,
    updateCollectionDetail_description,
    updateCollectionDetail_id,
    updateCollectionDetail_lastModifiedDate,
    updateCollectionDetail_name,
    updateCollectionDetail_status,
    updateCollectionDetail_type,

    -- * UpdateVpcEndpointDetail
    UpdateVpcEndpointDetail (..),
    newUpdateVpcEndpointDetail,
    updateVpcEndpointDetail_id,
    updateVpcEndpointDetail_lastModifiedDate,
    updateVpcEndpointDetail_name,
    updateVpcEndpointDetail_securityGroupIds,
    updateVpcEndpointDetail_status,
    updateVpcEndpointDetail_subnetIds,

    -- * VpcEndpointDetail
    VpcEndpointDetail (..),
    newVpcEndpointDetail,
    vpcEndpointDetail_createdDate,
    vpcEndpointDetail_id,
    vpcEndpointDetail_name,
    vpcEndpointDetail_securityGroupIds,
    vpcEndpointDetail_status,
    vpcEndpointDetail_subnetIds,
    vpcEndpointDetail_vpcId,

    -- * VpcEndpointErrorDetail
    VpcEndpointErrorDetail (..),
    newVpcEndpointErrorDetail,
    vpcEndpointErrorDetail_errorCode,
    vpcEndpointErrorDetail_errorMessage,
    vpcEndpointErrorDetail_id,

    -- * VpcEndpointFilters
    VpcEndpointFilters (..),
    newVpcEndpointFilters,
    vpcEndpointFilters_status,

    -- * VpcEndpointSummary
    VpcEndpointSummary (..),
    newVpcEndpointSummary,
    vpcEndpointSummary_id,
    vpcEndpointSummary_name,
    vpcEndpointSummary_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearchServerless.Types.AccessPolicyDetail
import Amazonka.OpenSearchServerless.Types.AccessPolicyStats
import Amazonka.OpenSearchServerless.Types.AccessPolicySummary
import Amazonka.OpenSearchServerless.Types.AccessPolicyType
import Amazonka.OpenSearchServerless.Types.AccountSettingsDetail
import Amazonka.OpenSearchServerless.Types.CapacityLimits
import Amazonka.OpenSearchServerless.Types.CollectionDetail
import Amazonka.OpenSearchServerless.Types.CollectionErrorDetail
import Amazonka.OpenSearchServerless.Types.CollectionFilters
import Amazonka.OpenSearchServerless.Types.CollectionStatus
import Amazonka.OpenSearchServerless.Types.CollectionSummary
import Amazonka.OpenSearchServerless.Types.CollectionType
import Amazonka.OpenSearchServerless.Types.CreateCollectionDetail
import Amazonka.OpenSearchServerless.Types.CreateVpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.DeleteCollectionDetail
import Amazonka.OpenSearchServerless.Types.DeleteVpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.Document
import Amazonka.OpenSearchServerless.Types.SamlConfigOptions
import Amazonka.OpenSearchServerless.Types.SecurityConfigDetail
import Amazonka.OpenSearchServerless.Types.SecurityConfigStats
import Amazonka.OpenSearchServerless.Types.SecurityConfigSummary
import Amazonka.OpenSearchServerless.Types.SecurityConfigType
import Amazonka.OpenSearchServerless.Types.SecurityPolicyDetail
import Amazonka.OpenSearchServerless.Types.SecurityPolicyStats
import Amazonka.OpenSearchServerless.Types.SecurityPolicySummary
import Amazonka.OpenSearchServerless.Types.SecurityPolicyType
import Amazonka.OpenSearchServerless.Types.Tag
import Amazonka.OpenSearchServerless.Types.UpdateCollectionDetail
import Amazonka.OpenSearchServerless.Types.UpdateVpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.VpcEndpointDetail
import Amazonka.OpenSearchServerless.Types.VpcEndpointErrorDetail
import Amazonka.OpenSearchServerless.Types.VpcEndpointFilters
import Amazonka.OpenSearchServerless.Types.VpcEndpointStatus
import Amazonka.OpenSearchServerless.Types.VpcEndpointSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-11-01@ of the Amazon OpenSearch Service Serverless SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "OpenSearchServerless",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "aoss",
      Core.signingName = "aoss",
      Core.version = "2021-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "OpenSearchServerless",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | When creating a resource, thrown when a resource with the same name
-- already exists or is being created. When deleting a resource, thrown
-- when the resource is not in the ACTIVE or FAILED state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Thrown when an error internal to the service occurs while processing a
-- request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | OCU Limit Exceeded for service limits
_OcuLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OcuLimitExceededException =
  Core._MatchServiceError
    defaultService
    "OcuLimitExceededException"

-- | Thrown when accessing or deleting a resource that does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Thrown when you attempt to create more resources than the service allows
-- based on service quotas.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Thrown when the HTTP request contains invalid input or is missing
-- required input.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
