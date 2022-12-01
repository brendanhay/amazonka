{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RolesAnywhere.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _TooManyTagsException,
    _ResourceNotFoundException,
    _ValidationException,

    -- * TrustAnchorType
    TrustAnchorType (..),

    -- * CredentialSummary
    CredentialSummary (..),
    newCredentialSummary,
    credentialSummary_issuer,
    credentialSummary_failed,
    credentialSummary_seenAt,
    credentialSummary_enabled,
    credentialSummary_serialNumber,
    credentialSummary_x509CertificateData,

    -- * CrlDetail
    CrlDetail (..),
    newCrlDetail,
    crlDetail_crlArn,
    crlDetail_crlId,
    crlDetail_name,
    crlDetail_crlData,
    crlDetail_enabled,
    crlDetail_trustAnchorArn,
    crlDetail_createdAt,
    crlDetail_updatedAt,

    -- * CrlDetailResponse
    CrlDetailResponse (..),
    newCrlDetailResponse,
    crlDetailResponse_crl,

    -- * InstanceProperty
    InstanceProperty (..),
    newInstanceProperty,
    instanceProperty_failed,
    instanceProperty_seenAt,
    instanceProperty_properties,

    -- * ListRequest
    ListRequest (..),
    newListRequest,
    listRequest_nextToken,
    listRequest_pageSize,

    -- * ProfileDetail
    ProfileDetail (..),
    newProfileDetail,
    profileDetail_name,
    profileDetail_profileId,
    profileDetail_managedPolicyArns,
    profileDetail_profileArn,
    profileDetail_roleArns,
    profileDetail_enabled,
    profileDetail_durationSeconds,
    profileDetail_sessionPolicy,
    profileDetail_createdBy,
    profileDetail_requireInstanceProperties,
    profileDetail_createdAt,
    profileDetail_updatedAt,

    -- * ProfileDetailResponse
    ProfileDetailResponse (..),
    newProfileDetailResponse,
    profileDetailResponse_profile,

    -- * ScalarCrlRequest
    ScalarCrlRequest (..),
    newScalarCrlRequest,
    scalarCrlRequest_crlId,

    -- * ScalarProfileRequest
    ScalarProfileRequest (..),
    newScalarProfileRequest,
    scalarProfileRequest_profileId,

    -- * ScalarTrustAnchorRequest
    ScalarTrustAnchorRequest (..),
    newScalarTrustAnchorRequest,
    scalarTrustAnchorRequest_trustAnchorId,

    -- * Source
    Source (..),
    newSource,
    source_sourceType,
    source_sourceData,

    -- * SourceData
    SourceData (..),
    newSourceData,
    sourceData_acmPcaArn,
    sourceData_x509CertificateData,

    -- * SubjectDetail
    SubjectDetail (..),
    newSubjectDetail,
    subjectDetail_subjectId,
    subjectDetail_enabled,
    subjectDetail_subjectArn,
    subjectDetail_credentials,
    subjectDetail_x509Subject,
    subjectDetail_instanceProperties,
    subjectDetail_createdAt,
    subjectDetail_updatedAt,
    subjectDetail_lastSeenAt,

    -- * SubjectSummary
    SubjectSummary (..),
    newSubjectSummary,
    subjectSummary_subjectId,
    subjectSummary_enabled,
    subjectSummary_subjectArn,
    subjectSummary_x509Subject,
    subjectSummary_createdAt,
    subjectSummary_updatedAt,
    subjectSummary_lastSeenAt,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TrustAnchorDetail
    TrustAnchorDetail (..),
    newTrustAnchorDetail,
    trustAnchorDetail_name,
    trustAnchorDetail_trustAnchorId,
    trustAnchorDetail_enabled,
    trustAnchorDetail_source,
    trustAnchorDetail_trustAnchorArn,
    trustAnchorDetail_createdAt,
    trustAnchorDetail_updatedAt,

    -- * TrustAnchorDetailResponse
    TrustAnchorDetailResponse (..),
    newTrustAnchorDetailResponse,
    trustAnchorDetailResponse_trustAnchor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.CredentialSummary
import Amazonka.RolesAnywhere.Types.CrlDetail
import Amazonka.RolesAnywhere.Types.CrlDetailResponse
import Amazonka.RolesAnywhere.Types.InstanceProperty
import Amazonka.RolesAnywhere.Types.ListRequest
import Amazonka.RolesAnywhere.Types.ProfileDetail
import Amazonka.RolesAnywhere.Types.ProfileDetailResponse
import Amazonka.RolesAnywhere.Types.ScalarCrlRequest
import Amazonka.RolesAnywhere.Types.ScalarProfileRequest
import Amazonka.RolesAnywhere.Types.ScalarTrustAnchorRequest
import Amazonka.RolesAnywhere.Types.Source
import Amazonka.RolesAnywhere.Types.SourceData
import Amazonka.RolesAnywhere.Types.SubjectDetail
import Amazonka.RolesAnywhere.Types.SubjectSummary
import Amazonka.RolesAnywhere.Types.Tag
import Amazonka.RolesAnywhere.Types.TrustAnchorDetail
import Amazonka.RolesAnywhere.Types.TrustAnchorDetailResponse
import Amazonka.RolesAnywhere.Types.TrustAnchorType
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon IAM Roles Anywhere SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RolesAnywhere",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rolesanywhere",
      Core.signingName = "rolesanywhere",
      Core.version = "2018-05-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "RolesAnywhere",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Too many tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
    Prelude.. Core.hasStatus 400

-- | The resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Validation exception error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
