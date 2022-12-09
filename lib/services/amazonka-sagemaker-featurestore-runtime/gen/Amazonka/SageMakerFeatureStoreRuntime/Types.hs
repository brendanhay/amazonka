{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerFeatureStoreRuntime.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerFeatureStoreRuntime.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessForbidden,
    _InternalFailure,
    _ResourceNotFound,
    _ServiceUnavailable,
    _ValidationError,

    -- * TargetStore
    TargetStore (..),

    -- * BatchGetRecordError
    BatchGetRecordError (..),
    newBatchGetRecordError,
    batchGetRecordError_featureGroupName,
    batchGetRecordError_recordIdentifierValueAsString,
    batchGetRecordError_errorCode,
    batchGetRecordError_errorMessage,

    -- * BatchGetRecordIdentifier
    BatchGetRecordIdentifier (..),
    newBatchGetRecordIdentifier,
    batchGetRecordIdentifier_featureNames,
    batchGetRecordIdentifier_featureGroupName,
    batchGetRecordIdentifier_recordIdentifiersValueAsString,

    -- * BatchGetRecordResultDetail
    BatchGetRecordResultDetail (..),
    newBatchGetRecordResultDetail,
    batchGetRecordResultDetail_featureGroupName,
    batchGetRecordResultDetail_recordIdentifierValueAsString,
    batchGetRecordResultDetail_record,

    -- * FeatureValue
    FeatureValue (..),
    newFeatureValue,
    featureValue_featureName,
    featureValue_valueAsString,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordError
import Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordIdentifier
import Amazonka.SageMakerFeatureStoreRuntime.Types.BatchGetRecordResultDetail
import Amazonka.SageMakerFeatureStoreRuntime.Types.FeatureValue
import Amazonka.SageMakerFeatureStoreRuntime.Types.TargetStore
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-01@ of the Amazon SageMaker Feature Store Runtime SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "SageMakerFeatureStoreRuntime",
      Core.signer = Sign.v4,
      Core.endpointPrefix =
        "featurestore-runtime.sagemaker",
      Core.signingName = "sagemaker",
      Core.version = "2020-07-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "SageMakerFeatureStoreRuntime",
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

-- | You do not have permission to perform an action.
_AccessForbidden :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessForbidden =
  Core._MatchServiceError
    defaultService
    "AccessForbidden"
    Prelude.. Core.hasStatus 403

-- | An internal failure occurred. Try your request again. If the problem
-- persists, contact Amazon Web Services customer support.
_InternalFailure :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailure =
  Core._MatchServiceError
    defaultService
    "InternalFailure"
    Prelude.. Core.hasStatus 500

-- | A resource that is required to perform an action was not found.
_ResourceNotFound :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFound =
  Core._MatchServiceError
    defaultService
    "ResourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The service is currently unavailable.
_ServiceUnavailable :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailable =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailable"
    Prelude.. Core.hasStatus 503

-- | There was an error validating your request.
_ValidationError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationError =
  Core._MatchServiceError
    defaultService
    "ValidationError"
    Prelude.. Core.hasStatus 400
