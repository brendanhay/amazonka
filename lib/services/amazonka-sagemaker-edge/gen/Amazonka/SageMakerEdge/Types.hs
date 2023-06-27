{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerEdge.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServiceException,

    -- * ChecksumType
    ChecksumType (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DeploymentType
    DeploymentType (..),

    -- * FailureHandlingPolicy
    FailureHandlingPolicy (..),

    -- * ModelState
    ModelState (..),

    -- * Checksum
    Checksum (..),
    newChecksum,
    checksum_sum,
    checksum_type,

    -- * Definition
    Definition (..),
    newDefinition,
    definition_checksum,
    definition_modelHandle,
    definition_s3Url,
    definition_state,

    -- * DeploymentModel
    DeploymentModel (..),
    newDeploymentModel,
    deploymentModel_desiredState,
    deploymentModel_modelHandle,
    deploymentModel_modelName,
    deploymentModel_modelVersion,
    deploymentModel_rollbackFailureReason,
    deploymentModel_state,
    deploymentModel_status,
    deploymentModel_statusReason,

    -- * DeploymentResult
    DeploymentResult (..),
    newDeploymentResult,
    deploymentResult_deploymentEndTime,
    deploymentResult_deploymentModels,
    deploymentResult_deploymentName,
    deploymentResult_deploymentStartTime,
    deploymentResult_deploymentStatus,
    deploymentResult_deploymentStatusMessage,

    -- * EdgeDeployment
    EdgeDeployment (..),
    newEdgeDeployment,
    edgeDeployment_definitions,
    edgeDeployment_deploymentName,
    edgeDeployment_failureHandlingPolicy,
    edgeDeployment_type,

    -- * EdgeMetric
    EdgeMetric (..),
    newEdgeMetric,
    edgeMetric_dimension,
    edgeMetric_metricName,
    edgeMetric_timestamp,
    edgeMetric_value,

    -- * Model
    Model (..),
    newModel,
    model_latestInference,
    model_latestSampleTime,
    model_modelMetrics,
    model_modelName,
    model_modelVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerEdge.Types.Checksum
import Amazonka.SageMakerEdge.Types.ChecksumType
import Amazonka.SageMakerEdge.Types.Definition
import Amazonka.SageMakerEdge.Types.DeploymentModel
import Amazonka.SageMakerEdge.Types.DeploymentResult
import Amazonka.SageMakerEdge.Types.DeploymentStatus
import Amazonka.SageMakerEdge.Types.DeploymentType
import Amazonka.SageMakerEdge.Types.EdgeDeployment
import Amazonka.SageMakerEdge.Types.EdgeMetric
import Amazonka.SageMakerEdge.Types.FailureHandlingPolicy
import Amazonka.SageMakerEdge.Types.Model
import Amazonka.SageMakerEdge.Types.ModelState
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-09-23@ of the Amazon Sagemaker Edge Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SageMakerEdge",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "edge.sagemaker",
      Core.signingName = "sagemaker",
      Core.version = "2020-09-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SageMakerEdge",
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

-- | An internal failure occurred. Try your request again. If the problem
-- persists, contact Amazon Web Services customer support.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
