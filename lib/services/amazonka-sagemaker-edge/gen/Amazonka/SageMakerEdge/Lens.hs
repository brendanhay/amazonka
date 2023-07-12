{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerEdge.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Lens
  ( -- * Operations

    -- ** GetDeployments
    getDeployments_deviceName,
    getDeployments_deviceFleetName,
    getDeploymentsResponse_deployments,
    getDeploymentsResponse_httpStatus,

    -- ** GetDeviceRegistration
    getDeviceRegistration_deviceName,
    getDeviceRegistration_deviceFleetName,
    getDeviceRegistrationResponse_cacheTTL,
    getDeviceRegistrationResponse_deviceRegistration,
    getDeviceRegistrationResponse_httpStatus,

    -- ** SendHeartbeat
    sendHeartbeat_agentMetrics,
    sendHeartbeat_deploymentResult,
    sendHeartbeat_models,
    sendHeartbeat_agentVersion,
    sendHeartbeat_deviceName,
    sendHeartbeat_deviceFleetName,

    -- * Types

    -- ** Checksum
    checksum_sum,
    checksum_type,

    -- ** Definition
    definition_checksum,
    definition_modelHandle,
    definition_s3Url,
    definition_state,

    -- ** DeploymentModel
    deploymentModel_desiredState,
    deploymentModel_modelHandle,
    deploymentModel_modelName,
    deploymentModel_modelVersion,
    deploymentModel_rollbackFailureReason,
    deploymentModel_state,
    deploymentModel_status,
    deploymentModel_statusReason,

    -- ** DeploymentResult
    deploymentResult_deploymentEndTime,
    deploymentResult_deploymentModels,
    deploymentResult_deploymentName,
    deploymentResult_deploymentStartTime,
    deploymentResult_deploymentStatus,
    deploymentResult_deploymentStatusMessage,

    -- ** EdgeDeployment
    edgeDeployment_definitions,
    edgeDeployment_deploymentName,
    edgeDeployment_failureHandlingPolicy,
    edgeDeployment_type,

    -- ** EdgeMetric
    edgeMetric_dimension,
    edgeMetric_metricName,
    edgeMetric_timestamp,
    edgeMetric_value,

    -- ** Model
    model_latestInference,
    model_latestSampleTime,
    model_modelMetrics,
    model_modelName,
    model_modelVersion,
  )
where

import Amazonka.SageMakerEdge.GetDeployments
import Amazonka.SageMakerEdge.GetDeviceRegistration
import Amazonka.SageMakerEdge.SendHeartbeat
import Amazonka.SageMakerEdge.Types.Checksum
import Amazonka.SageMakerEdge.Types.Definition
import Amazonka.SageMakerEdge.Types.DeploymentModel
import Amazonka.SageMakerEdge.Types.DeploymentResult
import Amazonka.SageMakerEdge.Types.EdgeDeployment
import Amazonka.SageMakerEdge.Types.EdgeMetric
import Amazonka.SageMakerEdge.Types.Model
