{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerEdge.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getDeviceRegistrationResponse_deviceRegistration,
    getDeviceRegistrationResponse_cacheTTL,
    getDeviceRegistrationResponse_httpStatus,

    -- ** SendHeartbeat
    sendHeartbeat_models,
    sendHeartbeat_deploymentResult,
    sendHeartbeat_agentMetrics,
    sendHeartbeat_agentVersion,
    sendHeartbeat_deviceName,
    sendHeartbeat_deviceFleetName,

    -- * Types

    -- ** Checksum
    checksum_type,
    checksum_sum,

    -- ** Definition
    definition_state,
    definition_checksum,
    definition_s3Url,
    definition_modelHandle,

    -- ** DeploymentModel
    deploymentModel_rollbackFailureReason,
    deploymentModel_statusReason,
    deploymentModel_state,
    deploymentModel_modelVersion,
    deploymentModel_status,
    deploymentModel_desiredState,
    deploymentModel_modelName,
    deploymentModel_modelHandle,

    -- ** DeploymentResult
    deploymentResult_deploymentStatus,
    deploymentResult_deploymentName,
    deploymentResult_deploymentModels,
    deploymentResult_deploymentEndTime,
    deploymentResult_deploymentStatusMessage,
    deploymentResult_deploymentStartTime,

    -- ** EdgeDeployment
    edgeDeployment_type,
    edgeDeployment_deploymentName,
    edgeDeployment_failureHandlingPolicy,
    edgeDeployment_definitions,

    -- ** EdgeMetric
    edgeMetric_timestamp,
    edgeMetric_metricName,
    edgeMetric_dimension,
    edgeMetric_value,

    -- ** Model
    model_latestSampleTime,
    model_modelVersion,
    model_modelMetrics,
    model_modelName,
    model_latestInference,
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
