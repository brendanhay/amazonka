{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerEdge.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMakerEdge.Lens
  ( -- * Operations

    -- ** SendHeartbeat
    sendHeartbeat_agentMetrics,
    sendHeartbeat_models,
    sendHeartbeat_agentVersion,
    sendHeartbeat_deviceName,
    sendHeartbeat_deviceFleetName,

    -- ** GetDeviceRegistration
    getDeviceRegistration_deviceName,
    getDeviceRegistration_deviceFleetName,
    getDeviceRegistrationResponse_cacheTTL,
    getDeviceRegistrationResponse_deviceRegistration,
    getDeviceRegistrationResponse_httpStatus,

    -- * Types

    -- ** EdgeMetric
    edgeMetric_dimension,
    edgeMetric_metricName,
    edgeMetric_value,
    edgeMetric_timestamp,

    -- ** Model
    model_modelName,
    model_modelMetrics,
    model_modelVersion,
    model_latestInference,
    model_latestSampleTime,
  )
where

import Network.AWS.SageMakerEdge.GetDeviceRegistration
import Network.AWS.SageMakerEdge.SendHeartbeat
import Network.AWS.SageMakerEdge.Types.EdgeMetric
import Network.AWS.SageMakerEdge.Types.Model
