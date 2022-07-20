{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerEdge.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerEdge.Lens
  ( -- * Operations

    -- ** GetDeviceRegistration
    getDeviceRegistration_deviceName,
    getDeviceRegistration_deviceFleetName,
    getDeviceRegistrationResponse_deviceRegistration,
    getDeviceRegistrationResponse_cacheTTL,
    getDeviceRegistrationResponse_httpStatus,

    -- ** SendHeartbeat
    sendHeartbeat_models,
    sendHeartbeat_agentMetrics,
    sendHeartbeat_agentVersion,
    sendHeartbeat_deviceName,
    sendHeartbeat_deviceFleetName,

    -- * Types

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

import Amazonka.SageMakerEdge.GetDeviceRegistration
import Amazonka.SageMakerEdge.SendHeartbeat
import Amazonka.SageMakerEdge.Types.EdgeMetric
import Amazonka.SageMakerEdge.Types.Model
