{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerRuntime.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerRuntime.Lens
  ( -- * Operations

    -- ** InvokeEndpoint
    invokeEndpoint_accept,
    invokeEndpoint_contentType,
    invokeEndpoint_customAttributes,
    invokeEndpoint_enableExplanations,
    invokeEndpoint_inferenceId,
    invokeEndpoint_targetContainerHostname,
    invokeEndpoint_targetModel,
    invokeEndpoint_targetVariant,
    invokeEndpoint_endpointName,
    invokeEndpoint_body,
    invokeEndpointResponse_contentType,
    invokeEndpointResponse_customAttributes,
    invokeEndpointResponse_invokedProductionVariant,
    invokeEndpointResponse_httpStatus,
    invokeEndpointResponse_body,

    -- ** InvokeEndpointAsync
    invokeEndpointAsync_accept,
    invokeEndpointAsync_contentType,
    invokeEndpointAsync_customAttributes,
    invokeEndpointAsync_inferenceId,
    invokeEndpointAsync_invocationTimeoutSeconds,
    invokeEndpointAsync_requestTTLSeconds,
    invokeEndpointAsync_endpointName,
    invokeEndpointAsync_inputLocation,
    invokeEndpointAsyncResponse_failureLocation,
    invokeEndpointAsyncResponse_inferenceId,
    invokeEndpointAsyncResponse_outputLocation,
    invokeEndpointAsyncResponse_httpStatus,

    -- * Types
  )
where

import Amazonka.SageMakerRuntime.InvokeEndpoint
import Amazonka.SageMakerRuntime.InvokeEndpointAsync
