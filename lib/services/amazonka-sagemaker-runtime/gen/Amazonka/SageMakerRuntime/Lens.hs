{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerRuntime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerRuntime.Lens
  ( -- * Operations

    -- ** InvokeEndpoint
    invokeEndpoint_targetContainerHostname,
    invokeEndpoint_accept,
    invokeEndpoint_targetVariant,
    invokeEndpoint_customAttributes,
    invokeEndpoint_targetModel,
    invokeEndpoint_inferenceId,
    invokeEndpoint_contentType,
    invokeEndpoint_endpointName,
    invokeEndpoint_body,
    invokeEndpointResponse_customAttributes,
    invokeEndpointResponse_invokedProductionVariant,
    invokeEndpointResponse_contentType,
    invokeEndpointResponse_httpStatus,
    invokeEndpointResponse_body,

    -- ** InvokeEndpointAsync
    invokeEndpointAsync_accept,
    invokeEndpointAsync_customAttributes,
    invokeEndpointAsync_requestTTLSeconds,
    invokeEndpointAsync_inferenceId,
    invokeEndpointAsync_contentType,
    invokeEndpointAsync_endpointName,
    invokeEndpointAsync_inputLocation,
    invokeEndpointAsyncResponse_outputLocation,
    invokeEndpointAsyncResponse_inferenceId,
    invokeEndpointAsyncResponse_httpStatus,

    -- * Types
  )
where

import Amazonka.SageMakerRuntime.InvokeEndpoint
import Amazonka.SageMakerRuntime.InvokeEndpointAsync
