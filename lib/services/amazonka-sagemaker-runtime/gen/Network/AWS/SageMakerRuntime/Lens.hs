{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMakerRuntime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMakerRuntime.Lens
  ( -- * Operations

    -- ** InvokeEndpointAsync
    invokeEndpointAsync_accept,
    invokeEndpointAsync_customAttributes,
    invokeEndpointAsync_inferenceId,
    invokeEndpointAsync_requestTTLSeconds,
    invokeEndpointAsync_contentType,
    invokeEndpointAsync_endpointName,
    invokeEndpointAsync_inputLocation,
    invokeEndpointAsyncResponse_outputLocation,
    invokeEndpointAsyncResponse_inferenceId,
    invokeEndpointAsyncResponse_httpStatus,

    -- ** InvokeEndpoint
    invokeEndpoint_accept,
    invokeEndpoint_targetModel,
    invokeEndpoint_customAttributes,
    invokeEndpoint_inferenceId,
    invokeEndpoint_targetVariant,
    invokeEndpoint_contentType,
    invokeEndpoint_targetContainerHostname,
    invokeEndpoint_endpointName,
    invokeEndpoint_body,
    invokeEndpointResponse_invokedProductionVariant,
    invokeEndpointResponse_customAttributes,
    invokeEndpointResponse_contentType,
    invokeEndpointResponse_httpStatus,
    invokeEndpointResponse_body,

    -- * Types
  )
where

import Network.AWS.SageMakerRuntime.InvokeEndpoint
import Network.AWS.SageMakerRuntime.InvokeEndpointAsync
