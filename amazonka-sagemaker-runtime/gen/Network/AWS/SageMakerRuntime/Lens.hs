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

    -- ** InvokeEndpoint
    invokeEndpoint_targetContainerHostname,
    invokeEndpoint_contentType,
    invokeEndpoint_targetModel,
    invokeEndpoint_accept,
    invokeEndpoint_customAttributes,
    invokeEndpoint_inferenceId,
    invokeEndpoint_targetVariant,
    invokeEndpoint_endpointName,
    invokeEndpoint_body,
    invokeEndpointResponse_contentType,
    invokeEndpointResponse_invokedProductionVariant,
    invokeEndpointResponse_customAttributes,
    invokeEndpointResponse_httpStatus,
    invokeEndpointResponse_body,

    -- * Types
  )
where

import Network.AWS.SageMakerRuntime.InvokeEndpoint
