{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.SageMakerRuntime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-05-13@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- The Amazon SageMaker runtime API.
module Amazonka.SageMakerRuntime
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalDependencyException
    _InternalDependencyException,

    -- ** InternalFailure
    _InternalFailure,

    -- ** ModelError
    _ModelError,

    -- ** ModelNotReadyException
    _ModelNotReadyException,

    -- ** ServiceUnavailable
    _ServiceUnavailable,

    -- ** ValidationError
    _ValidationError,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** InvokeEndpoint
    InvokeEndpoint (InvokeEndpoint'),
    newInvokeEndpoint,
    InvokeEndpointResponse (InvokeEndpointResponse'),
    newInvokeEndpointResponse,

    -- ** InvokeEndpointAsync
    InvokeEndpointAsync (InvokeEndpointAsync'),
    newInvokeEndpointAsync,
    InvokeEndpointAsyncResponse (InvokeEndpointAsyncResponse'),
    newInvokeEndpointAsyncResponse,

    -- * Types
  )
where

import Amazonka.SageMakerRuntime.InvokeEndpoint
import Amazonka.SageMakerRuntime.InvokeEndpointAsync
import Amazonka.SageMakerRuntime.Lens
import Amazonka.SageMakerRuntime.Types
import Amazonka.SageMakerRuntime.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'SageMakerRuntime'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
