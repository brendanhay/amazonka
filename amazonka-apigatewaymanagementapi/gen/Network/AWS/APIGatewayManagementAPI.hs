{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The Amazon API Gateway Management API allows you to directly manage
-- runtime aspects of your deployed APIs. To use it, you must explicitly
-- set the SDK\'s endpoint to point to the endpoint of your deployed API.
-- The endpoint will be of the form
-- https:\/\/{api-id}.execute-api.{region}.amazonaws.com\/{stage}, or will
-- be the endpoint corresponding to your API\'s custom domain and base
-- path, if applicable.
module Network.AWS.APIGatewayManagementAPI
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** GoneException
    _GoneException,

    -- ** PayloadTooLargeException
    _PayloadTooLargeException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** PostToConnection
    PostToConnection (PostToConnection'),
    newPostToConnection,
    PostToConnectionResponse (PostToConnectionResponse'),
    newPostToConnectionResponse,

    -- ** GetConnection
    GetConnection (GetConnection'),
    newGetConnection,
    GetConnectionResponse (GetConnectionResponse'),
    newGetConnectionResponse,

    -- * Types

    -- ** Identity
    Identity (Identity'),
    newIdentity,
  )
where

import Network.AWS.APIGatewayManagementAPI.DeleteConnection
import Network.AWS.APIGatewayManagementAPI.GetConnection
import Network.AWS.APIGatewayManagementAPI.Lens
import Network.AWS.APIGatewayManagementAPI.PostToConnection
import Network.AWS.APIGatewayManagementAPI.Types
import Network.AWS.APIGatewayManagementAPI.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'APIGatewayManagementAPI'.

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
