{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IoTSecureTunneling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-10-05@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Secure Tunneling
--
-- AWS IoT Secure Tunnling enables you to create remote connections to
-- devices deployed in the field.
--
-- For more information about how AWS IoT Secure Tunneling works, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/secure-tunneling.html AWS IoT Secure Tunneling>.
module Amazonka.IoTSecureTunneling
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CloseTunnel
    CloseTunnel (CloseTunnel'),
    newCloseTunnel,
    CloseTunnelResponse (CloseTunnelResponse'),
    newCloseTunnelResponse,

    -- ** OpenTunnel
    OpenTunnel (OpenTunnel'),
    newOpenTunnel,
    OpenTunnelResponse (OpenTunnelResponse'),
    newOpenTunnelResponse,

    -- ** DescribeTunnel
    DescribeTunnel (DescribeTunnel'),
    newDescribeTunnel,
    DescribeTunnelResponse (DescribeTunnelResponse'),
    newDescribeTunnelResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListTunnels
    ListTunnels (ListTunnels'),
    newListTunnels,
    ListTunnelsResponse (ListTunnelsResponse'),
    newListTunnelsResponse,

    -- * Types

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** TunnelStatus
    TunnelStatus (..),

    -- ** ConnectionState
    ConnectionState (ConnectionState'),
    newConnectionState,

    -- ** DestinationConfig
    DestinationConfig (DestinationConfig'),
    newDestinationConfig,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimeoutConfig
    TimeoutConfig (TimeoutConfig'),
    newTimeoutConfig,

    -- ** Tunnel
    Tunnel (Tunnel'),
    newTunnel,

    -- ** TunnelSummary
    TunnelSummary (TunnelSummary'),
    newTunnelSummary,
  )
where

import Amazonka.IoTSecureTunneling.CloseTunnel
import Amazonka.IoTSecureTunneling.DescribeTunnel
import Amazonka.IoTSecureTunneling.Lens
import Amazonka.IoTSecureTunneling.ListTagsForResource
import Amazonka.IoTSecureTunneling.ListTunnels
import Amazonka.IoTSecureTunneling.OpenTunnel
import Amazonka.IoTSecureTunneling.TagResource
import Amazonka.IoTSecureTunneling.Types
import Amazonka.IoTSecureTunneling.UntagResource
import Amazonka.IoTSecureTunneling.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTSecureTunneling'.

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
