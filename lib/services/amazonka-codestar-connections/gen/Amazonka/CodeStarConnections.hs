{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeStarConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-12-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS CodeStar Connections
--
-- This AWS CodeStar Connections API Reference provides descriptions and
-- usage examples of the operations and data types for the AWS CodeStar
-- Connections API. You can use the connections API to work with
-- connections and installations.
--
-- /Connections/ are configurations that you use to connect AWS resources
-- to external code repositories. Each connection is a resource that can be
-- given to services such as CodePipeline to connect to a third-party
-- repository such as Bitbucket. For example, you can add the connection in
-- CodePipeline so that it triggers your pipeline when a code change is
-- made to your third-party code repository. Each connection is named and
-- associated with a unique ARN that is used to reference the connection.
--
-- When you create a connection, the console initiates a third-party
-- connection handshake. /Installations/ are the apps that are used to
-- conduct this handshake. For example, the installation for the Bitbucket
-- provider type is the Bitbucket app. When you create a connection, you
-- can choose an existing installation or create one.
--
-- When you want to create a connection to an installed provider type such
-- as GitHub Enterprise Server, you create a /host/ for your connections.
--
-- You can work with connections by calling:
--
-- -   CreateConnection, which creates a uniquely named connection that can
--     be referenced by services such as CodePipeline.
--
-- -   DeleteConnection, which deletes the specified connection.
--
-- -   GetConnection, which returns information about the connection,
--     including the connection status.
--
-- -   ListConnections, which lists the connections associated with your
--     account.
--
-- You can work with hosts by calling:
--
-- -   CreateHost, which creates a host that represents the infrastructure
--     where your provider is installed.
--
-- -   DeleteHost, which deletes the specified host.
--
-- -   GetHost, which returns information about the host, including the
--     setup status.
--
-- -   ListHosts, which lists the hosts associated with your account.
--
-- You can work with tags in AWS CodeStar Connections by calling the
-- following:
--
-- -   ListTagsForResource, which gets information about AWS tags for a
--     specified Amazon Resource Name (ARN) in AWS CodeStar Connections.
--
-- -   TagResource, which adds or updates tags for a resource in AWS
--     CodeStar Connections.
--
-- -   UntagResource, which removes tags for a resource in AWS CodeStar
--     Connections.
--
-- For information about how to use AWS CodeStar Connections, see the
-- <https://docs.aws.amazon.com/dtconsole/latest/userguide/welcome-connections.html Developer Tools User Guide>.
module Amazonka.CodeStarConnections
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictException
    _ConflictException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** UnsupportedOperationException
    _UnsupportedOperationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateConnection
    CreateConnection (CreateConnection'),
    newCreateConnection,
    CreateConnectionResponse (CreateConnectionResponse'),
    newCreateConnectionResponse,

    -- ** CreateHost
    CreateHost (CreateHost'),
    newCreateHost,
    CreateHostResponse (CreateHostResponse'),
    newCreateHostResponse,

    -- ** DeleteConnection
    DeleteConnection (DeleteConnection'),
    newDeleteConnection,
    DeleteConnectionResponse (DeleteConnectionResponse'),
    newDeleteConnectionResponse,

    -- ** DeleteHost
    DeleteHost (DeleteHost'),
    newDeleteHost,
    DeleteHostResponse (DeleteHostResponse'),
    newDeleteHostResponse,

    -- ** GetConnection
    GetConnection (GetConnection'),
    newGetConnection,
    GetConnectionResponse (GetConnectionResponse'),
    newGetConnectionResponse,

    -- ** GetHost
    GetHost (GetHost'),
    newGetHost,
    GetHostResponse (GetHostResponse'),
    newGetHostResponse,

    -- ** ListConnections
    ListConnections (ListConnections'),
    newListConnections,
    ListConnectionsResponse (ListConnectionsResponse'),
    newListConnectionsResponse,

    -- ** ListHosts
    ListHosts (ListHosts'),
    newListHosts,
    ListHostsResponse (ListHostsResponse'),
    newListHostsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

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

    -- ** UpdateHost
    UpdateHost (UpdateHost'),
    newUpdateHost,
    UpdateHostResponse (UpdateHostResponse'),
    newUpdateHostResponse,

    -- * Types

    -- ** ConnectionStatus
    ConnectionStatus (..),

    -- ** ProviderType
    ProviderType (..),

    -- ** Connection
    Connection (Connection'),
    newConnection,

    -- ** Host
    Host (Host'),
    newHost,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** VpcConfiguration
    VpcConfiguration (VpcConfiguration'),
    newVpcConfiguration,
  )
where

import Amazonka.CodeStarConnections.CreateConnection
import Amazonka.CodeStarConnections.CreateHost
import Amazonka.CodeStarConnections.DeleteConnection
import Amazonka.CodeStarConnections.DeleteHost
import Amazonka.CodeStarConnections.GetConnection
import Amazonka.CodeStarConnections.GetHost
import Amazonka.CodeStarConnections.Lens
import Amazonka.CodeStarConnections.ListConnections
import Amazonka.CodeStarConnections.ListHosts
import Amazonka.CodeStarConnections.ListTagsForResource
import Amazonka.CodeStarConnections.TagResource
import Amazonka.CodeStarConnections.Types
import Amazonka.CodeStarConnections.UntagResource
import Amazonka.CodeStarConnections.UpdateHost
import Amazonka.CodeStarConnections.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeStarConnections'.

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
