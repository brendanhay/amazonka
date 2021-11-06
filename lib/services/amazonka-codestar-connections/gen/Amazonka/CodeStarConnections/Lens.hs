{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStarConnections.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarConnections.Lens
  ( -- * Operations

    -- ** CreateHost
    createHost_vpcConfiguration,
    createHost_tags,
    createHost_name,
    createHost_providerType,
    createHost_providerEndpoint,
    createHostResponse_hostArn,
    createHostResponse_tags,
    createHostResponse_httpStatus,

    -- ** ListConnections
    listConnections_hostArnFilter,
    listConnections_nextToken,
    listConnections_maxResults,
    listConnections_providerTypeFilter,
    listConnectionsResponse_connections,
    listConnectionsResponse_nextToken,
    listConnectionsResponse_httpStatus,

    -- ** DeleteConnection
    deleteConnection_connectionArn,
    deleteConnectionResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateConnection
    createConnection_providerType,
    createConnection_hostArn,
    createConnection_tags,
    createConnection_connectionName,
    createConnectionResponse_tags,
    createConnectionResponse_httpStatus,
    createConnectionResponse_connectionArn,

    -- ** GetConnection
    getConnection_connectionArn,
    getConnectionResponse_connection,
    getConnectionResponse_httpStatus,

    -- ** DeleteHost
    deleteHost_hostArn,
    deleteHostResponse_httpStatus,

    -- ** UpdateHost
    updateHost_providerEndpoint,
    updateHost_vpcConfiguration,
    updateHost_hostArn,
    updateHostResponse_httpStatus,

    -- ** ListHosts
    listHosts_nextToken,
    listHosts_maxResults,
    listHostsResponse_hosts,
    listHostsResponse_nextToken,
    listHostsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetHost
    getHost_hostArn,
    getHostResponse_status,
    getHostResponse_providerEndpoint,
    getHostResponse_providerType,
    getHostResponse_name,
    getHostResponse_vpcConfiguration,
    getHostResponse_httpStatus,

    -- * Types

    -- ** Connection
    connection_ownerAccountId,
    connection_providerType,
    connection_connectionName,
    connection_connectionStatus,
    connection_hostArn,
    connection_connectionArn,

    -- ** Host
    host_status,
    host_providerEndpoint,
    host_providerType,
    host_statusMessage,
    host_name,
    host_hostArn,
    host_vpcConfiguration,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** VpcConfiguration
    vpcConfiguration_tlsCertificate,
    vpcConfiguration_vpcId,
    vpcConfiguration_subnetIds,
    vpcConfiguration_securityGroupIds,
  )
where

import Amazonka.CodeStarConnections.CreateConnection
import Amazonka.CodeStarConnections.CreateHost
import Amazonka.CodeStarConnections.DeleteConnection
import Amazonka.CodeStarConnections.DeleteHost
import Amazonka.CodeStarConnections.GetConnection
import Amazonka.CodeStarConnections.GetHost
import Amazonka.CodeStarConnections.ListConnections
import Amazonka.CodeStarConnections.ListHosts
import Amazonka.CodeStarConnections.ListTagsForResource
import Amazonka.CodeStarConnections.TagResource
import Amazonka.CodeStarConnections.Types.Connection
import Amazonka.CodeStarConnections.Types.Host
import Amazonka.CodeStarConnections.Types.Tag
import Amazonka.CodeStarConnections.Types.VpcConfiguration
import Amazonka.CodeStarConnections.UntagResource
import Amazonka.CodeStarConnections.UpdateHost
