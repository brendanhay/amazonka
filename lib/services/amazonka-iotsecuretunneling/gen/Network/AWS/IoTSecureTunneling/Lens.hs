{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTSecureTunneling.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSecureTunneling.Lens
  ( -- * Operations

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CloseTunnel
    closeTunnel_delete,
    closeTunnel_tunnelId,
    closeTunnelResponse_httpStatus,

    -- ** OpenTunnel
    openTunnel_destinationConfig,
    openTunnel_description,
    openTunnel_timeoutConfig,
    openTunnel_tags,
    openTunnelResponse_sourceAccessToken,
    openTunnelResponse_tunnelArn,
    openTunnelResponse_destinationAccessToken,
    openTunnelResponse_tunnelId,
    openTunnelResponse_httpStatus,

    -- ** DescribeTunnel
    describeTunnel_tunnelId,
    describeTunnelResponse_tunnel,
    describeTunnelResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListTunnels
    listTunnels_nextToken,
    listTunnels_thingName,
    listTunnels_maxResults,
    listTunnelsResponse_nextToken,
    listTunnelsResponse_tunnelSummaries,
    listTunnelsResponse_httpStatus,

    -- * Types

    -- ** ConnectionState
    connectionState_status,
    connectionState_lastUpdatedAt,

    -- ** DestinationConfig
    destinationConfig_thingName,
    destinationConfig_services,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeoutConfig
    timeoutConfig_maxLifetimeTimeoutMinutes,

    -- ** Tunnel
    tunnel_status,
    tunnel_lastUpdatedAt,
    tunnel_createdAt,
    tunnel_tunnelArn,
    tunnel_sourceConnectionState,
    tunnel_destinationConnectionState,
    tunnel_tunnelId,
    tunnel_destinationConfig,
    tunnel_description,
    tunnel_timeoutConfig,
    tunnel_tags,

    -- ** TunnelSummary
    tunnelSummary_status,
    tunnelSummary_lastUpdatedAt,
    tunnelSummary_createdAt,
    tunnelSummary_tunnelArn,
    tunnelSummary_tunnelId,
    tunnelSummary_description,
  )
where

import Network.AWS.IoTSecureTunneling.CloseTunnel
import Network.AWS.IoTSecureTunneling.DescribeTunnel
import Network.AWS.IoTSecureTunneling.ListTagsForResource
import Network.AWS.IoTSecureTunneling.ListTunnels
import Network.AWS.IoTSecureTunneling.OpenTunnel
import Network.AWS.IoTSecureTunneling.TagResource
import Network.AWS.IoTSecureTunneling.Types.ConnectionState
import Network.AWS.IoTSecureTunneling.Types.DestinationConfig
import Network.AWS.IoTSecureTunneling.Types.Tag
import Network.AWS.IoTSecureTunneling.Types.TimeoutConfig
import Network.AWS.IoTSecureTunneling.Types.Tunnel
import Network.AWS.IoTSecureTunneling.Types.TunnelSummary
import Network.AWS.IoTSecureTunneling.UntagResource
