{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSecureTunneling.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Lens
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

import Amazonka.IoTSecureTunneling.CloseTunnel
import Amazonka.IoTSecureTunneling.DescribeTunnel
import Amazonka.IoTSecureTunneling.ListTagsForResource
import Amazonka.IoTSecureTunneling.ListTunnels
import Amazonka.IoTSecureTunneling.OpenTunnel
import Amazonka.IoTSecureTunneling.TagResource
import Amazonka.IoTSecureTunneling.Types.ConnectionState
import Amazonka.IoTSecureTunneling.Types.DestinationConfig
import Amazonka.IoTSecureTunneling.Types.Tag
import Amazonka.IoTSecureTunneling.Types.TimeoutConfig
import Amazonka.IoTSecureTunneling.Types.Tunnel
import Amazonka.IoTSecureTunneling.Types.TunnelSummary
import Amazonka.IoTSecureTunneling.UntagResource
