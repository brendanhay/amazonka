{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTSecureTunneling.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSecureTunneling.Lens
  ( -- * Operations

    -- ** CloseTunnel
    closeTunnel_delete,
    closeTunnel_tunnelId,
    closeTunnelResponse_httpStatus,

    -- ** DescribeTunnel
    describeTunnel_tunnelId,
    describeTunnelResponse_tunnel,
    describeTunnelResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTunnels
    listTunnels_nextToken,
    listTunnels_thingName,
    listTunnels_maxResults,
    listTunnelsResponse_nextToken,
    listTunnelsResponse_tunnelSummaries,
    listTunnelsResponse_httpStatus,

    -- ** OpenTunnel
    openTunnel_tags,
    openTunnel_description,
    openTunnel_destinationConfig,
    openTunnel_timeoutConfig,
    openTunnelResponse_sourceAccessToken,
    openTunnelResponse_destinationAccessToken,
    openTunnelResponse_tunnelId,
    openTunnelResponse_tunnelArn,
    openTunnelResponse_httpStatus,

    -- ** RotateTunnelAccessToken
    rotateTunnelAccessToken_destinationConfig,
    rotateTunnelAccessToken_tunnelId,
    rotateTunnelAccessToken_clientMode,
    rotateTunnelAccessTokenResponse_sourceAccessToken,
    rotateTunnelAccessTokenResponse_destinationAccessToken,
    rotateTunnelAccessTokenResponse_tunnelArn,
    rotateTunnelAccessTokenResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** ConnectionState
    connectionState_lastUpdatedAt,
    connectionState_status,

    -- ** DestinationConfig
    destinationConfig_thingName,
    destinationConfig_services,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TimeoutConfig
    timeoutConfig_maxLifetimeTimeoutMinutes,

    -- ** Tunnel
    tunnel_tags,
    tunnel_lastUpdatedAt,
    tunnel_destinationConnectionState,
    tunnel_status,
    tunnel_description,
    tunnel_destinationConfig,
    tunnel_tunnelId,
    tunnel_timeoutConfig,
    tunnel_tunnelArn,
    tunnel_createdAt,
    tunnel_sourceConnectionState,

    -- ** TunnelSummary
    tunnelSummary_lastUpdatedAt,
    tunnelSummary_status,
    tunnelSummary_description,
    tunnelSummary_tunnelId,
    tunnelSummary_tunnelArn,
    tunnelSummary_createdAt,
  )
where

import Amazonka.IoTSecureTunneling.CloseTunnel
import Amazonka.IoTSecureTunneling.DescribeTunnel
import Amazonka.IoTSecureTunneling.ListTagsForResource
import Amazonka.IoTSecureTunneling.ListTunnels
import Amazonka.IoTSecureTunneling.OpenTunnel
import Amazonka.IoTSecureTunneling.RotateTunnelAccessToken
import Amazonka.IoTSecureTunneling.TagResource
import Amazonka.IoTSecureTunneling.Types.ConnectionState
import Amazonka.IoTSecureTunneling.Types.DestinationConfig
import Amazonka.IoTSecureTunneling.Types.Tag
import Amazonka.IoTSecureTunneling.Types.TimeoutConfig
import Amazonka.IoTSecureTunneling.Types.Tunnel
import Amazonka.IoTSecureTunneling.Types.TunnelSummary
import Amazonka.IoTSecureTunneling.UntagResource
