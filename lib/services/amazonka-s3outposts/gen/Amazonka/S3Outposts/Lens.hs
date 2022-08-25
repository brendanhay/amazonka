{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3Outposts.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3Outposts.Lens
  ( -- * Operations

    -- ** CreateEndpoint
    createEndpoint_customerOwnedIpv4Pool,
    createEndpoint_accessType,
    createEndpoint_outpostId,
    createEndpoint_subnetId,
    createEndpoint_securityGroupId,
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointId,
    deleteEndpoint_outpostId,

    -- ** ListEndpoints
    listEndpoints_nextToken,
    listEndpoints_maxResults,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_endpoints,
    listEndpointsResponse_httpStatus,

    -- ** ListSharedEndpoints
    listSharedEndpoints_nextToken,
    listSharedEndpoints_maxResults,
    listSharedEndpoints_outpostId,
    listSharedEndpointsResponse_nextToken,
    listSharedEndpointsResponse_endpoints,
    listSharedEndpointsResponse_httpStatus,

    -- * Types

    -- ** Endpoint
    endpoint_outpostsId,
    endpoint_subnetId,
    endpoint_securityGroupId,
    endpoint_status,
    endpoint_customerOwnedIpv4Pool,
    endpoint_cidrBlock,
    endpoint_creationTime,
    endpoint_accessType,
    endpoint_vpcId,
    endpoint_endpointArn,
    endpoint_networkInterfaces,

    -- ** NetworkInterface
    networkInterface_networkInterfaceId,
  )
where

import Amazonka.S3Outposts.CreateEndpoint
import Amazonka.S3Outposts.DeleteEndpoint
import Amazonka.S3Outposts.ListEndpoints
import Amazonka.S3Outposts.ListSharedEndpoints
import Amazonka.S3Outposts.Types.Endpoint
import Amazonka.S3Outposts.Types.NetworkInterface
