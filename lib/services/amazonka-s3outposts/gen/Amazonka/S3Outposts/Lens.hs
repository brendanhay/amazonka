{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.S3Outposts.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3Outposts.Lens
  ( -- * Operations

    -- ** CreateEndpoint
    createEndpoint_accessType,
    createEndpoint_customerOwnedIpv4Pool,
    createEndpoint_outpostId,
    createEndpoint_subnetId,
    createEndpoint_securityGroupId,
    createEndpointResponse_endpointArn,
    createEndpointResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointId,
    deleteEndpoint_outpostId,

    -- ** ListEndpoints
    listEndpoints_maxResults,
    listEndpoints_nextToken,
    listEndpointsResponse_endpoints,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,

    -- ** ListOutpostsWithS3
    listOutpostsWithS3_maxResults,
    listOutpostsWithS3_nextToken,
    listOutpostsWithS3Response_nextToken,
    listOutpostsWithS3Response_outposts,
    listOutpostsWithS3Response_httpStatus,

    -- ** ListSharedEndpoints
    listSharedEndpoints_maxResults,
    listSharedEndpoints_nextToken,
    listSharedEndpoints_outpostId,
    listSharedEndpointsResponse_endpoints,
    listSharedEndpointsResponse_nextToken,
    listSharedEndpointsResponse_httpStatus,

    -- * Types

    -- ** Endpoint
    endpoint_accessType,
    endpoint_cidrBlock,
    endpoint_creationTime,
    endpoint_customerOwnedIpv4Pool,
    endpoint_endpointArn,
    endpoint_failedReason,
    endpoint_networkInterfaces,
    endpoint_outpostsId,
    endpoint_securityGroupId,
    endpoint_status,
    endpoint_subnetId,
    endpoint_vpcId,

    -- ** FailedReason
    failedReason_errorCode,
    failedReason_message,

    -- ** NetworkInterface
    networkInterface_networkInterfaceId,

    -- ** Outpost
    outpost_capacityInBytes,
    outpost_outpostArn,
    outpost_outpostId,
    outpost_ownerId,
  )
where

import Amazonka.S3Outposts.CreateEndpoint
import Amazonka.S3Outposts.DeleteEndpoint
import Amazonka.S3Outposts.ListEndpoints
import Amazonka.S3Outposts.ListOutpostsWithS3
import Amazonka.S3Outposts.ListSharedEndpoints
import Amazonka.S3Outposts.Types.Endpoint
import Amazonka.S3Outposts.Types.FailedReason
import Amazonka.S3Outposts.Types.NetworkInterface
import Amazonka.S3Outposts.Types.Outpost
