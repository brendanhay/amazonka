{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified subnets from the set of configured subnets for the
-- load balancer.
--
-- After a subnet is removed, all EC2 instances registered with the load
-- balancer in the removed subnet go into the @OutOfService@ state. Then,
-- the load balancer balances the traffic among the remaining routable
-- subnets.
module Network.AWS.ELB.DetachLoadBalancerFromSubnets
  ( -- * Creating a Request
    DetachLoadBalancerFromSubnets (..),
    newDetachLoadBalancerFromSubnets,

    -- * Request Lenses
    detachLoadBalancerFromSubnets_loadBalancerName,
    detachLoadBalancerFromSubnets_subnets,

    -- * Destructuring the Response
    DetachLoadBalancerFromSubnetsResponse (..),
    newDetachLoadBalancerFromSubnetsResponse,

    -- * Response Lenses
    detachLoadBalancerFromSubnetsResponse_subnets,
    detachLoadBalancerFromSubnetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DetachLoadBalancerFromSubnets.
--
-- /See:/ 'newDetachLoadBalancerFromSubnets' smart constructor.
data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The IDs of the subnets.
    subnets :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachLoadBalancerFromSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'detachLoadBalancerFromSubnets_loadBalancerName' - The name of the load balancer.
--
-- 'subnets', 'detachLoadBalancerFromSubnets_subnets' - The IDs of the subnets.
newDetachLoadBalancerFromSubnets ::
  -- | 'loadBalancerName'
  Core.Text ->
  DetachLoadBalancerFromSubnets
newDetachLoadBalancerFromSubnets pLoadBalancerName_ =
  DetachLoadBalancerFromSubnets'
    { loadBalancerName =
        pLoadBalancerName_,
      subnets = Core.mempty
    }

-- | The name of the load balancer.
detachLoadBalancerFromSubnets_loadBalancerName :: Lens.Lens' DetachLoadBalancerFromSubnets Core.Text
detachLoadBalancerFromSubnets_loadBalancerName = Lens.lens (\DetachLoadBalancerFromSubnets' {loadBalancerName} -> loadBalancerName) (\s@DetachLoadBalancerFromSubnets' {} a -> s {loadBalancerName = a} :: DetachLoadBalancerFromSubnets)

-- | The IDs of the subnets.
detachLoadBalancerFromSubnets_subnets :: Lens.Lens' DetachLoadBalancerFromSubnets [Core.Text]
detachLoadBalancerFromSubnets_subnets = Lens.lens (\DetachLoadBalancerFromSubnets' {subnets} -> subnets) (\s@DetachLoadBalancerFromSubnets' {} a -> s {subnets = a} :: DetachLoadBalancerFromSubnets) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DetachLoadBalancerFromSubnets
  where
  type
    AWSResponse DetachLoadBalancerFromSubnets =
      DetachLoadBalancerFromSubnetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancerFromSubnetsResult"
      ( \s h x ->
          DetachLoadBalancerFromSubnetsResponse'
            Core.<$> ( x Core..@? "Subnets" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachLoadBalancerFromSubnets

instance Core.NFData DetachLoadBalancerFromSubnets

instance Core.ToHeaders DetachLoadBalancerFromSubnets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachLoadBalancerFromSubnets where
  toPath = Core.const "/"

instance Core.ToQuery DetachLoadBalancerFromSubnets where
  toQuery DetachLoadBalancerFromSubnets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DetachLoadBalancerFromSubnets" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "Subnets" Core.=: Core.toQueryList "member" subnets
      ]

-- | Contains the output of DetachLoadBalancerFromSubnets.
--
-- /See:/ 'newDetachLoadBalancerFromSubnetsResponse' smart constructor.
data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse'
  { -- | The IDs of the remaining subnets for the load balancer.
    subnets :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachLoadBalancerFromSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnets', 'detachLoadBalancerFromSubnetsResponse_subnets' - The IDs of the remaining subnets for the load balancer.
--
-- 'httpStatus', 'detachLoadBalancerFromSubnetsResponse_httpStatus' - The response's http status code.
newDetachLoadBalancerFromSubnetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachLoadBalancerFromSubnetsResponse
newDetachLoadBalancerFromSubnetsResponse pHttpStatus_ =
  DetachLoadBalancerFromSubnetsResponse'
    { subnets =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IDs of the remaining subnets for the load balancer.
detachLoadBalancerFromSubnetsResponse_subnets :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse (Core.Maybe [Core.Text])
detachLoadBalancerFromSubnetsResponse_subnets = Lens.lens (\DetachLoadBalancerFromSubnetsResponse' {subnets} -> subnets) (\s@DetachLoadBalancerFromSubnetsResponse' {} a -> s {subnets = a} :: DetachLoadBalancerFromSubnetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detachLoadBalancerFromSubnetsResponse_httpStatus :: Lens.Lens' DetachLoadBalancerFromSubnetsResponse Core.Int
detachLoadBalancerFromSubnetsResponse_httpStatus = Lens.lens (\DetachLoadBalancerFromSubnetsResponse' {httpStatus} -> httpStatus) (\s@DetachLoadBalancerFromSubnetsResponse' {} a -> s {httpStatus = a} :: DetachLoadBalancerFromSubnetsResponse)

instance
  Core.NFData
    DetachLoadBalancerFromSubnetsResponse
