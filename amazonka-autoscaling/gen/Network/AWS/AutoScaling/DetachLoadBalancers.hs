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
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more Classic Load Balancers from the specified Auto
-- Scaling group.
--
-- This operation detaches only Classic Load Balancers. If you have
-- Application Load Balancers, Network Load Balancers, or Gateway Load
-- Balancers, use the DetachLoadBalancerTargetGroups API instead.
--
-- When you detach a load balancer, it enters the @Removing@ state while
-- deregistering the instances in the group. When all instances are
-- deregistered, then you can no longer describe the load balancer using
-- the DescribeLoadBalancers API call. The instances remain running.
module Network.AWS.AutoScaling.DetachLoadBalancers
  ( -- * Creating a Request
    DetachLoadBalancers (..),
    newDetachLoadBalancers,

    -- * Request Lenses
    detachLoadBalancers_autoScalingGroupName,
    detachLoadBalancers_loadBalancerNames,

    -- * Destructuring the Response
    DetachLoadBalancersResponse (..),
    newDetachLoadBalancersResponse,

    -- * Response Lenses
    detachLoadBalancersResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The names of the load balancers. You can specify up to 10 load
    -- balancers.
    loadBalancerNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'detachLoadBalancers_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'loadBalancerNames', 'detachLoadBalancers_loadBalancerNames' - The names of the load balancers. You can specify up to 10 load
-- balancers.
newDetachLoadBalancers ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DetachLoadBalancers
newDetachLoadBalancers pAutoScalingGroupName_ =
  DetachLoadBalancers'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      loadBalancerNames = Core.mempty
    }

-- | The name of the Auto Scaling group.
detachLoadBalancers_autoScalingGroupName :: Lens.Lens' DetachLoadBalancers Core.Text
detachLoadBalancers_autoScalingGroupName = Lens.lens (\DetachLoadBalancers' {autoScalingGroupName} -> autoScalingGroupName) (\s@DetachLoadBalancers' {} a -> s {autoScalingGroupName = a} :: DetachLoadBalancers)

-- | The names of the load balancers. You can specify up to 10 load
-- balancers.
detachLoadBalancers_loadBalancerNames :: Lens.Lens' DetachLoadBalancers [Core.Text]
detachLoadBalancers_loadBalancerNames = Lens.lens (\DetachLoadBalancers' {loadBalancerNames} -> loadBalancerNames) (\s@DetachLoadBalancers' {} a -> s {loadBalancerNames = a} :: DetachLoadBalancers) Core.. Lens._Coerce

instance Core.AWSRequest DetachLoadBalancers where
  type
    AWSResponse DetachLoadBalancers =
      DetachLoadBalancersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancersResult"
      ( \s h x ->
          DetachLoadBalancersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachLoadBalancers

instance Core.NFData DetachLoadBalancers

instance Core.ToHeaders DetachLoadBalancers where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachLoadBalancers where
  toPath = Core.const "/"

instance Core.ToQuery DetachLoadBalancers where
  toQuery DetachLoadBalancers' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DetachLoadBalancers" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "LoadBalancerNames"
          Core.=: Core.toQueryList "member" loadBalancerNames
      ]

-- | /See:/ 'newDetachLoadBalancersResponse' smart constructor.
data DetachLoadBalancersResponse = DetachLoadBalancersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachLoadBalancersResponse_httpStatus' - The response's http status code.
newDetachLoadBalancersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachLoadBalancersResponse
newDetachLoadBalancersResponse pHttpStatus_ =
  DetachLoadBalancersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
detachLoadBalancersResponse_httpStatus :: Lens.Lens' DetachLoadBalancersResponse Core.Int
detachLoadBalancersResponse_httpStatus = Lens.lens (\DetachLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DetachLoadBalancersResponse' {} a -> s {httpStatus = a} :: DetachLoadBalancersResponse)

instance Core.NFData DetachLoadBalancersResponse
