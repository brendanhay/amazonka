{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachLoadBalancers' smart constructor.
data DetachLoadBalancers = DetachLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The names of the load balancers. You can specify up to 10 load
    -- balancers.
    loadBalancerNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DetachLoadBalancers
newDetachLoadBalancers pAutoScalingGroupName_ =
  DetachLoadBalancers'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      loadBalancerNames = Prelude.mempty
    }

-- | The name of the Auto Scaling group.
detachLoadBalancers_autoScalingGroupName :: Lens.Lens' DetachLoadBalancers Prelude.Text
detachLoadBalancers_autoScalingGroupName = Lens.lens (\DetachLoadBalancers' {autoScalingGroupName} -> autoScalingGroupName) (\s@DetachLoadBalancers' {} a -> s {autoScalingGroupName = a} :: DetachLoadBalancers)

-- | The names of the load balancers. You can specify up to 10 load
-- balancers.
detachLoadBalancers_loadBalancerNames :: Lens.Lens' DetachLoadBalancers [Prelude.Text]
detachLoadBalancers_loadBalancerNames = Lens.lens (\DetachLoadBalancers' {loadBalancerNames} -> loadBalancerNames) (\s@DetachLoadBalancers' {} a -> s {loadBalancerNames = a} :: DetachLoadBalancers) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DetachLoadBalancers where
  type
    Rs DetachLoadBalancers =
      DetachLoadBalancersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancersResult"
      ( \s h x ->
          DetachLoadBalancersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachLoadBalancers

instance Prelude.NFData DetachLoadBalancers

instance Prelude.ToHeaders DetachLoadBalancers where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DetachLoadBalancers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DetachLoadBalancers where
  toQuery DetachLoadBalancers' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DetachLoadBalancers" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "LoadBalancerNames"
          Prelude.=: Prelude.toQueryList "member" loadBalancerNames
      ]

-- | /See:/ 'newDetachLoadBalancersResponse' smart constructor.
data DetachLoadBalancersResponse = DetachLoadBalancersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DetachLoadBalancersResponse
newDetachLoadBalancersResponse pHttpStatus_ =
  DetachLoadBalancersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
detachLoadBalancersResponse_httpStatus :: Lens.Lens' DetachLoadBalancersResponse Prelude.Int
detachLoadBalancersResponse_httpStatus = Lens.lens (\DetachLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DetachLoadBalancersResponse' {} a -> s {httpStatus = a} :: DetachLoadBalancersResponse)

instance Prelude.NFData DetachLoadBalancersResponse
