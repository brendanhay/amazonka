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
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To attach an Application Load Balancer, Network Load Balancer, or
-- Gateway Load Balancer, use the AttachLoadBalancerTargetGroups API
-- operation instead.
--
-- Attaches one or more Classic Load Balancers to the specified Auto
-- Scaling group. Amazon EC2 Auto Scaling registers the running instances
-- with these Classic Load Balancers.
--
-- To describe the load balancers for an Auto Scaling group, call the
-- DescribeLoadBalancers API. To detach the load balancer from the Auto
-- Scaling group, call the DetachLoadBalancers API.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.AttachLoadBalancers
  ( -- * Creating a Request
    AttachLoadBalancers (..),
    newAttachLoadBalancers,

    -- * Request Lenses
    attachLoadBalancers_autoScalingGroupName,
    attachLoadBalancers_loadBalancerNames,

    -- * Destructuring the Response
    AttachLoadBalancersResponse (..),
    newAttachLoadBalancersResponse,

    -- * Response Lenses
    attachLoadBalancersResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachLoadBalancers' smart constructor.
data AttachLoadBalancers = AttachLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The names of the load balancers. You can specify up to 10 load
    -- balancers.
    loadBalancerNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'attachLoadBalancers_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'loadBalancerNames', 'attachLoadBalancers_loadBalancerNames' - The names of the load balancers. You can specify up to 10 load
-- balancers.
newAttachLoadBalancers ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  AttachLoadBalancers
newAttachLoadBalancers pAutoScalingGroupName_ =
  AttachLoadBalancers'
    { autoScalingGroupName =
        pAutoScalingGroupName_,
      loadBalancerNames = Prelude.mempty
    }

-- | The name of the Auto Scaling group.
attachLoadBalancers_autoScalingGroupName :: Lens.Lens' AttachLoadBalancers Prelude.Text
attachLoadBalancers_autoScalingGroupName = Lens.lens (\AttachLoadBalancers' {autoScalingGroupName} -> autoScalingGroupName) (\s@AttachLoadBalancers' {} a -> s {autoScalingGroupName = a} :: AttachLoadBalancers)

-- | The names of the load balancers. You can specify up to 10 load
-- balancers.
attachLoadBalancers_loadBalancerNames :: Lens.Lens' AttachLoadBalancers [Prelude.Text]
attachLoadBalancers_loadBalancerNames = Lens.lens (\AttachLoadBalancers' {loadBalancerNames} -> loadBalancerNames) (\s@AttachLoadBalancers' {} a -> s {loadBalancerNames = a} :: AttachLoadBalancers) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AttachLoadBalancers where
  type
    Rs AttachLoadBalancers =
      AttachLoadBalancersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancersResult"
      ( \s h x ->
          AttachLoadBalancersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachLoadBalancers

instance Prelude.NFData AttachLoadBalancers

instance Prelude.ToHeaders AttachLoadBalancers where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AttachLoadBalancers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AttachLoadBalancers where
  toQuery AttachLoadBalancers' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AttachLoadBalancers" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "LoadBalancerNames"
          Prelude.=: Prelude.toQueryList "member" loadBalancerNames
      ]

-- | /See:/ 'newAttachLoadBalancersResponse' smart constructor.
data AttachLoadBalancersResponse = AttachLoadBalancersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachLoadBalancersResponse_httpStatus' - The response's http status code.
newAttachLoadBalancersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachLoadBalancersResponse
newAttachLoadBalancersResponse pHttpStatus_ =
  AttachLoadBalancersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
attachLoadBalancersResponse_httpStatus :: Lens.Lens' AttachLoadBalancersResponse Prelude.Int
attachLoadBalancersResponse_httpStatus = Lens.lens (\AttachLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@AttachLoadBalancersResponse' {} a -> s {httpStatus = a} :: AttachLoadBalancersResponse)

instance Prelude.NFData AttachLoadBalancersResponse
