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
-- Module      : Amazonka.AutoScaling.AttachLoadBalancers
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- DescribeLoadBalancers API. To detach a load balancer from the Auto
-- Scaling group, call the DetachLoadBalancers API.
--
-- This operation is additive and does not detach existing Classic Load
-- Balancers or target groups from the Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Use Elastic Load Balancing to distribute traffic across the instances in your Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.AttachLoadBalancers
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachLoadBalancers' smart constructor.
data AttachLoadBalancers = AttachLoadBalancers'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The names of the load balancers. You can specify up to 10 load
    -- balancers.
    loadBalancerNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
attachLoadBalancers_loadBalancerNames = Lens.lens (\AttachLoadBalancers' {loadBalancerNames} -> loadBalancerNames) (\s@AttachLoadBalancers' {} a -> s {loadBalancerNames = a} :: AttachLoadBalancers) Prelude.. Lens.coerced

instance Core.AWSRequest AttachLoadBalancers where
  type
    AWSResponse AttachLoadBalancers =
      AttachLoadBalancersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancersResult"
      ( \s h x ->
          AttachLoadBalancersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachLoadBalancers where
  hashWithSalt _salt AttachLoadBalancers' {..} =
    _salt `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` loadBalancerNames

instance Prelude.NFData AttachLoadBalancers where
  rnf AttachLoadBalancers' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf loadBalancerNames

instance Data.ToHeaders AttachLoadBalancers where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachLoadBalancers where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachLoadBalancers where
  toQuery AttachLoadBalancers' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AttachLoadBalancers" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "LoadBalancerNames"
          Data.=: Data.toQueryList "member" loadBalancerNames
      ]

-- | /See:/ 'newAttachLoadBalancersResponse' smart constructor.
data AttachLoadBalancersResponse = AttachLoadBalancersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData AttachLoadBalancersResponse where
  rnf AttachLoadBalancersResponse' {..} =
    Prelude.rnf httpStatus
