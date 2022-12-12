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
-- Module      : Amazonka.AutoScaling.AttachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more target groups to the specified Auto Scaling group.
--
-- This operation is used with the following load balancer types:
--
-- -   Application Load Balancer - Operates at the application layer (layer
--     7) and supports HTTP and HTTPS.
--
-- -   Network Load Balancer - Operates at the transport layer (layer 4)
--     and supports TCP, TLS, and UDP.
--
-- -   Gateway Load Balancer - Operates at the network layer (layer 3).
--
-- To describe the target groups for an Auto Scaling group, call the
-- DescribeLoadBalancerTargetGroups API. To detach the target group from
-- the Auto Scaling group, call the DetachLoadBalancerTargetGroups API.
--
-- This operation is additive and does not detach existing target groups or
-- Classic Load Balancers from the Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Use Elastic Load Balancing to distribute traffic across the instances in your Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.AttachLoadBalancerTargetGroups
  ( -- * Creating a Request
    AttachLoadBalancerTargetGroups (..),
    newAttachLoadBalancerTargetGroups,

    -- * Request Lenses
    attachLoadBalancerTargetGroups_autoScalingGroupName,
    attachLoadBalancerTargetGroups_targetGroupARNs,

    -- * Destructuring the Response
    AttachLoadBalancerTargetGroupsResponse (..),
    newAttachLoadBalancerTargetGroupsResponse,

    -- * Response Lenses
    attachLoadBalancerTargetGroupsResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the target groups. You can specify
    -- up to 10 target groups. To get the ARN of a target group, use the
    -- Elastic Load Balancing
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
    -- API operation.
    targetGroupARNs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancerTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'attachLoadBalancerTargetGroups_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'targetGroupARNs', 'attachLoadBalancerTargetGroups_targetGroupARNs' - The Amazon Resource Names (ARNs) of the target groups. You can specify
-- up to 10 target groups. To get the ARN of a target group, use the
-- Elastic Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
newAttachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  AttachLoadBalancerTargetGroups
newAttachLoadBalancerTargetGroups
  pAutoScalingGroupName_ =
    AttachLoadBalancerTargetGroups'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        targetGroupARNs = Prelude.mempty
      }

-- | The name of the Auto Scaling group.
attachLoadBalancerTargetGroups_autoScalingGroupName :: Lens.Lens' AttachLoadBalancerTargetGroups Prelude.Text
attachLoadBalancerTargetGroups_autoScalingGroupName = Lens.lens (\AttachLoadBalancerTargetGroups' {autoScalingGroupName} -> autoScalingGroupName) (\s@AttachLoadBalancerTargetGroups' {} a -> s {autoScalingGroupName = a} :: AttachLoadBalancerTargetGroups)

-- | The Amazon Resource Names (ARNs) of the target groups. You can specify
-- up to 10 target groups. To get the ARN of a target group, use the
-- Elastic Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
attachLoadBalancerTargetGroups_targetGroupARNs :: Lens.Lens' AttachLoadBalancerTargetGroups [Prelude.Text]
attachLoadBalancerTargetGroups_targetGroupARNs = Lens.lens (\AttachLoadBalancerTargetGroups' {targetGroupARNs} -> targetGroupARNs) (\s@AttachLoadBalancerTargetGroups' {} a -> s {targetGroupARNs = a} :: AttachLoadBalancerTargetGroups) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AttachLoadBalancerTargetGroups
  where
  type
    AWSResponse AttachLoadBalancerTargetGroups =
      AttachLoadBalancerTargetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          AttachLoadBalancerTargetGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachLoadBalancerTargetGroups
  where
  hashWithSalt
    _salt
    AttachLoadBalancerTargetGroups' {..} =
      _salt `Prelude.hashWithSalt` autoScalingGroupName
        `Prelude.hashWithSalt` targetGroupARNs

instance
  Prelude.NFData
    AttachLoadBalancerTargetGroups
  where
  rnf AttachLoadBalancerTargetGroups' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf targetGroupARNs

instance
  Data.ToHeaders
    AttachLoadBalancerTargetGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AttachLoadBalancerTargetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachLoadBalancerTargetGroups where
  toQuery AttachLoadBalancerTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AttachLoadBalancerTargetGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "TargetGroupARNs"
          Data.=: Data.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'newAttachLoadBalancerTargetGroupsResponse' smart constructor.
data AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancerTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachLoadBalancerTargetGroupsResponse_httpStatus' - The response's http status code.
newAttachLoadBalancerTargetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachLoadBalancerTargetGroupsResponse
newAttachLoadBalancerTargetGroupsResponse
  pHttpStatus_ =
    AttachLoadBalancerTargetGroupsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
attachLoadBalancerTargetGroupsResponse_httpStatus :: Lens.Lens' AttachLoadBalancerTargetGroupsResponse Prelude.Int
attachLoadBalancerTargetGroupsResponse_httpStatus = Lens.lens (\AttachLoadBalancerTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@AttachLoadBalancerTargetGroupsResponse' {} a -> s {httpStatus = a} :: AttachLoadBalancerTargetGroupsResponse)

instance
  Prelude.NFData
    AttachLoadBalancerTargetGroupsResponse
  where
  rnf AttachLoadBalancerTargetGroupsResponse' {..} =
    Prelude.rnf httpStatus
