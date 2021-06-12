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
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-load-balancer.html Elastic Load Balancing and Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
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

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The Amazon Resource Names (ARN) of the target groups. You can specify up
    -- to 10 target groups. To get the ARN of a target group, use the Elastic
    -- Load Balancing
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
    -- API operation.
    targetGroupARNs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'targetGroupARNs', 'attachLoadBalancerTargetGroups_targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups. You can specify up
-- to 10 target groups. To get the ARN of a target group, use the Elastic
-- Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
newAttachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  AttachLoadBalancerTargetGroups
newAttachLoadBalancerTargetGroups
  pAutoScalingGroupName_ =
    AttachLoadBalancerTargetGroups'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        targetGroupARNs = Core.mempty
      }

-- | The name of the Auto Scaling group.
attachLoadBalancerTargetGroups_autoScalingGroupName :: Lens.Lens' AttachLoadBalancerTargetGroups Core.Text
attachLoadBalancerTargetGroups_autoScalingGroupName = Lens.lens (\AttachLoadBalancerTargetGroups' {autoScalingGroupName} -> autoScalingGroupName) (\s@AttachLoadBalancerTargetGroups' {} a -> s {autoScalingGroupName = a} :: AttachLoadBalancerTargetGroups)

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up
-- to 10 target groups. To get the ARN of a target group, use the Elastic
-- Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
attachLoadBalancerTargetGroups_targetGroupARNs :: Lens.Lens' AttachLoadBalancerTargetGroups [Core.Text]
attachLoadBalancerTargetGroups_targetGroupARNs = Lens.lens (\AttachLoadBalancerTargetGroups' {targetGroupARNs} -> targetGroupARNs) (\s@AttachLoadBalancerTargetGroups' {} a -> s {targetGroupARNs = a} :: AttachLoadBalancerTargetGroups) Core.. Lens._Coerce

instance
  Core.AWSRequest
    AttachLoadBalancerTargetGroups
  where
  type
    AWSResponse AttachLoadBalancerTargetGroups =
      AttachLoadBalancerTargetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "AttachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          AttachLoadBalancerTargetGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachLoadBalancerTargetGroups

instance Core.NFData AttachLoadBalancerTargetGroups

instance
  Core.ToHeaders
    AttachLoadBalancerTargetGroups
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AttachLoadBalancerTargetGroups where
  toPath = Core.const "/"

instance Core.ToQuery AttachLoadBalancerTargetGroups where
  toQuery AttachLoadBalancerTargetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "AttachLoadBalancerTargetGroups" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "TargetGroupARNs"
          Core.=: Core.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'newAttachLoadBalancerTargetGroupsResponse' smart constructor.
data AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AttachLoadBalancerTargetGroupsResponse
newAttachLoadBalancerTargetGroupsResponse
  pHttpStatus_ =
    AttachLoadBalancerTargetGroupsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
attachLoadBalancerTargetGroupsResponse_httpStatus :: Lens.Lens' AttachLoadBalancerTargetGroupsResponse Core.Int
attachLoadBalancerTargetGroupsResponse_httpStatus = Lens.lens (\AttachLoadBalancerTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@AttachLoadBalancerTargetGroupsResponse' {} a -> s {httpStatus = a} :: AttachLoadBalancerTargetGroupsResponse)

instance
  Core.NFData
    AttachLoadBalancerTargetGroupsResponse
