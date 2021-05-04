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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the target groups. You can specify up
    -- to 10 target groups. To get the ARN of a target group, use the Elastic
    -- Load Balancing
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
    -- API operation.
    targetGroupARNs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up
-- to 10 target groups. To get the ARN of a target group, use the Elastic
-- Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- API operation.
attachLoadBalancerTargetGroups_targetGroupARNs :: Lens.Lens' AttachLoadBalancerTargetGroups [Prelude.Text]
attachLoadBalancerTargetGroups_targetGroupARNs = Lens.lens (\AttachLoadBalancerTargetGroups' {targetGroupARNs} -> targetGroupARNs) (\s@AttachLoadBalancerTargetGroups' {} a -> s {targetGroupARNs = a} :: AttachLoadBalancerTargetGroups) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    AttachLoadBalancerTargetGroups
  where
  type
    Rs AttachLoadBalancerTargetGroups =
      AttachLoadBalancerTargetGroupsResponse
  request = Request.postQuery defaultService
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

instance
  Prelude.NFData
    AttachLoadBalancerTargetGroups

instance
  Prelude.ToHeaders
    AttachLoadBalancerTargetGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    AttachLoadBalancerTargetGroups
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AttachLoadBalancerTargetGroups
  where
  toQuery AttachLoadBalancerTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AttachLoadBalancerTargetGroups" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "TargetGroupARNs"
          Prelude.=: Prelude.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'newAttachLoadBalancerTargetGroupsResponse' smart constructor.
data AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
