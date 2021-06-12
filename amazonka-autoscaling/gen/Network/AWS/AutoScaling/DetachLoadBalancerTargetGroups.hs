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
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more target groups from the specified Auto Scaling
-- group.
module Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
  ( -- * Creating a Request
    DetachLoadBalancerTargetGroups (..),
    newDetachLoadBalancerTargetGroups,

    -- * Request Lenses
    detachLoadBalancerTargetGroups_autoScalingGroupName,
    detachLoadBalancerTargetGroups_targetGroupARNs,

    -- * Destructuring the Response
    DetachLoadBalancerTargetGroupsResponse (..),
    newDetachLoadBalancerTargetGroupsResponse,

    -- * Response Lenses
    detachLoadBalancerTargetGroupsResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachLoadBalancerTargetGroups' smart constructor.
data DetachLoadBalancerTargetGroups = DetachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text,
    -- | The Amazon Resource Names (ARN) of the target groups. You can specify up
    -- to 10 target groups.
    targetGroupARNs :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachLoadBalancerTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'detachLoadBalancerTargetGroups_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'targetGroupARNs', 'detachLoadBalancerTargetGroups_targetGroupARNs' - The Amazon Resource Names (ARN) of the target groups. You can specify up
-- to 10 target groups.
newDetachLoadBalancerTargetGroups ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DetachLoadBalancerTargetGroups
newDetachLoadBalancerTargetGroups
  pAutoScalingGroupName_ =
    DetachLoadBalancerTargetGroups'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        targetGroupARNs = Core.mempty
      }

-- | The name of the Auto Scaling group.
detachLoadBalancerTargetGroups_autoScalingGroupName :: Lens.Lens' DetachLoadBalancerTargetGroups Core.Text
detachLoadBalancerTargetGroups_autoScalingGroupName = Lens.lens (\DetachLoadBalancerTargetGroups' {autoScalingGroupName} -> autoScalingGroupName) (\s@DetachLoadBalancerTargetGroups' {} a -> s {autoScalingGroupName = a} :: DetachLoadBalancerTargetGroups)

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up
-- to 10 target groups.
detachLoadBalancerTargetGroups_targetGroupARNs :: Lens.Lens' DetachLoadBalancerTargetGroups [Core.Text]
detachLoadBalancerTargetGroups_targetGroupARNs = Lens.lens (\DetachLoadBalancerTargetGroups' {targetGroupARNs} -> targetGroupARNs) (\s@DetachLoadBalancerTargetGroups' {} a -> s {targetGroupARNs = a} :: DetachLoadBalancerTargetGroups) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DetachLoadBalancerTargetGroups
  where
  type
    AWSResponse DetachLoadBalancerTargetGroups =
      DetachLoadBalancerTargetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DetachLoadBalancerTargetGroupsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachLoadBalancerTargetGroups

instance Core.NFData DetachLoadBalancerTargetGroups

instance
  Core.ToHeaders
    DetachLoadBalancerTargetGroups
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachLoadBalancerTargetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DetachLoadBalancerTargetGroups where
  toQuery DetachLoadBalancerTargetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DetachLoadBalancerTargetGroups" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "TargetGroupARNs"
          Core.=: Core.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'newDetachLoadBalancerTargetGroupsResponse' smart constructor.
data DetachLoadBalancerTargetGroupsResponse = DetachLoadBalancerTargetGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachLoadBalancerTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'detachLoadBalancerTargetGroupsResponse_httpStatus' - The response's http status code.
newDetachLoadBalancerTargetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachLoadBalancerTargetGroupsResponse
newDetachLoadBalancerTargetGroupsResponse
  pHttpStatus_ =
    DetachLoadBalancerTargetGroupsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
detachLoadBalancerTargetGroupsResponse_httpStatus :: Lens.Lens' DetachLoadBalancerTargetGroupsResponse Core.Int
detachLoadBalancerTargetGroupsResponse_httpStatus = Lens.lens (\DetachLoadBalancerTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DetachLoadBalancerTargetGroupsResponse' {} a -> s {httpStatus = a} :: DetachLoadBalancerTargetGroupsResponse)

instance
  Core.NFData
    DetachLoadBalancerTargetGroupsResponse
