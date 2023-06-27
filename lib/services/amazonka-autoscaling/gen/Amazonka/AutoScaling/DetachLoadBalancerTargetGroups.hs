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
-- Module      : Amazonka.AutoScaling.DetachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API operation is superseded by DetachTrafficSources, which can
-- detach multiple traffic sources types. We recommend using
-- @DetachTrafficSources@ to simplify how you manage traffic sources.
-- However, we continue to support @DetachLoadBalancerTargetGroups@. You
-- can use both the original @DetachLoadBalancerTargetGroups@ API operation
-- and @DetachTrafficSources@ on the same Auto Scaling group.
--
-- Detaches one or more target groups from the specified Auto Scaling
-- group.
--
-- When you detach a target group, it enters the @Removing@ state while
-- deregistering the instances in the group. When all instances are
-- deregistered, then you can no longer describe the target group using the
-- DescribeLoadBalancerTargetGroups API call. The instances remain running.
--
-- You can use this operation to detach target groups that were attached by
-- using AttachLoadBalancerTargetGroups, but not for target groups that
-- were attached by using AttachTrafficSources.
module Amazonka.AutoScaling.DetachLoadBalancerTargetGroups
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachLoadBalancerTargetGroups' smart constructor.
data DetachLoadBalancerTargetGroups = DetachLoadBalancerTargetGroups'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The Amazon Resource Names (ARN) of the target groups. You can specify up
    -- to 10 target groups.
    targetGroupARNs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DetachLoadBalancerTargetGroups
newDetachLoadBalancerTargetGroups
  pAutoScalingGroupName_ =
    DetachLoadBalancerTargetGroups'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        targetGroupARNs = Prelude.mempty
      }

-- | The name of the Auto Scaling group.
detachLoadBalancerTargetGroups_autoScalingGroupName :: Lens.Lens' DetachLoadBalancerTargetGroups Prelude.Text
detachLoadBalancerTargetGroups_autoScalingGroupName = Lens.lens (\DetachLoadBalancerTargetGroups' {autoScalingGroupName} -> autoScalingGroupName) (\s@DetachLoadBalancerTargetGroups' {} a -> s {autoScalingGroupName = a} :: DetachLoadBalancerTargetGroups)

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up
-- to 10 target groups.
detachLoadBalancerTargetGroups_targetGroupARNs :: Lens.Lens' DetachLoadBalancerTargetGroups [Prelude.Text]
detachLoadBalancerTargetGroups_targetGroupARNs = Lens.lens (\DetachLoadBalancerTargetGroups' {targetGroupARNs} -> targetGroupARNs) (\s@DetachLoadBalancerTargetGroups' {} a -> s {targetGroupARNs = a} :: DetachLoadBalancerTargetGroups) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DetachLoadBalancerTargetGroups
  where
  type
    AWSResponse DetachLoadBalancerTargetGroups =
      DetachLoadBalancerTargetGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DetachLoadBalancerTargetGroupsResult"
      ( \s h x ->
          DetachLoadBalancerTargetGroupsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachLoadBalancerTargetGroups
  where
  hashWithSalt
    _salt
    DetachLoadBalancerTargetGroups' {..} =
      _salt
        `Prelude.hashWithSalt` autoScalingGroupName
        `Prelude.hashWithSalt` targetGroupARNs

instance
  Prelude.NFData
    DetachLoadBalancerTargetGroups
  where
  rnf DetachLoadBalancerTargetGroups' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf targetGroupARNs

instance
  Data.ToHeaders
    DetachLoadBalancerTargetGroups
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DetachLoadBalancerTargetGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DetachLoadBalancerTargetGroups where
  toQuery DetachLoadBalancerTargetGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DetachLoadBalancerTargetGroups" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "TargetGroupARNs"
          Data.=: Data.toQueryList "member" targetGroupARNs
      ]

-- | /See:/ 'newDetachLoadBalancerTargetGroupsResponse' smart constructor.
data DetachLoadBalancerTargetGroupsResponse = DetachLoadBalancerTargetGroupsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DetachLoadBalancerTargetGroupsResponse
newDetachLoadBalancerTargetGroupsResponse
  pHttpStatus_ =
    DetachLoadBalancerTargetGroupsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
detachLoadBalancerTargetGroupsResponse_httpStatus :: Lens.Lens' DetachLoadBalancerTargetGroupsResponse Prelude.Int
detachLoadBalancerTargetGroupsResponse_httpStatus = Lens.lens (\DetachLoadBalancerTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@DetachLoadBalancerTargetGroupsResponse' {} a -> s {httpStatus = a} :: DetachLoadBalancerTargetGroupsResponse)

instance
  Prelude.NFData
    DetachLoadBalancerTargetGroupsResponse
  where
  rnf DetachLoadBalancerTargetGroupsResponse' {..} =
    Prelude.rnf httpStatus
