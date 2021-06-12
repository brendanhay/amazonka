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
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the lifecycle hooks for the specified Auto Scaling group.
module Network.AWS.AutoScaling.DescribeLifecycleHooks
  ( -- * Creating a Request
    DescribeLifecycleHooks (..),
    newDescribeLifecycleHooks,

    -- * Request Lenses
    describeLifecycleHooks_lifecycleHookNames,
    describeLifecycleHooks_autoScalingGroupName,

    -- * Destructuring the Response
    DescribeLifecycleHooksResponse (..),
    newDescribeLifecycleHooksResponse,

    -- * Response Lenses
    describeLifecycleHooksResponse_lifecycleHooks,
    describeLifecycleHooksResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLifecycleHooks' smart constructor.
data DescribeLifecycleHooks = DescribeLifecycleHooks'
  { -- | The names of one or more lifecycle hooks. If you omit this parameter,
    -- all lifecycle hooks are described.
    lifecycleHookNames :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLifecycleHooks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHookNames', 'describeLifecycleHooks_lifecycleHookNames' - The names of one or more lifecycle hooks. If you omit this parameter,
-- all lifecycle hooks are described.
--
-- 'autoScalingGroupName', 'describeLifecycleHooks_autoScalingGroupName' - The name of the Auto Scaling group.
newDescribeLifecycleHooks ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  DescribeLifecycleHooks
newDescribeLifecycleHooks pAutoScalingGroupName_ =
  DescribeLifecycleHooks'
    { lifecycleHookNames =
        Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The names of one or more lifecycle hooks. If you omit this parameter,
-- all lifecycle hooks are described.
describeLifecycleHooks_lifecycleHookNames :: Lens.Lens' DescribeLifecycleHooks (Core.Maybe [Core.Text])
describeLifecycleHooks_lifecycleHookNames = Lens.lens (\DescribeLifecycleHooks' {lifecycleHookNames} -> lifecycleHookNames) (\s@DescribeLifecycleHooks' {} a -> s {lifecycleHookNames = a} :: DescribeLifecycleHooks) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
describeLifecycleHooks_autoScalingGroupName :: Lens.Lens' DescribeLifecycleHooks Core.Text
describeLifecycleHooks_autoScalingGroupName = Lens.lens (\DescribeLifecycleHooks' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribeLifecycleHooks' {} a -> s {autoScalingGroupName = a} :: DescribeLifecycleHooks)

instance Core.AWSRequest DescribeLifecycleHooks where
  type
    AWSResponse DescribeLifecycleHooks =
      DescribeLifecycleHooksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLifecycleHooksResult"
      ( \s h x ->
          DescribeLifecycleHooksResponse'
            Core.<$> ( x Core..@? "LifecycleHooks" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLifecycleHooks

instance Core.NFData DescribeLifecycleHooks

instance Core.ToHeaders DescribeLifecycleHooks where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLifecycleHooks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLifecycleHooks where
  toQuery DescribeLifecycleHooks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeLifecycleHooks" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "LifecycleHookNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> lifecycleHookNames
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeLifecycleHooksResponse' smart constructor.
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
  { -- | The lifecycle hooks for the specified group.
    lifecycleHooks :: Core.Maybe [LifecycleHook],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLifecycleHooksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHooks', 'describeLifecycleHooksResponse_lifecycleHooks' - The lifecycle hooks for the specified group.
--
-- 'httpStatus', 'describeLifecycleHooksResponse_httpStatus' - The response's http status code.
newDescribeLifecycleHooksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLifecycleHooksResponse
newDescribeLifecycleHooksResponse pHttpStatus_ =
  DescribeLifecycleHooksResponse'
    { lifecycleHooks =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle hooks for the specified group.
describeLifecycleHooksResponse_lifecycleHooks :: Lens.Lens' DescribeLifecycleHooksResponse (Core.Maybe [LifecycleHook])
describeLifecycleHooksResponse_lifecycleHooks = Lens.lens (\DescribeLifecycleHooksResponse' {lifecycleHooks} -> lifecycleHooks) (\s@DescribeLifecycleHooksResponse' {} a -> s {lifecycleHooks = a} :: DescribeLifecycleHooksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLifecycleHooksResponse_httpStatus :: Lens.Lens' DescribeLifecycleHooksResponse Core.Int
describeLifecycleHooksResponse_httpStatus = Lens.lens (\DescribeLifecycleHooksResponse' {httpStatus} -> httpStatus) (\s@DescribeLifecycleHooksResponse' {} a -> s {httpStatus = a} :: DescribeLifecycleHooksResponse)

instance Core.NFData DescribeLifecycleHooksResponse
