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
-- Gets information about the lifecycle hooks for the specified Auto
-- Scaling group.
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLifecycleHooks' smart constructor.
data DescribeLifecycleHooks = DescribeLifecycleHooks'
  { -- | The names of one or more lifecycle hooks. If you omit this parameter,
    -- all lifecycle hooks are described.
    lifecycleHookNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeLifecycleHooks
newDescribeLifecycleHooks pAutoScalingGroupName_ =
  DescribeLifecycleHooks'
    { lifecycleHookNames =
        Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The names of one or more lifecycle hooks. If you omit this parameter,
-- all lifecycle hooks are described.
describeLifecycleHooks_lifecycleHookNames :: Lens.Lens' DescribeLifecycleHooks (Prelude.Maybe [Prelude.Text])
describeLifecycleHooks_lifecycleHookNames = Lens.lens (\DescribeLifecycleHooks' {lifecycleHookNames} -> lifecycleHookNames) (\s@DescribeLifecycleHooks' {} a -> s {lifecycleHookNames = a} :: DescribeLifecycleHooks) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
describeLifecycleHooks_autoScalingGroupName :: Lens.Lens' DescribeLifecycleHooks Prelude.Text
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
            Prelude.<$> ( x Core..@? "LifecycleHooks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLifecycleHooks

instance Prelude.NFData DescribeLifecycleHooks

instance Core.ToHeaders DescribeLifecycleHooks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLifecycleHooks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLifecycleHooks where
  toQuery DescribeLifecycleHooks' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeLifecycleHooks" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "LifecycleHookNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> lifecycleHookNames
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]

-- | /See:/ 'newDescribeLifecycleHooksResponse' smart constructor.
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
  { -- | The lifecycle hooks for the specified group.
    lifecycleHooks :: Prelude.Maybe [LifecycleHook],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeLifecycleHooksResponse
newDescribeLifecycleHooksResponse pHttpStatus_ =
  DescribeLifecycleHooksResponse'
    { lifecycleHooks =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle hooks for the specified group.
describeLifecycleHooksResponse_lifecycleHooks :: Lens.Lens' DescribeLifecycleHooksResponse (Prelude.Maybe [LifecycleHook])
describeLifecycleHooksResponse_lifecycleHooks = Lens.lens (\DescribeLifecycleHooksResponse' {lifecycleHooks} -> lifecycleHooks) (\s@DescribeLifecycleHooksResponse' {} a -> s {lifecycleHooks = a} :: DescribeLifecycleHooksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLifecycleHooksResponse_httpStatus :: Lens.Lens' DescribeLifecycleHooksResponse Prelude.Int
describeLifecycleHooksResponse_httpStatus = Lens.lens (\DescribeLifecycleHooksResponse' {httpStatus} -> httpStatus) (\s@DescribeLifecycleHooksResponse' {} a -> s {httpStatus = a} :: DescribeLifecycleHooksResponse)

instance
  Prelude.NFData
    DescribeLifecycleHooksResponse
