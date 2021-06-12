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
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- The following hook types are supported:
--
-- -   autoscaling:EC2_INSTANCE_LAUNCHING
--
-- -   autoscaling:EC2_INSTANCE_TERMINATING
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
  ( -- * Creating a Request
    DescribeLifecycleHookTypes (..),
    newDescribeLifecycleHookTypes,

    -- * Destructuring the Response
    DescribeLifecycleHookTypesResponse (..),
    newDescribeLifecycleHookTypesResponse,

    -- * Response Lenses
    describeLifecycleHookTypesResponse_lifecycleHookTypes,
    describeLifecycleHookTypesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLifecycleHookTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLifecycleHookTypes ::
  DescribeLifecycleHookTypes
newDescribeLifecycleHookTypes =
  DescribeLifecycleHookTypes'

instance Core.AWSRequest DescribeLifecycleHookTypes where
  type
    AWSResponse DescribeLifecycleHookTypes =
      DescribeLifecycleHookTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLifecycleHookTypesResult"
      ( \s h x ->
          DescribeLifecycleHookTypesResponse'
            Core.<$> ( x Core..@? "LifecycleHookTypes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLifecycleHookTypes

instance Core.NFData DescribeLifecycleHookTypes

instance Core.ToHeaders DescribeLifecycleHookTypes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLifecycleHookTypes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLifecycleHookTypes where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeLifecycleHookTypes" :: Core.ByteString),
            "Version" Core.=: ("2011-01-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { -- | The lifecycle hook types.
    lifecycleHookTypes :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLifecycleHookTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHookTypes', 'describeLifecycleHookTypesResponse_lifecycleHookTypes' - The lifecycle hook types.
--
-- 'httpStatus', 'describeLifecycleHookTypesResponse_httpStatus' - The response's http status code.
newDescribeLifecycleHookTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLifecycleHookTypesResponse
newDescribeLifecycleHookTypesResponse pHttpStatus_ =
  DescribeLifecycleHookTypesResponse'
    { lifecycleHookTypes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The lifecycle hook types.
describeLifecycleHookTypesResponse_lifecycleHookTypes :: Lens.Lens' DescribeLifecycleHookTypesResponse (Core.Maybe [Core.Text])
describeLifecycleHookTypesResponse_lifecycleHookTypes = Lens.lens (\DescribeLifecycleHookTypesResponse' {lifecycleHookTypes} -> lifecycleHookTypes) (\s@DescribeLifecycleHookTypesResponse' {} a -> s {lifecycleHookTypes = a} :: DescribeLifecycleHookTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLifecycleHookTypesResponse_httpStatus :: Lens.Lens' DescribeLifecycleHookTypesResponse Core.Int
describeLifecycleHookTypesResponse_httpStatus = Lens.lens (\DescribeLifecycleHookTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeLifecycleHookTypesResponse' {} a -> s {httpStatus = a} :: DescribeLifecycleHookTypesResponse)

instance
  Core.NFData
    DescribeLifecycleHookTypesResponse
