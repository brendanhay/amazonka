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
-- Module      : Network.AWS.ELBv2.DescribeTargetHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the health of the specified targets or all of your targets.
module Network.AWS.ELBv2.DescribeTargetHealth
  ( -- * Creating a Request
    DescribeTargetHealth (..),
    newDescribeTargetHealth,

    -- * Request Lenses
    describeTargetHealth_targets,
    describeTargetHealth_targetGroupArn,

    -- * Destructuring the Response
    DescribeTargetHealthResponse (..),
    newDescribeTargetHealthResponse,

    -- * Response Lenses
    describeTargetHealthResponse_targetHealthDescriptions,
    describeTargetHealthResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTargetHealth' smart constructor.
data DescribeTargetHealth = DescribeTargetHealth'
  { -- | The targets.
    targets :: Core.Maybe [TargetDescription],
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTargetHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targets', 'describeTargetHealth_targets' - The targets.
--
-- 'targetGroupArn', 'describeTargetHealth_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
newDescribeTargetHealth ::
  -- | 'targetGroupArn'
  Core.Text ->
  DescribeTargetHealth
newDescribeTargetHealth pTargetGroupArn_ =
  DescribeTargetHealth'
    { targets = Core.Nothing,
      targetGroupArn = pTargetGroupArn_
    }

-- | The targets.
describeTargetHealth_targets :: Lens.Lens' DescribeTargetHealth (Core.Maybe [TargetDescription])
describeTargetHealth_targets = Lens.lens (\DescribeTargetHealth' {targets} -> targets) (\s@DescribeTargetHealth' {} a -> s {targets = a} :: DescribeTargetHealth) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the target group.
describeTargetHealth_targetGroupArn :: Lens.Lens' DescribeTargetHealth Core.Text
describeTargetHealth_targetGroupArn = Lens.lens (\DescribeTargetHealth' {targetGroupArn} -> targetGroupArn) (\s@DescribeTargetHealth' {} a -> s {targetGroupArn = a} :: DescribeTargetHealth)

instance Core.AWSRequest DescribeTargetHealth where
  type
    AWSResponse DescribeTargetHealth =
      DescribeTargetHealthResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTargetHealthResult"
      ( \s h x ->
          DescribeTargetHealthResponse'
            Core.<$> ( x Core..@? "TargetHealthDescriptions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTargetHealth

instance Core.NFData DescribeTargetHealth

instance Core.ToHeaders DescribeTargetHealth where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTargetHealth where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTargetHealth where
  toQuery DescribeTargetHealth' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeTargetHealth" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "Targets"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> targets),
        "TargetGroupArn" Core.=: targetGroupArn
      ]

-- | /See:/ 'newDescribeTargetHealthResponse' smart constructor.
data DescribeTargetHealthResponse = DescribeTargetHealthResponse'
  { -- | Information about the health of the targets.
    targetHealthDescriptions :: Core.Maybe [TargetHealthDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTargetHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetHealthDescriptions', 'describeTargetHealthResponse_targetHealthDescriptions' - Information about the health of the targets.
--
-- 'httpStatus', 'describeTargetHealthResponse_httpStatus' - The response's http status code.
newDescribeTargetHealthResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTargetHealthResponse
newDescribeTargetHealthResponse pHttpStatus_ =
  DescribeTargetHealthResponse'
    { targetHealthDescriptions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the health of the targets.
describeTargetHealthResponse_targetHealthDescriptions :: Lens.Lens' DescribeTargetHealthResponse (Core.Maybe [TargetHealthDescription])
describeTargetHealthResponse_targetHealthDescriptions = Lens.lens (\DescribeTargetHealthResponse' {targetHealthDescriptions} -> targetHealthDescriptions) (\s@DescribeTargetHealthResponse' {} a -> s {targetHealthDescriptions = a} :: DescribeTargetHealthResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTargetHealthResponse_httpStatus :: Lens.Lens' DescribeTargetHealthResponse Core.Int
describeTargetHealthResponse_httpStatus = Lens.lens (\DescribeTargetHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeTargetHealthResponse' {} a -> s {httpStatus = a} :: DescribeTargetHealthResponse)

instance Core.NFData DescribeTargetHealthResponse
