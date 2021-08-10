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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTargetHealth' smart constructor.
data DescribeTargetHealth = DescribeTargetHealth'
  { -- | The targets.
    targets :: Prelude.Maybe [TargetDescription],
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeTargetHealth
newDescribeTargetHealth pTargetGroupArn_ =
  DescribeTargetHealth'
    { targets = Prelude.Nothing,
      targetGroupArn = pTargetGroupArn_
    }

-- | The targets.
describeTargetHealth_targets :: Lens.Lens' DescribeTargetHealth (Prelude.Maybe [TargetDescription])
describeTargetHealth_targets = Lens.lens (\DescribeTargetHealth' {targets} -> targets) (\s@DescribeTargetHealth' {} a -> s {targets = a} :: DescribeTargetHealth) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the target group.
describeTargetHealth_targetGroupArn :: Lens.Lens' DescribeTargetHealth Prelude.Text
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
            Prelude.<$> ( x Core..@? "TargetHealthDescriptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTargetHealth

instance Prelude.NFData DescribeTargetHealth

instance Core.ToHeaders DescribeTargetHealth where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTargetHealth where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTargetHealth where
  toQuery DescribeTargetHealth' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeTargetHealth" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "Targets"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> targets),
        "TargetGroupArn" Core.=: targetGroupArn
      ]

-- | /See:/ 'newDescribeTargetHealthResponse' smart constructor.
data DescribeTargetHealthResponse = DescribeTargetHealthResponse'
  { -- | Information about the health of the targets.
    targetHealthDescriptions :: Prelude.Maybe [TargetHealthDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTargetHealthResponse
newDescribeTargetHealthResponse pHttpStatus_ =
  DescribeTargetHealthResponse'
    { targetHealthDescriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the health of the targets.
describeTargetHealthResponse_targetHealthDescriptions :: Lens.Lens' DescribeTargetHealthResponse (Prelude.Maybe [TargetHealthDescription])
describeTargetHealthResponse_targetHealthDescriptions = Lens.lens (\DescribeTargetHealthResponse' {targetHealthDescriptions} -> targetHealthDescriptions) (\s@DescribeTargetHealthResponse' {} a -> s {targetHealthDescriptions = a} :: DescribeTargetHealthResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTargetHealthResponse_httpStatus :: Lens.Lens' DescribeTargetHealthResponse Prelude.Int
describeTargetHealthResponse_httpStatus = Lens.lens (\DescribeTargetHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeTargetHealthResponse' {} a -> s {httpStatus = a} :: DescribeTargetHealthResponse)

instance Prelude.NFData DescribeTargetHealthResponse
