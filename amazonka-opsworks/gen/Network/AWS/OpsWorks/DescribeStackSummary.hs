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
-- Module      : Network.AWS.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the number of layers and apps in a specified stack, and the
-- number of instances in each state, such as @running_setup@ or @online@.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeStackSummary
  ( -- * Creating a Request
    DescribeStackSummary (..),
    newDescribeStackSummary,

    -- * Request Lenses
    describeStackSummary_stackId,

    -- * Destructuring the Response
    DescribeStackSummaryResponse (..),
    newDescribeStackSummaryResponse,

    -- * Response Lenses
    describeStackSummaryResponse_stackSummary,
    describeStackSummaryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStackSummary' smart constructor.
data DescribeStackSummary = DescribeStackSummary'
  { -- | The stack ID.
    stackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeStackSummary_stackId' - The stack ID.
newDescribeStackSummary ::
  -- | 'stackId'
  Prelude.Text ->
  DescribeStackSummary
newDescribeStackSummary pStackId_ =
  DescribeStackSummary' {stackId = pStackId_}

-- | The stack ID.
describeStackSummary_stackId :: Lens.Lens' DescribeStackSummary Prelude.Text
describeStackSummary_stackId = Lens.lens (\DescribeStackSummary' {stackId} -> stackId) (\s@DescribeStackSummary' {} a -> s {stackId = a} :: DescribeStackSummary)

instance Core.AWSRequest DescribeStackSummary where
  type
    AWSResponse DescribeStackSummary =
      DescribeStackSummaryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStackSummaryResponse'
            Prelude.<$> (x Core..?> "StackSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackSummary

instance Prelude.NFData DescribeStackSummary

instance Core.ToHeaders DescribeStackSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeStackSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeStackSummary where
  toJSON DescribeStackSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Core..= stackId)]
      )

instance Core.ToPath DescribeStackSummary where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStackSummary where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeStackSummary@ request.
--
-- /See:/ 'newDescribeStackSummaryResponse' smart constructor.
data DescribeStackSummaryResponse = DescribeStackSummaryResponse'
  { -- | A @StackSummary@ object that contains the results.
    stackSummary :: Prelude.Maybe StackSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSummary', 'describeStackSummaryResponse_stackSummary' - A @StackSummary@ object that contains the results.
--
-- 'httpStatus', 'describeStackSummaryResponse_httpStatus' - The response's http status code.
newDescribeStackSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackSummaryResponse
newDescribeStackSummaryResponse pHttpStatus_ =
  DescribeStackSummaryResponse'
    { stackSummary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A @StackSummary@ object that contains the results.
describeStackSummaryResponse_stackSummary :: Lens.Lens' DescribeStackSummaryResponse (Prelude.Maybe StackSummary)
describeStackSummaryResponse_stackSummary = Lens.lens (\DescribeStackSummaryResponse' {stackSummary} -> stackSummary) (\s@DescribeStackSummaryResponse' {} a -> s {stackSummary = a} :: DescribeStackSummaryResponse)

-- | The response's http status code.
describeStackSummaryResponse_httpStatus :: Lens.Lens' DescribeStackSummaryResponse Prelude.Int
describeStackSummaryResponse_httpStatus = Lens.lens (\DescribeStackSummaryResponse' {httpStatus} -> httpStatus) (\s@DescribeStackSummaryResponse' {} a -> s {httpStatus = a} :: DescribeStackSummaryResponse)

instance Prelude.NFData DescribeStackSummaryResponse
