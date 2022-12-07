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
-- Module      : Amazonka.OpsWorks.DescribeStackSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorks.DescribeStackSummary
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStackSummaryResponse'
            Prelude.<$> (x Data..?> "StackSummary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackSummary where
  hashWithSalt _salt DescribeStackSummary' {..} =
    _salt `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeStackSummary where
  rnf DescribeStackSummary' {..} = Prelude.rnf stackId

instance Data.ToHeaders DescribeStackSummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeStackSummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStackSummary where
  toJSON DescribeStackSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StackId" Data..= stackId)]
      )

instance Data.ToPath DescribeStackSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStackSummary where
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

instance Prelude.NFData DescribeStackSummaryResponse where
  rnf DescribeStackSummaryResponse' {..} =
    Prelude.rnf stackSummary
      `Prelude.seq` Prelude.rnf httpStatus
