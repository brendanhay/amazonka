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
-- Module      : Network.AWS.OpsWorks.DescribeStacks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of one or more stacks.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeStacks
  ( -- * Creating a Request
    DescribeStacks (..),
    newDescribeStacks,

    -- * Request Lenses
    describeStacks_stackIds,

    -- * Destructuring the Response
    DescribeStacksResponse (..),
    newDescribeStacksResponse,

    -- * Response Lenses
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | An array of stack IDs that specify the stacks to be described. If you
    -- omit this parameter, @DescribeStacks@ returns a description of every
    -- stack.
    stackIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStacks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackIds', 'describeStacks_stackIds' - An array of stack IDs that specify the stacks to be described. If you
-- omit this parameter, @DescribeStacks@ returns a description of every
-- stack.
newDescribeStacks ::
  DescribeStacks
newDescribeStacks =
  DescribeStacks' {stackIds = Prelude.Nothing}

-- | An array of stack IDs that specify the stacks to be described. If you
-- omit this parameter, @DescribeStacks@ returns a description of every
-- stack.
describeStacks_stackIds :: Lens.Lens' DescribeStacks (Prelude.Maybe [Prelude.Text])
describeStacks_stackIds = Lens.lens (\DescribeStacks' {stackIds} -> stackIds) (\s@DescribeStacks' {} a -> s {stackIds = a} :: DescribeStacks) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeStacks where
  type
    AWSResponse DescribeStacks =
      DescribeStacksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStacksResponse'
            Prelude.<$> (x Core..?> "Stacks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStacks

instance Prelude.NFData DescribeStacks

instance Core.ToHeaders DescribeStacks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeStacks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeStacks where
  toJSON DescribeStacks' {..} =
    Core.object
      ( Prelude.catMaybes
          [("StackIds" Core..=) Prelude.<$> stackIds]
      )

instance Core.ToPath DescribeStacks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStacks where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeStacks@ request.
--
-- /See:/ 'newDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | An array of @Stack@ objects that describe the stacks.
    stacks :: Prelude.Maybe [Stack],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStacksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stacks', 'describeStacksResponse_stacks' - An array of @Stack@ objects that describe the stacks.
--
-- 'httpStatus', 'describeStacksResponse_httpStatus' - The response's http status code.
newDescribeStacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStacksResponse
newDescribeStacksResponse pHttpStatus_ =
  DescribeStacksResponse'
    { stacks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Stack@ objects that describe the stacks.
describeStacksResponse_stacks :: Lens.Lens' DescribeStacksResponse (Prelude.Maybe [Stack])
describeStacksResponse_stacks = Lens.lens (\DescribeStacksResponse' {stacks} -> stacks) (\s@DescribeStacksResponse' {} a -> s {stacks = a} :: DescribeStacksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStacksResponse_httpStatus :: Lens.Lens' DescribeStacksResponse Prelude.Int
describeStacksResponse_httpStatus = Lens.lens (\DescribeStacksResponse' {httpStatus} -> httpStatus) (\s@DescribeStacksResponse' {} a -> s {httpStatus = a} :: DescribeStacksResponse)

instance Prelude.NFData DescribeStacksResponse
