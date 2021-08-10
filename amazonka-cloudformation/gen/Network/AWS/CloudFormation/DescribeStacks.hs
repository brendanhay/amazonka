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
-- Module      : Network.AWS.CloudFormation.DescribeStacks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description for the specified stack; if no stack name was
-- specified, then it returns the description for all the stacks created.
--
-- If the stack does not exist, an @AmazonCloudFormationException@ is
-- returned.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStacks
  ( -- * Creating a Request
    DescribeStacks (..),
    newDescribeStacks,

    -- * Request Lenses
    describeStacks_nextToken,
    describeStacks_stackName,

    -- * Destructuring the Response
    DescribeStacksResponse (..),
    newDescribeStacksResponse,

    -- * Response Lenses
    describeStacksResponse_nextToken,
    describeStacksResponse_stacks,
    describeStacksResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for DescribeStacks action.
--
-- /See:/ 'newDescribeStacks' smart constructor.
data DescribeStacks = DescribeStacks'
  { -- | A string that identifies the next page of stacks that you want to
    -- retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique stack ID that is associated with the stack, which
    -- are not always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    stackName :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'describeStacks_nextToken' - A string that identifies the next page of stacks that you want to
-- retrieve.
--
-- 'stackName', 'describeStacks_stackName' - The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
newDescribeStacks ::
  DescribeStacks
newDescribeStacks =
  DescribeStacks'
    { nextToken = Prelude.Nothing,
      stackName = Prelude.Nothing
    }

-- | A string that identifies the next page of stacks that you want to
-- retrieve.
describeStacks_nextToken :: Lens.Lens' DescribeStacks (Prelude.Maybe Prelude.Text)
describeStacks_nextToken = Lens.lens (\DescribeStacks' {nextToken} -> nextToken) (\s@DescribeStacks' {} a -> s {nextToken = a} :: DescribeStacks)

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
describeStacks_stackName :: Lens.Lens' DescribeStacks (Prelude.Maybe Prelude.Text)
describeStacks_stackName = Lens.lens (\DescribeStacks' {stackName} -> stackName) (\s@DescribeStacks' {} a -> s {stackName = a} :: DescribeStacks)

instance Core.AWSPager DescribeStacks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStacksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStacksResponse_stacks Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeStacks_nextToken
          Lens..~ rs
          Lens.^? describeStacksResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeStacks where
  type
    AWSResponse DescribeStacks =
      DescribeStacksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStacksResult"
      ( \s h x ->
          DescribeStacksResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "Stacks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStacks

instance Prelude.NFData DescribeStacks

instance Core.ToHeaders DescribeStacks where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeStacks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStacks where
  toQuery DescribeStacks' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeStacks" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "StackName" Core.=: stackName
      ]

-- | The output for a DescribeStacks action.
--
-- /See:/ 'newDescribeStacksResponse' smart constructor.
data DescribeStacksResponse = DescribeStacksResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next
    -- page of stacks. If no additional page exists, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of stack structures.
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
-- 'nextToken', 'describeStacksResponse_nextToken' - If the output exceeds 1 MB in size, a string that identifies the next
-- page of stacks. If no additional page exists, this value is null.
--
-- 'stacks', 'describeStacksResponse_stacks' - A list of stack structures.
--
-- 'httpStatus', 'describeStacksResponse_httpStatus' - The response's http status code.
newDescribeStacksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStacksResponse
newDescribeStacksResponse pHttpStatus_ =
  DescribeStacksResponse'
    { nextToken =
        Prelude.Nothing,
      stacks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next
-- page of stacks. If no additional page exists, this value is null.
describeStacksResponse_nextToken :: Lens.Lens' DescribeStacksResponse (Prelude.Maybe Prelude.Text)
describeStacksResponse_nextToken = Lens.lens (\DescribeStacksResponse' {nextToken} -> nextToken) (\s@DescribeStacksResponse' {} a -> s {nextToken = a} :: DescribeStacksResponse)

-- | A list of stack structures.
describeStacksResponse_stacks :: Lens.Lens' DescribeStacksResponse (Prelude.Maybe [Stack])
describeStacksResponse_stacks = Lens.lens (\DescribeStacksResponse' {stacks} -> stacks) (\s@DescribeStacksResponse' {} a -> s {stacks = a} :: DescribeStacksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStacksResponse_httpStatus :: Lens.Lens' DescribeStacksResponse Prelude.Int
describeStacksResponse_httpStatus = Lens.lens (\DescribeStacksResponse' {httpStatus} -> httpStatus) (\s@DescribeStacksResponse' {} a -> s {httpStatus = a} :: DescribeStacksResponse)

instance Prelude.NFData DescribeStacksResponse
