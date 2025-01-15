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
-- Module      : Amazonka.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all stack related events for a specified stack in reverse
-- chronological order. For more information about a stack\'s event
-- history, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html Stacks>
-- in the CloudFormation User Guide.
--
-- You can list events for stacks that have failed to create or have been
-- deleted by specifying the unique stack identifier (stack ID).
--
-- This operation returns paginated results.
module Amazonka.CloudFormation.DescribeStackEvents
  ( -- * Creating a Request
    DescribeStackEvents (..),
    newDescribeStackEvents,

    -- * Request Lenses
    describeStackEvents_nextToken,
    describeStackEvents_stackName,

    -- * Destructuring the Response
    DescribeStackEventsResponse (..),
    newDescribeStackEventsResponse,

    -- * Response Lenses
    describeStackEventsResponse_nextToken,
    describeStackEventsResponse_stackEvents,
    describeStackEventsResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for DescribeStackEvents action.
--
-- /See:/ 'newDescribeStackEvents' smart constructor.
data DescribeStackEvents = DescribeStackEvents'
  { -- | A string that identifies the next page of events that you want to
    -- retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name or the unique stack ID that\'s associated with the stack, which
    -- aren\'t always interchangeable:
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
-- Create a value of 'DescribeStackEvents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStackEvents_nextToken' - A string that identifies the next page of events that you want to
-- retrieve.
--
-- 'stackName', 'describeStackEvents_stackName' - The name or the unique stack ID that\'s associated with the stack, which
-- aren\'t always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
newDescribeStackEvents ::
  DescribeStackEvents
newDescribeStackEvents =
  DescribeStackEvents'
    { nextToken = Prelude.Nothing,
      stackName = Prelude.Nothing
    }

-- | A string that identifies the next page of events that you want to
-- retrieve.
describeStackEvents_nextToken :: Lens.Lens' DescribeStackEvents (Prelude.Maybe Prelude.Text)
describeStackEvents_nextToken = Lens.lens (\DescribeStackEvents' {nextToken} -> nextToken) (\s@DescribeStackEvents' {} a -> s {nextToken = a} :: DescribeStackEvents)

-- | The name or the unique stack ID that\'s associated with the stack, which
-- aren\'t always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
describeStackEvents_stackName :: Lens.Lens' DescribeStackEvents (Prelude.Maybe Prelude.Text)
describeStackEvents_stackName = Lens.lens (\DescribeStackEvents' {stackName} -> stackName) (\s@DescribeStackEvents' {} a -> s {stackName = a} :: DescribeStackEvents)

instance Core.AWSPager DescribeStackEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStackEventsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStackEventsResponse_stackEvents
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeStackEvents_nextToken
              Lens..~ rs
              Lens.^? describeStackEventsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeStackEvents where
  type
    AWSResponse DescribeStackEvents =
      DescribeStackEventsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeStackEventsResult"
      ( \s h x ->
          DescribeStackEventsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "StackEvents" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStackEvents where
  hashWithSalt _salt DescribeStackEvents' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData DescribeStackEvents where
  rnf DescribeStackEvents' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf stackName

instance Data.ToHeaders DescribeStackEvents where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStackEvents where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStackEvents where
  toQuery DescribeStackEvents' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeStackEvents" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "StackName" Data.=: stackName
      ]

-- | The output for a DescribeStackEvents action.
--
-- /See:/ 'newDescribeStackEventsResponse' smart constructor.
data DescribeStackEventsResponse = DescribeStackEventsResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next
    -- page of events. If no additional page exists, this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @StackEvents@ structures.
    stackEvents :: Prelude.Maybe [StackEvent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStackEventsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStackEventsResponse_nextToken' - If the output exceeds 1 MB in size, a string that identifies the next
-- page of events. If no additional page exists, this value is null.
--
-- 'stackEvents', 'describeStackEventsResponse_stackEvents' - A list of @StackEvents@ structures.
--
-- 'httpStatus', 'describeStackEventsResponse_httpStatus' - The response's http status code.
newDescribeStackEventsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStackEventsResponse
newDescribeStackEventsResponse pHttpStatus_ =
  DescribeStackEventsResponse'
    { nextToken =
        Prelude.Nothing,
      stackEvents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next
-- page of events. If no additional page exists, this value is null.
describeStackEventsResponse_nextToken :: Lens.Lens' DescribeStackEventsResponse (Prelude.Maybe Prelude.Text)
describeStackEventsResponse_nextToken = Lens.lens (\DescribeStackEventsResponse' {nextToken} -> nextToken) (\s@DescribeStackEventsResponse' {} a -> s {nextToken = a} :: DescribeStackEventsResponse)

-- | A list of @StackEvents@ structures.
describeStackEventsResponse_stackEvents :: Lens.Lens' DescribeStackEventsResponse (Prelude.Maybe [StackEvent])
describeStackEventsResponse_stackEvents = Lens.lens (\DescribeStackEventsResponse' {stackEvents} -> stackEvents) (\s@DescribeStackEventsResponse' {} a -> s {stackEvents = a} :: DescribeStackEventsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeStackEventsResponse_httpStatus :: Lens.Lens' DescribeStackEventsResponse Prelude.Int
describeStackEventsResponse_httpStatus = Lens.lens (\DescribeStackEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeStackEventsResponse' {} a -> s {httpStatus = a} :: DescribeStackEventsResponse)

instance Prelude.NFData DescribeStackEventsResponse where
  rnf DescribeStackEventsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf stackEvents `Prelude.seq`
        Prelude.rnf httpStatus
