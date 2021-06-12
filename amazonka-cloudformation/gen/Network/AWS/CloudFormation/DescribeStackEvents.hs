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
-- Module      : Network.AWS.CloudFormation.DescribeStackEvents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all stack related events for a specified stack in reverse
-- chronological order. For more information about a stack\'s event
-- history, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html Stacks>
-- in the AWS CloudFormation User Guide.
--
-- You can list events for stacks that have failed to create or have been
-- deleted by specifying the unique stack identifier (stack ID).
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.DescribeStackEvents
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

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for DescribeStackEvents action.
--
-- /See:/ 'newDescribeStackEvents' smart constructor.
data DescribeStackEvents = DescribeStackEvents'
  { -- | A string that identifies the next page of events that you want to
    -- retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | The name or the unique stack ID that is associated with the stack, which
    -- are not always interchangeable:
    --
    -- -   Running stacks: You can specify either the stack\'s name or its
    --     unique stack ID.
    --
    -- -   Deleted stacks: You must specify the unique stack ID.
    --
    -- Default: There is no default value.
    stackName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'stackName', 'describeStackEvents_stackName' - The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
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
    { nextToken = Core.Nothing,
      stackName = Core.Nothing
    }

-- | A string that identifies the next page of events that you want to
-- retrieve.
describeStackEvents_nextToken :: Lens.Lens' DescribeStackEvents (Core.Maybe Core.Text)
describeStackEvents_nextToken = Lens.lens (\DescribeStackEvents' {nextToken} -> nextToken) (\s@DescribeStackEvents' {} a -> s {nextToken = a} :: DescribeStackEvents)

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
describeStackEvents_stackName :: Lens.Lens' DescribeStackEvents (Core.Maybe Core.Text)
describeStackEvents_stackName = Lens.lens (\DescribeStackEvents' {stackName} -> stackName) (\s@DescribeStackEvents' {} a -> s {stackName = a} :: DescribeStackEvents)

instance Core.AWSPager DescribeStackEvents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeStackEventsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeStackEventsResponse_stackEvents
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeStackEvents_nextToken
          Lens..~ rs
          Lens.^? describeStackEventsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeStackEvents where
  type
    AWSResponse DescribeStackEvents =
      DescribeStackEventsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStackEventsResult"
      ( \s h x ->
          DescribeStackEventsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "StackEvents" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStackEvents

instance Core.NFData DescribeStackEvents

instance Core.ToHeaders DescribeStackEvents where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeStackEvents where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStackEvents where
  toQuery DescribeStackEvents' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeStackEvents" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "StackName" Core.=: stackName
      ]

-- | The output for a DescribeStackEvents action.
--
-- /See:/ 'newDescribeStackEventsResponse' smart constructor.
data DescribeStackEventsResponse = DescribeStackEventsResponse'
  { -- | If the output exceeds 1 MB in size, a string that identifies the next
    -- page of events. If no additional page exists, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @StackEvents@ structures.
    stackEvents :: Core.Maybe [StackEvent],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeStackEventsResponse
newDescribeStackEventsResponse pHttpStatus_ =
  DescribeStackEventsResponse'
    { nextToken =
        Core.Nothing,
      stackEvents = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB in size, a string that identifies the next
-- page of events. If no additional page exists, this value is null.
describeStackEventsResponse_nextToken :: Lens.Lens' DescribeStackEventsResponse (Core.Maybe Core.Text)
describeStackEventsResponse_nextToken = Lens.lens (\DescribeStackEventsResponse' {nextToken} -> nextToken) (\s@DescribeStackEventsResponse' {} a -> s {nextToken = a} :: DescribeStackEventsResponse)

-- | A list of @StackEvents@ structures.
describeStackEventsResponse_stackEvents :: Lens.Lens' DescribeStackEventsResponse (Core.Maybe [StackEvent])
describeStackEventsResponse_stackEvents = Lens.lens (\DescribeStackEventsResponse' {stackEvents} -> stackEvents) (\s@DescribeStackEventsResponse' {} a -> s {stackEvents = a} :: DescribeStackEventsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeStackEventsResponse_httpStatus :: Lens.Lens' DescribeStackEventsResponse Core.Int
describeStackEventsResponse_httpStatus = Lens.lens (\DescribeStackEventsResponse' {httpStatus} -> httpStatus) (\s@DescribeStackEventsResponse' {} a -> s {httpStatus = a} :: DescribeStackEventsResponse)

instance Core.NFData DescribeStackEventsResponse
