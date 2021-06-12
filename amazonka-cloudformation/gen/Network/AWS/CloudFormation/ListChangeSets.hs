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
-- Module      : Network.AWS.CloudFormation.ListChangeSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the ID and status of each active change set for a stack. For
-- example, AWS CloudFormation lists change sets that are in the
-- @CREATE_IN_PROGRESS@ or @CREATE_PENDING@ state.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListChangeSets
  ( -- * Creating a Request
    ListChangeSets (..),
    newListChangeSets,

    -- * Request Lenses
    listChangeSets_nextToken,
    listChangeSets_stackName,

    -- * Destructuring the Response
    ListChangeSetsResponse (..),
    newListChangeSetsResponse,

    -- * Response Lenses
    listChangeSetsResponse_nextToken,
    listChangeSetsResponse_summaries,
    listChangeSetsResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListChangeSets action.
--
-- /See:/ 'newListChangeSets' smart constructor.
data ListChangeSets = ListChangeSets'
  { -- | A string (provided by the ListChangeSets response output) that
    -- identifies the next page of change sets that you want to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | The name or the Amazon Resource Name (ARN) of the stack for which you
    -- want to list change sets.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListChangeSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChangeSets_nextToken' - A string (provided by the ListChangeSets response output) that
-- identifies the next page of change sets that you want to retrieve.
--
-- 'stackName', 'listChangeSets_stackName' - The name or the Amazon Resource Name (ARN) of the stack for which you
-- want to list change sets.
newListChangeSets ::
  -- | 'stackName'
  Core.Text ->
  ListChangeSets
newListChangeSets pStackName_ =
  ListChangeSets'
    { nextToken = Core.Nothing,
      stackName = pStackName_
    }

-- | A string (provided by the ListChangeSets response output) that
-- identifies the next page of change sets that you want to retrieve.
listChangeSets_nextToken :: Lens.Lens' ListChangeSets (Core.Maybe Core.Text)
listChangeSets_nextToken = Lens.lens (\ListChangeSets' {nextToken} -> nextToken) (\s@ListChangeSets' {} a -> s {nextToken = a} :: ListChangeSets)

-- | The name or the Amazon Resource Name (ARN) of the stack for which you
-- want to list change sets.
listChangeSets_stackName :: Lens.Lens' ListChangeSets Core.Text
listChangeSets_stackName = Lens.lens (\ListChangeSets' {stackName} -> stackName) (\s@ListChangeSets' {} a -> s {stackName = a} :: ListChangeSets)

instance Core.AWSPager ListChangeSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listChangeSetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listChangeSetsResponse_summaries Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listChangeSets_nextToken
          Lens..~ rs
          Lens.^? listChangeSetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListChangeSets where
  type
    AWSResponse ListChangeSets =
      ListChangeSetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListChangeSetsResult"
      ( \s h x ->
          ListChangeSetsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "Summaries" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListChangeSets

instance Core.NFData ListChangeSets

instance Core.ToHeaders ListChangeSets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListChangeSets where
  toPath = Core.const "/"

instance Core.ToQuery ListChangeSets where
  toQuery ListChangeSets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListChangeSets" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "StackName" Core.=: stackName
      ]

-- | The output for the ListChangeSets action.
--
-- /See:/ 'newListChangeSetsResponse' smart constructor.
data ListChangeSetsResponse = ListChangeSetsResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of
    -- change sets. If there is no additional page, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @ChangeSetSummary@ structures that provides the ID and status
    -- of each change set for the specified stack.
    summaries :: Core.Maybe [ChangeSetSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListChangeSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChangeSetsResponse_nextToken' - If the output exceeds 1 MB, a string that identifies the next page of
-- change sets. If there is no additional page, this value is null.
--
-- 'summaries', 'listChangeSetsResponse_summaries' - A list of @ChangeSetSummary@ structures that provides the ID and status
-- of each change set for the specified stack.
--
-- 'httpStatus', 'listChangeSetsResponse_httpStatus' - The response's http status code.
newListChangeSetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListChangeSetsResponse
newListChangeSetsResponse pHttpStatus_ =
  ListChangeSetsResponse'
    { nextToken = Core.Nothing,
      summaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- change sets. If there is no additional page, this value is null.
listChangeSetsResponse_nextToken :: Lens.Lens' ListChangeSetsResponse (Core.Maybe Core.Text)
listChangeSetsResponse_nextToken = Lens.lens (\ListChangeSetsResponse' {nextToken} -> nextToken) (\s@ListChangeSetsResponse' {} a -> s {nextToken = a} :: ListChangeSetsResponse)

-- | A list of @ChangeSetSummary@ structures that provides the ID and status
-- of each change set for the specified stack.
listChangeSetsResponse_summaries :: Lens.Lens' ListChangeSetsResponse (Core.Maybe [ChangeSetSummary])
listChangeSetsResponse_summaries = Lens.lens (\ListChangeSetsResponse' {summaries} -> summaries) (\s@ListChangeSetsResponse' {} a -> s {summaries = a} :: ListChangeSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listChangeSetsResponse_httpStatus :: Lens.Lens' ListChangeSetsResponse Core.Int
listChangeSetsResponse_httpStatus = Lens.lens (\ListChangeSetsResponse' {httpStatus} -> httpStatus) (\s@ListChangeSetsResponse' {} a -> s {httpStatus = a} :: ListChangeSetsResponse)

instance Core.NFData ListChangeSetsResponse
