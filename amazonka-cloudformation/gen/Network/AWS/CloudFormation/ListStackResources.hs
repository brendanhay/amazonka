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
-- Module      : Network.AWS.CloudFormation.ListStackResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of all resources of the specified stack.
--
-- For deleted stacks, ListStackResources returns resource information for
-- up to 90 days after the stack has been deleted.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackResources
  ( -- * Creating a Request
    ListStackResources (..),
    newListStackResources,

    -- * Request Lenses
    listStackResources_nextToken,
    listStackResources_stackName,

    -- * Destructuring the Response
    ListStackResourcesResponse (..),
    newListStackResourcesResponse,

    -- * Response Lenses
    listStackResourcesResponse_nextToken,
    listStackResourcesResponse_stackResourceSummaries,
    listStackResourcesResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListStackResource action.
--
-- /See:/ 'newListStackResources' smart constructor.
data ListStackResources = ListStackResources'
  { -- | A string that identifies the next page of stack resources that you want
    -- to retrieve.
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
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStackResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackResources_nextToken' - A string that identifies the next page of stack resources that you want
-- to retrieve.
--
-- 'stackName', 'listStackResources_stackName' - The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
newListStackResources ::
  -- | 'stackName'
  Core.Text ->
  ListStackResources
newListStackResources pStackName_ =
  ListStackResources'
    { nextToken = Core.Nothing,
      stackName = pStackName_
    }

-- | A string that identifies the next page of stack resources that you want
-- to retrieve.
listStackResources_nextToken :: Lens.Lens' ListStackResources (Core.Maybe Core.Text)
listStackResources_nextToken = Lens.lens (\ListStackResources' {nextToken} -> nextToken) (\s@ListStackResources' {} a -> s {nextToken = a} :: ListStackResources)

-- | The name or the unique stack ID that is associated with the stack, which
-- are not always interchangeable:
--
-- -   Running stacks: You can specify either the stack\'s name or its
--     unique stack ID.
--
-- -   Deleted stacks: You must specify the unique stack ID.
--
-- Default: There is no default value.
listStackResources_stackName :: Lens.Lens' ListStackResources Core.Text
listStackResources_stackName = Lens.lens (\ListStackResources' {stackName} -> stackName) (\s@ListStackResources' {} a -> s {stackName = a} :: ListStackResources)

instance Core.AWSPager ListStackResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStackResourcesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listStackResourcesResponse_stackResourceSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listStackResources_nextToken
          Lens..~ rs
          Lens.^? listStackResourcesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListStackResources where
  type
    AWSResponse ListStackResources =
      ListStackResourcesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListStackResourcesResult"
      ( \s h x ->
          ListStackResourcesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> ( x Core..@? "StackResourceSummaries"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStackResources

instance Core.NFData ListStackResources

instance Core.ToHeaders ListStackResources where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListStackResources where
  toPath = Core.const "/"

instance Core.ToQuery ListStackResources where
  toQuery ListStackResources' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListStackResources" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "StackName" Core.=: stackName
      ]

-- | The output for a ListStackResources action.
--
-- /See:/ 'newListStackResourcesResponse' smart constructor.
data ListStackResourcesResponse = ListStackResourcesResponse'
  { -- | If the output exceeds 1 MB, a string that identifies the next page of
    -- stack resources. If no additional page exists, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @StackResourceSummary@ structures.
    stackResourceSummaries :: Core.Maybe [StackResourceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStackResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStackResourcesResponse_nextToken' - If the output exceeds 1 MB, a string that identifies the next page of
-- stack resources. If no additional page exists, this value is null.
--
-- 'stackResourceSummaries', 'listStackResourcesResponse_stackResourceSummaries' - A list of @StackResourceSummary@ structures.
--
-- 'httpStatus', 'listStackResourcesResponse_httpStatus' - The response's http status code.
newListStackResourcesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStackResourcesResponse
newListStackResourcesResponse pHttpStatus_ =
  ListStackResourcesResponse'
    { nextToken =
        Core.Nothing,
      stackResourceSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the output exceeds 1 MB, a string that identifies the next page of
-- stack resources. If no additional page exists, this value is null.
listStackResourcesResponse_nextToken :: Lens.Lens' ListStackResourcesResponse (Core.Maybe Core.Text)
listStackResourcesResponse_nextToken = Lens.lens (\ListStackResourcesResponse' {nextToken} -> nextToken) (\s@ListStackResourcesResponse' {} a -> s {nextToken = a} :: ListStackResourcesResponse)

-- | A list of @StackResourceSummary@ structures.
listStackResourcesResponse_stackResourceSummaries :: Lens.Lens' ListStackResourcesResponse (Core.Maybe [StackResourceSummary])
listStackResourcesResponse_stackResourceSummaries = Lens.lens (\ListStackResourcesResponse' {stackResourceSummaries} -> stackResourceSummaries) (\s@ListStackResourcesResponse' {} a -> s {stackResourceSummaries = a} :: ListStackResourcesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStackResourcesResponse_httpStatus :: Lens.Lens' ListStackResourcesResponse Core.Int
listStackResourcesResponse_httpStatus = Lens.lens (\ListStackResourcesResponse' {httpStatus} -> httpStatus) (\s@ListStackResourcesResponse' {} a -> s {httpStatus = a} :: ListStackResourcesResponse)

instance Core.NFData ListStackResourcesResponse
