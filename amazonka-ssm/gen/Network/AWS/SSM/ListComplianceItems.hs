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
-- Module      : Network.AWS.SSM.ListComplianceItems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a specified resource ID, this API action returns a list of
-- compliance statuses for different resource types. Currently, you can
-- only specify one resource ID per call. List results depend on the
-- criteria specified in the filter.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListComplianceItems
  ( -- * Creating a Request
    ListComplianceItems (..),
    newListComplianceItems,

    -- * Request Lenses
    listComplianceItems_nextToken,
    listComplianceItems_resourceTypes,
    listComplianceItems_maxResults,
    listComplianceItems_resourceIds,
    listComplianceItems_filters,

    -- * Destructuring the Response
    ListComplianceItemsResponse (..),
    newListComplianceItemsResponse,

    -- * Response Lenses
    listComplianceItemsResponse_nextToken,
    listComplianceItemsResponse_complianceItems,
    listComplianceItemsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListComplianceItems' smart constructor.
data ListComplianceItems = ListComplianceItems'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The type of resource from which to get compliance information.
    -- Currently, the only supported resource type is @ManagedInstance@.
    resourceTypes :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID for the resources from which to get compliance information.
    -- Currently, you can only specify one resource ID.
    resourceIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | One or more compliance filters. Use a filter to return a more specific
    -- list of results.
    filters :: Core.Maybe [ComplianceStringFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListComplianceItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComplianceItems_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'resourceTypes', 'listComplianceItems_resourceTypes' - The type of resource from which to get compliance information.
-- Currently, the only supported resource type is @ManagedInstance@.
--
-- 'maxResults', 'listComplianceItems_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'resourceIds', 'listComplianceItems_resourceIds' - The ID for the resources from which to get compliance information.
-- Currently, you can only specify one resource ID.
--
-- 'filters', 'listComplianceItems_filters' - One or more compliance filters. Use a filter to return a more specific
-- list of results.
newListComplianceItems ::
  ListComplianceItems
newListComplianceItems =
  ListComplianceItems'
    { nextToken = Core.Nothing,
      resourceTypes = Core.Nothing,
      maxResults = Core.Nothing,
      resourceIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listComplianceItems_nextToken :: Lens.Lens' ListComplianceItems (Core.Maybe Core.Text)
listComplianceItems_nextToken = Lens.lens (\ListComplianceItems' {nextToken} -> nextToken) (\s@ListComplianceItems' {} a -> s {nextToken = a} :: ListComplianceItems)

-- | The type of resource from which to get compliance information.
-- Currently, the only supported resource type is @ManagedInstance@.
listComplianceItems_resourceTypes :: Lens.Lens' ListComplianceItems (Core.Maybe (Core.NonEmpty Core.Text))
listComplianceItems_resourceTypes = Lens.lens (\ListComplianceItems' {resourceTypes} -> resourceTypes) (\s@ListComplianceItems' {} a -> s {resourceTypes = a} :: ListComplianceItems) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listComplianceItems_maxResults :: Lens.Lens' ListComplianceItems (Core.Maybe Core.Natural)
listComplianceItems_maxResults = Lens.lens (\ListComplianceItems' {maxResults} -> maxResults) (\s@ListComplianceItems' {} a -> s {maxResults = a} :: ListComplianceItems)

-- | The ID for the resources from which to get compliance information.
-- Currently, you can only specify one resource ID.
listComplianceItems_resourceIds :: Lens.Lens' ListComplianceItems (Core.Maybe (Core.NonEmpty Core.Text))
listComplianceItems_resourceIds = Lens.lens (\ListComplianceItems' {resourceIds} -> resourceIds) (\s@ListComplianceItems' {} a -> s {resourceIds = a} :: ListComplianceItems) Core.. Lens.mapping Lens._Coerce

-- | One or more compliance filters. Use a filter to return a more specific
-- list of results.
listComplianceItems_filters :: Lens.Lens' ListComplianceItems (Core.Maybe [ComplianceStringFilter])
listComplianceItems_filters = Lens.lens (\ListComplianceItems' {filters} -> filters) (\s@ListComplianceItems' {} a -> s {filters = a} :: ListComplianceItems) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager ListComplianceItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComplianceItemsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listComplianceItemsResponse_complianceItems
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listComplianceItems_nextToken
          Lens..~ rs
          Lens.^? listComplianceItemsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListComplianceItems where
  type
    AWSResponse ListComplianceItems =
      ListComplianceItemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComplianceItemsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ComplianceItems" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListComplianceItems

instance Core.NFData ListComplianceItems

instance Core.ToHeaders ListComplianceItems where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.ListComplianceItems" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListComplianceItems where
  toJSON ListComplianceItems' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceTypes" Core..=) Core.<$> resourceTypes,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ResourceIds" Core..=) Core.<$> resourceIds,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListComplianceItems where
  toPath = Core.const "/"

instance Core.ToQuery ListComplianceItems where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListComplianceItemsResponse' smart constructor.
data ListComplianceItemsResponse = ListComplianceItemsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of compliance information for the specified resource ID.
    complianceItems :: Core.Maybe [ComplianceItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListComplianceItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComplianceItemsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'complianceItems', 'listComplianceItemsResponse_complianceItems' - A list of compliance information for the specified resource ID.
--
-- 'httpStatus', 'listComplianceItemsResponse_httpStatus' - The response's http status code.
newListComplianceItemsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListComplianceItemsResponse
newListComplianceItemsResponse pHttpStatus_ =
  ListComplianceItemsResponse'
    { nextToken =
        Core.Nothing,
      complianceItems = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listComplianceItemsResponse_nextToken :: Lens.Lens' ListComplianceItemsResponse (Core.Maybe Core.Text)
listComplianceItemsResponse_nextToken = Lens.lens (\ListComplianceItemsResponse' {nextToken} -> nextToken) (\s@ListComplianceItemsResponse' {} a -> s {nextToken = a} :: ListComplianceItemsResponse)

-- | A list of compliance information for the specified resource ID.
listComplianceItemsResponse_complianceItems :: Lens.Lens' ListComplianceItemsResponse (Core.Maybe [ComplianceItem])
listComplianceItemsResponse_complianceItems = Lens.lens (\ListComplianceItemsResponse' {complianceItems} -> complianceItems) (\s@ListComplianceItemsResponse' {} a -> s {complianceItems = a} :: ListComplianceItemsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listComplianceItemsResponse_httpStatus :: Lens.Lens' ListComplianceItemsResponse Core.Int
listComplianceItemsResponse_httpStatus = Lens.lens (\ListComplianceItemsResponse' {httpStatus} -> httpStatus) (\s@ListComplianceItemsResponse' {} a -> s {httpStatus = a} :: ListComplianceItemsResponse)

instance Core.NFData ListComplianceItemsResponse
