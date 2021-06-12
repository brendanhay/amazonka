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
-- Module      : Network.AWS.SSM.GetInventory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query inventory information.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetInventory
  ( -- * Creating a Request
    GetInventory (..),
    newGetInventory,

    -- * Request Lenses
    getInventory_nextToken,
    getInventory_maxResults,
    getInventory_resultAttributes,
    getInventory_filters,
    getInventory_aggregators,

    -- * Destructuring the Response
    GetInventoryResponse (..),
    newGetInventoryResponse,

    -- * Response Lenses
    getInventoryResponse_nextToken,
    getInventoryResponse_entities,
    getInventoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetInventory' smart constructor.
data GetInventory = GetInventory'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The list of inventory item types to return.
    resultAttributes :: Core.Maybe (Core.NonEmpty ResultAttribute),
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    filters :: Core.Maybe (Core.NonEmpty InventoryFilter),
    -- | Returns counts of inventory types based on one or more expressions. For
    -- example, if you aggregate by using an expression that uses the
    -- @AWS:InstanceInformation.PlatformType@ type, you can see a count of how
    -- many Windows and Linux instances exist in your inventoried fleet.
    aggregators :: Core.Maybe (Core.NonEmpty InventoryAggregator)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInventory_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'getInventory_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'resultAttributes', 'getInventory_resultAttributes' - The list of inventory item types to return.
--
-- 'filters', 'getInventory_filters' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- 'aggregators', 'getInventory_aggregators' - Returns counts of inventory types based on one or more expressions. For
-- example, if you aggregate by using an expression that uses the
-- @AWS:InstanceInformation.PlatformType@ type, you can see a count of how
-- many Windows and Linux instances exist in your inventoried fleet.
newGetInventory ::
  GetInventory
newGetInventory =
  GetInventory'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      resultAttributes = Core.Nothing,
      filters = Core.Nothing,
      aggregators = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getInventory_nextToken :: Lens.Lens' GetInventory (Core.Maybe Core.Text)
getInventory_nextToken = Lens.lens (\GetInventory' {nextToken} -> nextToken) (\s@GetInventory' {} a -> s {nextToken = a} :: GetInventory)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getInventory_maxResults :: Lens.Lens' GetInventory (Core.Maybe Core.Natural)
getInventory_maxResults = Lens.lens (\GetInventory' {maxResults} -> maxResults) (\s@GetInventory' {} a -> s {maxResults = a} :: GetInventory)

-- | The list of inventory item types to return.
getInventory_resultAttributes :: Lens.Lens' GetInventory (Core.Maybe (Core.NonEmpty ResultAttribute))
getInventory_resultAttributes = Lens.lens (\GetInventory' {resultAttributes} -> resultAttributes) (\s@GetInventory' {} a -> s {resultAttributes = a} :: GetInventory) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. Use a filter to return a more specific list of
-- results.
getInventory_filters :: Lens.Lens' GetInventory (Core.Maybe (Core.NonEmpty InventoryFilter))
getInventory_filters = Lens.lens (\GetInventory' {filters} -> filters) (\s@GetInventory' {} a -> s {filters = a} :: GetInventory) Core.. Lens.mapping Lens._Coerce

-- | Returns counts of inventory types based on one or more expressions. For
-- example, if you aggregate by using an expression that uses the
-- @AWS:InstanceInformation.PlatformType@ type, you can see a count of how
-- many Windows and Linux instances exist in your inventoried fleet.
getInventory_aggregators :: Lens.Lens' GetInventory (Core.Maybe (Core.NonEmpty InventoryAggregator))
getInventory_aggregators = Lens.lens (\GetInventory' {aggregators} -> aggregators) (\s@GetInventory' {} a -> s {aggregators = a} :: GetInventory) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager GetInventory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInventoryResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getInventoryResponse_entities Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getInventory_nextToken
          Lens..~ rs
          Lens.^? getInventoryResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetInventory where
  type AWSResponse GetInventory = GetInventoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInventoryResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Entities" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInventory

instance Core.NFData GetInventory

instance Core.ToHeaders GetInventory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetInventory" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInventory where
  toJSON GetInventory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ResultAttributes" Core..=)
              Core.<$> resultAttributes,
            ("Filters" Core..=) Core.<$> filters,
            ("Aggregators" Core..=) Core.<$> aggregators
          ]
      )

instance Core.ToPath GetInventory where
  toPath = Core.const "/"

instance Core.ToQuery GetInventory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInventoryResponse' smart constructor.
data GetInventoryResponse = GetInventoryResponse'
  { -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Collection of inventory entities such as a collection of instance
    -- inventory.
    entities :: Core.Maybe [InventoryResultEntity],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInventoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInventoryResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'entities', 'getInventoryResponse_entities' - Collection of inventory entities such as a collection of instance
-- inventory.
--
-- 'httpStatus', 'getInventoryResponse_httpStatus' - The response's http status code.
newGetInventoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInventoryResponse
newGetInventoryResponse pHttpStatus_ =
  GetInventoryResponse'
    { nextToken = Core.Nothing,
      entities = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
getInventoryResponse_nextToken :: Lens.Lens' GetInventoryResponse (Core.Maybe Core.Text)
getInventoryResponse_nextToken = Lens.lens (\GetInventoryResponse' {nextToken} -> nextToken) (\s@GetInventoryResponse' {} a -> s {nextToken = a} :: GetInventoryResponse)

-- | Collection of inventory entities such as a collection of instance
-- inventory.
getInventoryResponse_entities :: Lens.Lens' GetInventoryResponse (Core.Maybe [InventoryResultEntity])
getInventoryResponse_entities = Lens.lens (\GetInventoryResponse' {entities} -> entities) (\s@GetInventoryResponse' {} a -> s {entities = a} :: GetInventoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getInventoryResponse_httpStatus :: Lens.Lens' GetInventoryResponse Core.Int
getInventoryResponse_httpStatus = Lens.lens (\GetInventoryResponse' {httpStatus} -> httpStatus) (\s@GetInventoryResponse' {} a -> s {httpStatus = a} :: GetInventoryResponse)

instance Core.NFData GetInventoryResponse
