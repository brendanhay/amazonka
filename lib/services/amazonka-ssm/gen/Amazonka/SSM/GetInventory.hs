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
-- Module      : Amazonka.SSM.GetInventory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query inventory information. This includes managed node status, such as
-- @Stopped@ or @Terminated@.
--
-- This operation returns paginated results.
module Amazonka.SSM.GetInventory
  ( -- * Creating a Request
    GetInventory (..),
    newGetInventory,

    -- * Request Lenses
    getInventory_aggregators,
    getInventory_filters,
    getInventory_maxResults,
    getInventory_nextToken,
    getInventory_resultAttributes,

    -- * Destructuring the Response
    GetInventoryResponse (..),
    newGetInventoryResponse,

    -- * Response Lenses
    getInventoryResponse_entities,
    getInventoryResponse_nextToken,
    getInventoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetInventory' smart constructor.
data GetInventory = GetInventory'
  { -- | Returns counts of inventory types based on one or more expressions. For
    -- example, if you aggregate by using an expression that uses the
    -- @AWS:InstanceInformation.PlatformType@ type, you can see a count of how
    -- many Windows and Linux managed nodes exist in your inventoried fleet.
    aggregators :: Prelude.Maybe (Prelude.NonEmpty InventoryAggregator),
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    filters :: Prelude.Maybe (Prelude.NonEmpty InventoryFilter),
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of inventory item types to return.
    resultAttributes :: Prelude.Maybe (Prelude.NonEmpty ResultAttribute)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInventory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregators', 'getInventory_aggregators' - Returns counts of inventory types based on one or more expressions. For
-- example, if you aggregate by using an expression that uses the
-- @AWS:InstanceInformation.PlatformType@ type, you can see a count of how
-- many Windows and Linux managed nodes exist in your inventoried fleet.
--
-- 'filters', 'getInventory_filters' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- 'maxResults', 'getInventory_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'getInventory_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'resultAttributes', 'getInventory_resultAttributes' - The list of inventory item types to return.
newGetInventory ::
  GetInventory
newGetInventory =
  GetInventory'
    { aggregators = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resultAttributes = Prelude.Nothing
    }

-- | Returns counts of inventory types based on one or more expressions. For
-- example, if you aggregate by using an expression that uses the
-- @AWS:InstanceInformation.PlatformType@ type, you can see a count of how
-- many Windows and Linux managed nodes exist in your inventoried fleet.
getInventory_aggregators :: Lens.Lens' GetInventory (Prelude.Maybe (Prelude.NonEmpty InventoryAggregator))
getInventory_aggregators = Lens.lens (\GetInventory' {aggregators} -> aggregators) (\s@GetInventory' {} a -> s {aggregators = a} :: GetInventory) Prelude.. Lens.mapping Lens.coerced

-- | One or more filters. Use a filter to return a more specific list of
-- results.
getInventory_filters :: Lens.Lens' GetInventory (Prelude.Maybe (Prelude.NonEmpty InventoryFilter))
getInventory_filters = Lens.lens (\GetInventory' {filters} -> filters) (\s@GetInventory' {} a -> s {filters = a} :: GetInventory) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
getInventory_maxResults :: Lens.Lens' GetInventory (Prelude.Maybe Prelude.Natural)
getInventory_maxResults = Lens.lens (\GetInventory' {maxResults} -> maxResults) (\s@GetInventory' {} a -> s {maxResults = a} :: GetInventory)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
getInventory_nextToken :: Lens.Lens' GetInventory (Prelude.Maybe Prelude.Text)
getInventory_nextToken = Lens.lens (\GetInventory' {nextToken} -> nextToken) (\s@GetInventory' {} a -> s {nextToken = a} :: GetInventory)

-- | The list of inventory item types to return.
getInventory_resultAttributes :: Lens.Lens' GetInventory (Prelude.Maybe (Prelude.NonEmpty ResultAttribute))
getInventory_resultAttributes = Lens.lens (\GetInventory' {resultAttributes} -> resultAttributes) (\s@GetInventory' {} a -> s {resultAttributes = a} :: GetInventory) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager GetInventory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInventoryResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getInventoryResponse_entities Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getInventory_nextToken
          Lens..~ rs
          Lens.^? getInventoryResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetInventory where
  type AWSResponse GetInventory = GetInventoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInventoryResponse'
            Prelude.<$> (x Data..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInventory where
  hashWithSalt _salt GetInventory' {..} =
    _salt `Prelude.hashWithSalt` aggregators
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resultAttributes

instance Prelude.NFData GetInventory where
  rnf GetInventory' {..} =
    Prelude.rnf aggregators
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resultAttributes

instance Data.ToHeaders GetInventory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.GetInventory" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInventory where
  toJSON GetInventory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aggregators" Data..=) Prelude.<$> aggregators,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResultAttributes" Data..=)
              Prelude.<$> resultAttributes
          ]
      )

instance Data.ToPath GetInventory where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInventory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInventoryResponse' smart constructor.
data GetInventoryResponse = GetInventoryResponse'
  { -- | Collection of inventory entities such as a collection of managed node
    -- inventory.
    entities :: Prelude.Maybe [InventoryResultEntity],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInventoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entities', 'getInventoryResponse_entities' - Collection of inventory entities such as a collection of managed node
-- inventory.
--
-- 'nextToken', 'getInventoryResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'getInventoryResponse_httpStatus' - The response's http status code.
newGetInventoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInventoryResponse
newGetInventoryResponse pHttpStatus_ =
  GetInventoryResponse'
    { entities = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Collection of inventory entities such as a collection of managed node
-- inventory.
getInventoryResponse_entities :: Lens.Lens' GetInventoryResponse (Prelude.Maybe [InventoryResultEntity])
getInventoryResponse_entities = Lens.lens (\GetInventoryResponse' {entities} -> entities) (\s@GetInventoryResponse' {} a -> s {entities = a} :: GetInventoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
getInventoryResponse_nextToken :: Lens.Lens' GetInventoryResponse (Prelude.Maybe Prelude.Text)
getInventoryResponse_nextToken = Lens.lens (\GetInventoryResponse' {nextToken} -> nextToken) (\s@GetInventoryResponse' {} a -> s {nextToken = a} :: GetInventoryResponse)

-- | The response's http status code.
getInventoryResponse_httpStatus :: Lens.Lens' GetInventoryResponse Prelude.Int
getInventoryResponse_httpStatus = Lens.lens (\GetInventoryResponse' {httpStatus} -> httpStatus) (\s@GetInventoryResponse' {} a -> s {httpStatus = a} :: GetInventoryResponse)

instance Prelude.NFData GetInventoryResponse where
  rnf GetInventoryResponse' {..} =
    Prelude.rnf entities
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
