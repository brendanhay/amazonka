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
-- Module      : Network.AWS.CostExplorer.ListCostCategoryDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, @NumberOfRules@ and effective dates of all Cost
-- Categories defined in the account. You have the option to use
-- @EffectiveOn@ to return a list of Cost Categories that were active on a
-- specific date. If there is no @EffectiveOn@ specified, you’ll see Cost
-- Categories that are effective on the current date. If Cost Category is
-- still effective, @EffectiveEnd@ is omitted in the response.
-- @ListCostCategoryDefinitions@ supports pagination. The request can have
-- a @MaxResults@ range up to 100.
module Network.AWS.CostExplorer.ListCostCategoryDefinitions
  ( -- * Creating a Request
    ListCostCategoryDefinitions (..),
    newListCostCategoryDefinitions,

    -- * Request Lenses
    listCostCategoryDefinitions_nextToken,
    listCostCategoryDefinitions_maxResults,
    listCostCategoryDefinitions_effectiveOn,

    -- * Destructuring the Response
    ListCostCategoryDefinitionsResponse (..),
    newListCostCategoryDefinitionsResponse,

    -- * Response Lenses
    listCostCategoryDefinitionsResponse_nextToken,
    listCostCategoryDefinitionsResponse_costCategoryReferences,
    listCostCategoryDefinitionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListCostCategoryDefinitions' smart constructor.
data ListCostCategoryDefinitions = ListCostCategoryDefinitions'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of entries a paginated response contains.
    maxResults :: Core.Maybe Core.Natural,
    -- | The date when the Cost Category was effective.
    effectiveOn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCostCategoryDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCostCategoryDefinitions_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'maxResults', 'listCostCategoryDefinitions_maxResults' - The number of entries a paginated response contains.
--
-- 'effectiveOn', 'listCostCategoryDefinitions_effectiveOn' - The date when the Cost Category was effective.
newListCostCategoryDefinitions ::
  ListCostCategoryDefinitions
newListCostCategoryDefinitions =
  ListCostCategoryDefinitions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      effectiveOn = Core.Nothing
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
listCostCategoryDefinitions_nextToken :: Lens.Lens' ListCostCategoryDefinitions (Core.Maybe Core.Text)
listCostCategoryDefinitions_nextToken = Lens.lens (\ListCostCategoryDefinitions' {nextToken} -> nextToken) (\s@ListCostCategoryDefinitions' {} a -> s {nextToken = a} :: ListCostCategoryDefinitions)

-- | The number of entries a paginated response contains.
listCostCategoryDefinitions_maxResults :: Lens.Lens' ListCostCategoryDefinitions (Core.Maybe Core.Natural)
listCostCategoryDefinitions_maxResults = Lens.lens (\ListCostCategoryDefinitions' {maxResults} -> maxResults) (\s@ListCostCategoryDefinitions' {} a -> s {maxResults = a} :: ListCostCategoryDefinitions)

-- | The date when the Cost Category was effective.
listCostCategoryDefinitions_effectiveOn :: Lens.Lens' ListCostCategoryDefinitions (Core.Maybe Core.Text)
listCostCategoryDefinitions_effectiveOn = Lens.lens (\ListCostCategoryDefinitions' {effectiveOn} -> effectiveOn) (\s@ListCostCategoryDefinitions' {} a -> s {effectiveOn = a} :: ListCostCategoryDefinitions)

instance Core.AWSRequest ListCostCategoryDefinitions where
  type
    AWSResponse ListCostCategoryDefinitions =
      ListCostCategoryDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCostCategoryDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "CostCategoryReferences"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListCostCategoryDefinitions

instance Core.NFData ListCostCategoryDefinitions

instance Core.ToHeaders ListCostCategoryDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.ListCostCategoryDefinitions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListCostCategoryDefinitions where
  toJSON ListCostCategoryDefinitions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("EffectiveOn" Core..=) Core.<$> effectiveOn
          ]
      )

instance Core.ToPath ListCostCategoryDefinitions where
  toPath = Core.const "/"

instance Core.ToQuery ListCostCategoryDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListCostCategoryDefinitionsResponse' smart constructor.
data ListCostCategoryDefinitionsResponse = ListCostCategoryDefinitionsResponse'
  { -- | The token to retrieve the next set of results. Amazon Web Services
    -- provides the token when the response from a previous call has more
    -- results than the maximum page size.
    nextToken :: Core.Maybe Core.Text,
    -- | A reference to a Cost Category containing enough information to identify
    -- the Cost Category.
    costCategoryReferences :: Core.Maybe [CostCategoryReference],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListCostCategoryDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCostCategoryDefinitionsResponse_nextToken' - The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
--
-- 'costCategoryReferences', 'listCostCategoryDefinitionsResponse_costCategoryReferences' - A reference to a Cost Category containing enough information to identify
-- the Cost Category.
--
-- 'httpStatus', 'listCostCategoryDefinitionsResponse_httpStatus' - The response's http status code.
newListCostCategoryDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListCostCategoryDefinitionsResponse
newListCostCategoryDefinitionsResponse pHttpStatus_ =
  ListCostCategoryDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      costCategoryReferences = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results. Amazon Web Services
-- provides the token when the response from a previous call has more
-- results than the maximum page size.
listCostCategoryDefinitionsResponse_nextToken :: Lens.Lens' ListCostCategoryDefinitionsResponse (Core.Maybe Core.Text)
listCostCategoryDefinitionsResponse_nextToken = Lens.lens (\ListCostCategoryDefinitionsResponse' {nextToken} -> nextToken) (\s@ListCostCategoryDefinitionsResponse' {} a -> s {nextToken = a} :: ListCostCategoryDefinitionsResponse)

-- | A reference to a Cost Category containing enough information to identify
-- the Cost Category.
listCostCategoryDefinitionsResponse_costCategoryReferences :: Lens.Lens' ListCostCategoryDefinitionsResponse (Core.Maybe [CostCategoryReference])
listCostCategoryDefinitionsResponse_costCategoryReferences = Lens.lens (\ListCostCategoryDefinitionsResponse' {costCategoryReferences} -> costCategoryReferences) (\s@ListCostCategoryDefinitionsResponse' {} a -> s {costCategoryReferences = a} :: ListCostCategoryDefinitionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCostCategoryDefinitionsResponse_httpStatus :: Lens.Lens' ListCostCategoryDefinitionsResponse Core.Int
listCostCategoryDefinitionsResponse_httpStatus = Lens.lens (\ListCostCategoryDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListCostCategoryDefinitionsResponse' {} a -> s {httpStatus = a} :: ListCostCategoryDefinitionsResponse)

instance
  Core.NFData
    ListCostCategoryDefinitionsResponse
