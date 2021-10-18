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
-- Module      : Network.AWS.Transcribe.ListCallAnalyticsCategories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information about the call analytics categories that
-- you\'ve created. You can use the information in this list to find a
-- specific category. You can then use the operation to get more
-- information about it.
module Network.AWS.Transcribe.ListCallAnalyticsCategories
  ( -- * Creating a Request
    ListCallAnalyticsCategories (..),
    newListCallAnalyticsCategories,

    -- * Request Lenses
    listCallAnalyticsCategories_nextToken,
    listCallAnalyticsCategories_maxResults,

    -- * Destructuring the Response
    ListCallAnalyticsCategoriesResponse (..),
    newListCallAnalyticsCategoriesResponse,

    -- * Response Lenses
    listCallAnalyticsCategoriesResponse_nextToken,
    listCallAnalyticsCategoriesResponse_categories,
    listCallAnalyticsCategoriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListCallAnalyticsCategories' smart constructor.
data ListCallAnalyticsCategories = ListCallAnalyticsCategories'
  { -- | When included, @NextToken@fetches the next set of categories if the
    -- result of the previous request was truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of categories to return in each page of results. If
    -- there are fewer results than the value you specify, only the actual
    -- results are returned. If you do not specify a value, the default of 5 is
    -- used.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCallAnalyticsCategories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCallAnalyticsCategories_nextToken' - When included, @NextToken@fetches the next set of categories if the
-- result of the previous request was truncated.
--
-- 'maxResults', 'listCallAnalyticsCategories_maxResults' - The maximum number of categories to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
newListCallAnalyticsCategories ::
  ListCallAnalyticsCategories
newListCallAnalyticsCategories =
  ListCallAnalyticsCategories'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | When included, @NextToken@fetches the next set of categories if the
-- result of the previous request was truncated.
listCallAnalyticsCategories_nextToken :: Lens.Lens' ListCallAnalyticsCategories (Prelude.Maybe Prelude.Text)
listCallAnalyticsCategories_nextToken = Lens.lens (\ListCallAnalyticsCategories' {nextToken} -> nextToken) (\s@ListCallAnalyticsCategories' {} a -> s {nextToken = a} :: ListCallAnalyticsCategories)

-- | The maximum number of categories to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
listCallAnalyticsCategories_maxResults :: Lens.Lens' ListCallAnalyticsCategories (Prelude.Maybe Prelude.Natural)
listCallAnalyticsCategories_maxResults = Lens.lens (\ListCallAnalyticsCategories' {maxResults} -> maxResults) (\s@ListCallAnalyticsCategories' {} a -> s {maxResults = a} :: ListCallAnalyticsCategories)

instance Core.AWSRequest ListCallAnalyticsCategories where
  type
    AWSResponse ListCallAnalyticsCategories =
      ListCallAnalyticsCategoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCallAnalyticsCategoriesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Categories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCallAnalyticsCategories

instance Prelude.NFData ListCallAnalyticsCategories

instance Core.ToHeaders ListCallAnalyticsCategories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListCallAnalyticsCategories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCallAnalyticsCategories where
  toJSON ListCallAnalyticsCategories' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListCallAnalyticsCategories where
  toPath = Prelude.const "/"

instance Core.ToQuery ListCallAnalyticsCategories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCallAnalyticsCategoriesResponse' smart constructor.
data ListCallAnalyticsCategoriesResponse = ListCallAnalyticsCategoriesResponse'
  { -- | The operation returns a page of jobs at a time. The maximum size of the
    -- list is set by the @MaxResults@ parameter. If there are more categories
    -- in the list than the page size, Amazon Transcribe returns the @NextPage@
    -- token. Include the token in the next request to the operation to return
    -- the next page of analytics categories.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects containing information about analytics categories.
    categories :: Prelude.Maybe [CategoryProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCallAnalyticsCategoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCallAnalyticsCategoriesResponse_nextToken' - The operation returns a page of jobs at a time. The maximum size of the
-- list is set by the @MaxResults@ parameter. If there are more categories
-- in the list than the page size, Amazon Transcribe returns the @NextPage@
-- token. Include the token in the next request to the operation to return
-- the next page of analytics categories.
--
-- 'categories', 'listCallAnalyticsCategoriesResponse_categories' - A list of objects containing information about analytics categories.
--
-- 'httpStatus', 'listCallAnalyticsCategoriesResponse_httpStatus' - The response's http status code.
newListCallAnalyticsCategoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCallAnalyticsCategoriesResponse
newListCallAnalyticsCategoriesResponse pHttpStatus_ =
  ListCallAnalyticsCategoriesResponse'
    { nextToken =
        Prelude.Nothing,
      categories = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The operation returns a page of jobs at a time. The maximum size of the
-- list is set by the @MaxResults@ parameter. If there are more categories
-- in the list than the page size, Amazon Transcribe returns the @NextPage@
-- token. Include the token in the next request to the operation to return
-- the next page of analytics categories.
listCallAnalyticsCategoriesResponse_nextToken :: Lens.Lens' ListCallAnalyticsCategoriesResponse (Prelude.Maybe Prelude.Text)
listCallAnalyticsCategoriesResponse_nextToken = Lens.lens (\ListCallAnalyticsCategoriesResponse' {nextToken} -> nextToken) (\s@ListCallAnalyticsCategoriesResponse' {} a -> s {nextToken = a} :: ListCallAnalyticsCategoriesResponse)

-- | A list of objects containing information about analytics categories.
listCallAnalyticsCategoriesResponse_categories :: Lens.Lens' ListCallAnalyticsCategoriesResponse (Prelude.Maybe [CategoryProperties])
listCallAnalyticsCategoriesResponse_categories = Lens.lens (\ListCallAnalyticsCategoriesResponse' {categories} -> categories) (\s@ListCallAnalyticsCategoriesResponse' {} a -> s {categories = a} :: ListCallAnalyticsCategoriesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listCallAnalyticsCategoriesResponse_httpStatus :: Lens.Lens' ListCallAnalyticsCategoriesResponse Prelude.Int
listCallAnalyticsCategoriesResponse_httpStatus = Lens.lens (\ListCallAnalyticsCategoriesResponse' {httpStatus} -> httpStatus) (\s@ListCallAnalyticsCategoriesResponse' {} a -> s {httpStatus = a} :: ListCallAnalyticsCategoriesResponse)

instance
  Prelude.NFData
    ListCallAnalyticsCategoriesResponse
