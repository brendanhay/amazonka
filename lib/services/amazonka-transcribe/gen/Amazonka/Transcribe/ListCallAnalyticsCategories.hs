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
-- Module      : Amazonka.Transcribe.ListCallAnalyticsCategories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of Call Analytics categories, including all rules that
-- make up each category.
--
-- To get detailed information about a specific Call Analytics category,
-- use the operation.
module Amazonka.Transcribe.ListCallAnalyticsCategories
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListCallAnalyticsCategories' smart constructor.
data ListCallAnalyticsCategories = ListCallAnalyticsCategories'
  { -- | If your @ListCallAnalyticsCategories@ request returns more results than
    -- can be displayed, @NextToken@ is displayed in the response with an
    -- associated string. To get the next page of results, copy this string and
    -- repeat your request, including @NextToken@ with the value of the copied
    -- string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Call Analytics categories to return in each page
    -- of results. If there are fewer results than the value you specify, only
    -- the actual results are returned. If you don\'t specify a value, a
    -- default of 5 is used.
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
-- 'nextToken', 'listCallAnalyticsCategories_nextToken' - If your @ListCallAnalyticsCategories@ request returns more results than
-- can be displayed, @NextToken@ is displayed in the response with an
-- associated string. To get the next page of results, copy this string and
-- repeat your request, including @NextToken@ with the value of the copied
-- string. Repeat as needed to view all your results.
--
-- 'maxResults', 'listCallAnalyticsCategories_maxResults' - The maximum number of Call Analytics categories to return in each page
-- of results. If there are fewer results than the value you specify, only
-- the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
newListCallAnalyticsCategories ::
  ListCallAnalyticsCategories
newListCallAnalyticsCategories =
  ListCallAnalyticsCategories'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If your @ListCallAnalyticsCategories@ request returns more results than
-- can be displayed, @NextToken@ is displayed in the response with an
-- associated string. To get the next page of results, copy this string and
-- repeat your request, including @NextToken@ with the value of the copied
-- string. Repeat as needed to view all your results.
listCallAnalyticsCategories_nextToken :: Lens.Lens' ListCallAnalyticsCategories (Prelude.Maybe Prelude.Text)
listCallAnalyticsCategories_nextToken = Lens.lens (\ListCallAnalyticsCategories' {nextToken} -> nextToken) (\s@ListCallAnalyticsCategories' {} a -> s {nextToken = a} :: ListCallAnalyticsCategories)

-- | The maximum number of Call Analytics categories to return in each page
-- of results. If there are fewer results than the value you specify, only
-- the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
listCallAnalyticsCategories_maxResults :: Lens.Lens' ListCallAnalyticsCategories (Prelude.Maybe Prelude.Natural)
listCallAnalyticsCategories_maxResults = Lens.lens (\ListCallAnalyticsCategories' {maxResults} -> maxResults) (\s@ListCallAnalyticsCategories' {} a -> s {maxResults = a} :: ListCallAnalyticsCategories)

instance Core.AWSRequest ListCallAnalyticsCategories where
  type
    AWSResponse ListCallAnalyticsCategories =
      ListCallAnalyticsCategoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCallAnalyticsCategoriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Categories" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCallAnalyticsCategories where
  hashWithSalt _salt ListCallAnalyticsCategories' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCallAnalyticsCategories where
  rnf ListCallAnalyticsCategories' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListCallAnalyticsCategories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.ListCallAnalyticsCategories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCallAnalyticsCategories where
  toJSON ListCallAnalyticsCategories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListCallAnalyticsCategories where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCallAnalyticsCategories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCallAnalyticsCategoriesResponse' smart constructor.
data ListCallAnalyticsCategoriesResponse = ListCallAnalyticsCategoriesResponse'
  { -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides detailed information about your Call Analytics categories,
    -- including all the rules associated with each category.
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
-- 'nextToken', 'listCallAnalyticsCategoriesResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'categories', 'listCallAnalyticsCategoriesResponse_categories' - Provides detailed information about your Call Analytics categories,
-- including all the rules associated with each category.
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

-- | If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
listCallAnalyticsCategoriesResponse_nextToken :: Lens.Lens' ListCallAnalyticsCategoriesResponse (Prelude.Maybe Prelude.Text)
listCallAnalyticsCategoriesResponse_nextToken = Lens.lens (\ListCallAnalyticsCategoriesResponse' {nextToken} -> nextToken) (\s@ListCallAnalyticsCategoriesResponse' {} a -> s {nextToken = a} :: ListCallAnalyticsCategoriesResponse)

-- | Provides detailed information about your Call Analytics categories,
-- including all the rules associated with each category.
listCallAnalyticsCategoriesResponse_categories :: Lens.Lens' ListCallAnalyticsCategoriesResponse (Prelude.Maybe [CategoryProperties])
listCallAnalyticsCategoriesResponse_categories = Lens.lens (\ListCallAnalyticsCategoriesResponse' {categories} -> categories) (\s@ListCallAnalyticsCategoriesResponse' {} a -> s {categories = a} :: ListCallAnalyticsCategoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCallAnalyticsCategoriesResponse_httpStatus :: Lens.Lens' ListCallAnalyticsCategoriesResponse Prelude.Int
listCallAnalyticsCategoriesResponse_httpStatus = Lens.lens (\ListCallAnalyticsCategoriesResponse' {httpStatus} -> httpStatus) (\s@ListCallAnalyticsCategoriesResponse' {} a -> s {httpStatus = a} :: ListCallAnalyticsCategoriesResponse)

instance
  Prelude.NFData
    ListCallAnalyticsCategoriesResponse
  where
  rnf ListCallAnalyticsCategoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf categories
      `Prelude.seq` Prelude.rnf httpStatus
