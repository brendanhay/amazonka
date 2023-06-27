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
-- Module      : Amazonka.RobOMaker.ListWorldTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists world templates.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListWorldTemplates
  ( -- * Creating a Request
    ListWorldTemplates (..),
    newListWorldTemplates,

    -- * Request Lenses
    listWorldTemplates_maxResults,
    listWorldTemplates_nextToken,

    -- * Destructuring the Response
    ListWorldTemplatesResponse (..),
    newListWorldTemplatesResponse,

    -- * Response Lenses
    listWorldTemplatesResponse_nextToken,
    listWorldTemplatesResponse_templateSummaries,
    listWorldTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListWorldTemplates' smart constructor.
data ListWorldTemplates = ListWorldTemplates'
  { -- | When this parameter is used, @ListWorldTemplates@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListWorldTemplates@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListWorldTemplates@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorldTemplates@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorldTemplates_maxResults' - When this parameter is used, @ListWorldTemplates@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldTemplates@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldTemplates@ returns up to 100
-- results and a @nextToken@ value if applicable.
--
-- 'nextToken', 'listWorldTemplates_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
newListWorldTemplates ::
  ListWorldTemplates
newListWorldTemplates =
  ListWorldTemplates'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | When this parameter is used, @ListWorldTemplates@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldTemplates@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldTemplates@ returns up to 100
-- results and a @nextToken@ value if applicable.
listWorldTemplates_maxResults :: Lens.Lens' ListWorldTemplates (Prelude.Maybe Prelude.Int)
listWorldTemplates_maxResults = Lens.lens (\ListWorldTemplates' {maxResults} -> maxResults) (\s@ListWorldTemplates' {} a -> s {maxResults = a} :: ListWorldTemplates)

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listWorldTemplates_nextToken :: Lens.Lens' ListWorldTemplates (Prelude.Maybe Prelude.Text)
listWorldTemplates_nextToken = Lens.lens (\ListWorldTemplates' {nextToken} -> nextToken) (\s@ListWorldTemplates' {} a -> s {nextToken = a} :: ListWorldTemplates)

instance Core.AWSPager ListWorldTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorldTemplatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorldTemplatesResponse_templateSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listWorldTemplates_nextToken
          Lens..~ rs
          Lens.^? listWorldTemplatesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListWorldTemplates where
  type
    AWSResponse ListWorldTemplates =
      ListWorldTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorldTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "templateSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorldTemplates where
  hashWithSalt _salt ListWorldTemplates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListWorldTemplates where
  rnf ListWorldTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListWorldTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListWorldTemplates where
  toJSON ListWorldTemplates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListWorldTemplates where
  toPath = Prelude.const "/listWorldTemplates"

instance Data.ToQuery ListWorldTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorldTemplatesResponse' smart constructor.
data ListWorldTemplatesResponse = ListWorldTemplatesResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorldTemplates@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information for templates.
    templateSummaries :: Prelude.Maybe [TemplateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorldTemplatesResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'templateSummaries', 'listWorldTemplatesResponse_templateSummaries' - Summary information for templates.
--
-- 'httpStatus', 'listWorldTemplatesResponse_httpStatus' - The response's http status code.
newListWorldTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorldTemplatesResponse
newListWorldTemplatesResponse pHttpStatus_ =
  ListWorldTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      templateSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listWorldTemplatesResponse_nextToken :: Lens.Lens' ListWorldTemplatesResponse (Prelude.Maybe Prelude.Text)
listWorldTemplatesResponse_nextToken = Lens.lens (\ListWorldTemplatesResponse' {nextToken} -> nextToken) (\s@ListWorldTemplatesResponse' {} a -> s {nextToken = a} :: ListWorldTemplatesResponse)

-- | Summary information for templates.
listWorldTemplatesResponse_templateSummaries :: Lens.Lens' ListWorldTemplatesResponse (Prelude.Maybe [TemplateSummary])
listWorldTemplatesResponse_templateSummaries = Lens.lens (\ListWorldTemplatesResponse' {templateSummaries} -> templateSummaries) (\s@ListWorldTemplatesResponse' {} a -> s {templateSummaries = a} :: ListWorldTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorldTemplatesResponse_httpStatus :: Lens.Lens' ListWorldTemplatesResponse Prelude.Int
listWorldTemplatesResponse_httpStatus = Lens.lens (\ListWorldTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListWorldTemplatesResponse' {} a -> s {httpStatus = a} :: ListWorldTemplatesResponse)

instance Prelude.NFData ListWorldTemplatesResponse where
  rnf ListWorldTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templateSummaries
      `Prelude.seq` Prelude.rnf httpStatus
