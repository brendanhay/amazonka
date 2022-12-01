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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listWorldTemplates_nextToken,
    listWorldTemplates_maxResults,

    -- * Destructuring the Response
    ListWorldTemplatesResponse (..),
    newListWorldTemplatesResponse,

    -- * Response Lenses
    listWorldTemplatesResponse_templateSummaries,
    listWorldTemplatesResponse_nextToken,
    listWorldTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListWorldTemplates' smart constructor.
data ListWorldTemplates = ListWorldTemplates'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorldTemplates@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is used, @ListWorldTemplates@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListWorldTemplates@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListWorldTemplates@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'nextToken', 'listWorldTemplates_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'maxResults', 'listWorldTemplates_maxResults' - When this parameter is used, @ListWorldTemplates@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldTemplates@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldTemplates@ returns up to 100
-- results and a @nextToken@ value if applicable.
newListWorldTemplates ::
  ListWorldTemplates
newListWorldTemplates =
  ListWorldTemplates'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listWorldTemplates_nextToken :: Lens.Lens' ListWorldTemplates (Prelude.Maybe Prelude.Text)
listWorldTemplates_nextToken = Lens.lens (\ListWorldTemplates' {nextToken} -> nextToken) (\s@ListWorldTemplates' {} a -> s {nextToken = a} :: ListWorldTemplates)

-- | When this parameter is used, @ListWorldTemplates@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListWorldTemplates@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListWorldTemplates@ returns up to 100
-- results and a @nextToken@ value if applicable.
listWorldTemplates_maxResults :: Lens.Lens' ListWorldTemplates (Prelude.Maybe Prelude.Int)
listWorldTemplates_maxResults = Lens.lens (\ListWorldTemplates' {maxResults} -> maxResults) (\s@ListWorldTemplates' {} a -> s {maxResults = a} :: ListWorldTemplates)

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
      Prelude.Just Prelude.$
        rq
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
            Prelude.<$> ( x Core..?> "templateSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorldTemplates where
  hashWithSalt _salt ListWorldTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWorldTemplates where
  rnf ListWorldTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListWorldTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListWorldTemplates where
  toJSON ListWorldTemplates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListWorldTemplates where
  toPath = Prelude.const "/listWorldTemplates"

instance Core.ToQuery ListWorldTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorldTemplatesResponse' smart constructor.
data ListWorldTemplatesResponse = ListWorldTemplatesResponse'
  { -- | Summary information for templates.
    templateSummaries :: Prelude.Maybe [TemplateSummary],
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorldTemplates@
    -- again and assign that token to the request object\'s @nextToken@
    -- parameter. If there are no remaining results, the previous response
    -- object\'s NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'templateSummaries', 'listWorldTemplatesResponse_templateSummaries' - Summary information for templates.
--
-- 'nextToken', 'listWorldTemplatesResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
--
-- 'httpStatus', 'listWorldTemplatesResponse_httpStatus' - The response's http status code.
newListWorldTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorldTemplatesResponse
newListWorldTemplatesResponse pHttpStatus_ =
  ListWorldTemplatesResponse'
    { templateSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information for templates.
listWorldTemplatesResponse_templateSummaries :: Lens.Lens' ListWorldTemplatesResponse (Prelude.Maybe [TemplateSummary])
listWorldTemplatesResponse_templateSummaries = Lens.lens (\ListWorldTemplatesResponse' {templateSummaries} -> templateSummaries) (\s@ListWorldTemplatesResponse' {} a -> s {templateSummaries = a} :: ListWorldTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorldTemplates@
-- again and assign that token to the request object\'s @nextToken@
-- parameter. If there are no remaining results, the previous response
-- object\'s NextToken parameter is set to null.
listWorldTemplatesResponse_nextToken :: Lens.Lens' ListWorldTemplatesResponse (Prelude.Maybe Prelude.Text)
listWorldTemplatesResponse_nextToken = Lens.lens (\ListWorldTemplatesResponse' {nextToken} -> nextToken) (\s@ListWorldTemplatesResponse' {} a -> s {nextToken = a} :: ListWorldTemplatesResponse)

-- | The response's http status code.
listWorldTemplatesResponse_httpStatus :: Lens.Lens' ListWorldTemplatesResponse Prelude.Int
listWorldTemplatesResponse_httpStatus = Lens.lens (\ListWorldTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListWorldTemplatesResponse' {} a -> s {httpStatus = a} :: ListWorldTemplatesResponse)

instance Prelude.NFData ListWorldTemplatesResponse where
  rnf ListWorldTemplatesResponse' {..} =
    Prelude.rnf templateSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
