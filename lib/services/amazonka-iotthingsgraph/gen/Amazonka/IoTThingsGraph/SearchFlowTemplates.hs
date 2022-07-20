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
-- Module      : Amazonka.IoTThingsGraph.SearchFlowTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for summary information about workflows.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.SearchFlowTemplates
  ( -- * Creating a Request
    SearchFlowTemplates (..),
    newSearchFlowTemplates,

    -- * Request Lenses
    searchFlowTemplates_nextToken,
    searchFlowTemplates_filters,
    searchFlowTemplates_maxResults,

    -- * Destructuring the Response
    SearchFlowTemplatesResponse (..),
    newSearchFlowTemplatesResponse,

    -- * Response Lenses
    searchFlowTemplatesResponse_nextToken,
    searchFlowTemplatesResponse_summaries,
    searchFlowTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchFlowTemplates' smart constructor.
data SearchFlowTemplates = SearchFlowTemplates'
  { -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that limit the result set. The only valid filter is
    -- @DEVICE_MODEL_ID@.
    filters :: Prelude.Maybe [FlowTemplateFilter],
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFlowTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchFlowTemplates_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'filters', 'searchFlowTemplates_filters' - An array of objects that limit the result set. The only valid filter is
-- @DEVICE_MODEL_ID@.
--
-- 'maxResults', 'searchFlowTemplates_maxResults' - The maximum number of results to return in the response.
newSearchFlowTemplates ::
  SearchFlowTemplates
newSearchFlowTemplates =
  SearchFlowTemplates'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
searchFlowTemplates_nextToken :: Lens.Lens' SearchFlowTemplates (Prelude.Maybe Prelude.Text)
searchFlowTemplates_nextToken = Lens.lens (\SearchFlowTemplates' {nextToken} -> nextToken) (\s@SearchFlowTemplates' {} a -> s {nextToken = a} :: SearchFlowTemplates)

-- | An array of objects that limit the result set. The only valid filter is
-- @DEVICE_MODEL_ID@.
searchFlowTemplates_filters :: Lens.Lens' SearchFlowTemplates (Prelude.Maybe [FlowTemplateFilter])
searchFlowTemplates_filters = Lens.lens (\SearchFlowTemplates' {filters} -> filters) (\s@SearchFlowTemplates' {} a -> s {filters = a} :: SearchFlowTemplates) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in the response.
searchFlowTemplates_maxResults :: Lens.Lens' SearchFlowTemplates (Prelude.Maybe Prelude.Natural)
searchFlowTemplates_maxResults = Lens.lens (\SearchFlowTemplates' {maxResults} -> maxResults) (\s@SearchFlowTemplates' {} a -> s {maxResults = a} :: SearchFlowTemplates)

instance Core.AWSPager SearchFlowTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchFlowTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchFlowTemplatesResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchFlowTemplates_nextToken
          Lens..~ rs
          Lens.^? searchFlowTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchFlowTemplates where
  type
    AWSResponse SearchFlowTemplates =
      SearchFlowTemplatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFlowTemplatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchFlowTemplates where
  hashWithSalt _salt SearchFlowTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData SearchFlowTemplates where
  rnf SearchFlowTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders SearchFlowTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.SearchFlowTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchFlowTemplates where
  toJSON SearchFlowTemplates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath SearchFlowTemplates where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchFlowTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchFlowTemplatesResponse' smart constructor.
data SearchFlowTemplatesResponse = SearchFlowTemplatesResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain summary information about each workflow
    -- in the result set.
    summaries :: Prelude.Maybe [FlowTemplateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFlowTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchFlowTemplatesResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'summaries', 'searchFlowTemplatesResponse_summaries' - An array of objects that contain summary information about each workflow
-- in the result set.
--
-- 'httpStatus', 'searchFlowTemplatesResponse_httpStatus' - The response's http status code.
newSearchFlowTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchFlowTemplatesResponse
newSearchFlowTemplatesResponse pHttpStatus_ =
  SearchFlowTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
searchFlowTemplatesResponse_nextToken :: Lens.Lens' SearchFlowTemplatesResponse (Prelude.Maybe Prelude.Text)
searchFlowTemplatesResponse_nextToken = Lens.lens (\SearchFlowTemplatesResponse' {nextToken} -> nextToken) (\s@SearchFlowTemplatesResponse' {} a -> s {nextToken = a} :: SearchFlowTemplatesResponse)

-- | An array of objects that contain summary information about each workflow
-- in the result set.
searchFlowTemplatesResponse_summaries :: Lens.Lens' SearchFlowTemplatesResponse (Prelude.Maybe [FlowTemplateSummary])
searchFlowTemplatesResponse_summaries = Lens.lens (\SearchFlowTemplatesResponse' {summaries} -> summaries) (\s@SearchFlowTemplatesResponse' {} a -> s {summaries = a} :: SearchFlowTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchFlowTemplatesResponse_httpStatus :: Lens.Lens' SearchFlowTemplatesResponse Prelude.Int
searchFlowTemplatesResponse_httpStatus = Lens.lens (\SearchFlowTemplatesResponse' {httpStatus} -> httpStatus) (\s@SearchFlowTemplatesResponse' {} a -> s {httpStatus = a} :: SearchFlowTemplatesResponse)

instance Prelude.NFData SearchFlowTemplatesResponse where
  rnf SearchFlowTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaries
      `Prelude.seq` Prelude.rnf httpStatus
