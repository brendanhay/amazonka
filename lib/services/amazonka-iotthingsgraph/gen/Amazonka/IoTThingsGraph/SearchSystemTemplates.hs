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
-- Module      : Amazonka.IoTThingsGraph.SearchSystemTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for summary information about systems in the user\'s account.
-- You can filter by the ID of a workflow to return only systems that use
-- the specified workflow.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.SearchSystemTemplates
  ( -- * Creating a Request
    SearchSystemTemplates (..),
    newSearchSystemTemplates,

    -- * Request Lenses
    searchSystemTemplates_filters,
    searchSystemTemplates_nextToken,
    searchSystemTemplates_maxResults,

    -- * Destructuring the Response
    SearchSystemTemplatesResponse (..),
    newSearchSystemTemplatesResponse,

    -- * Response Lenses
    searchSystemTemplatesResponse_nextToken,
    searchSystemTemplatesResponse_summaries,
    searchSystemTemplatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchSystemTemplates' smart constructor.
data SearchSystemTemplates = SearchSystemTemplates'
  { -- | An array of filters that limit the result set. The only valid filter is
    -- @FLOW_TEMPLATE_ID@.
    filters :: Prelude.Maybe [SystemTemplateFilter],
    -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSystemTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchSystemTemplates_filters' - An array of filters that limit the result set. The only valid filter is
-- @FLOW_TEMPLATE_ID@.
--
-- 'nextToken', 'searchSystemTemplates_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'searchSystemTemplates_maxResults' - The maximum number of results to return in the response.
newSearchSystemTemplates ::
  SearchSystemTemplates
newSearchSystemTemplates =
  SearchSystemTemplates'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An array of filters that limit the result set. The only valid filter is
-- @FLOW_TEMPLATE_ID@.
searchSystemTemplates_filters :: Lens.Lens' SearchSystemTemplates (Prelude.Maybe [SystemTemplateFilter])
searchSystemTemplates_filters = Lens.lens (\SearchSystemTemplates' {filters} -> filters) (\s@SearchSystemTemplates' {} a -> s {filters = a} :: SearchSystemTemplates) Prelude.. Lens.mapping Lens.coerced

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
searchSystemTemplates_nextToken :: Lens.Lens' SearchSystemTemplates (Prelude.Maybe Prelude.Text)
searchSystemTemplates_nextToken = Lens.lens (\SearchSystemTemplates' {nextToken} -> nextToken) (\s@SearchSystemTemplates' {} a -> s {nextToken = a} :: SearchSystemTemplates)

-- | The maximum number of results to return in the response.
searchSystemTemplates_maxResults :: Lens.Lens' SearchSystemTemplates (Prelude.Maybe Prelude.Natural)
searchSystemTemplates_maxResults = Lens.lens (\SearchSystemTemplates' {maxResults} -> maxResults) (\s@SearchSystemTemplates' {} a -> s {maxResults = a} :: SearchSystemTemplates)

instance Core.AWSPager SearchSystemTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchSystemTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchSystemTemplatesResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchSystemTemplates_nextToken
          Lens..~ rs
          Lens.^? searchSystemTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchSystemTemplates where
  type
    AWSResponse SearchSystemTemplates =
      SearchSystemTemplatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSystemTemplatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchSystemTemplates

instance Prelude.NFData SearchSystemTemplates

instance Core.ToHeaders SearchSystemTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.SearchSystemTemplates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchSystemTemplates where
  toJSON SearchSystemTemplates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath SearchSystemTemplates where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchSystemTemplates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchSystemTemplatesResponse' smart constructor.
data SearchSystemTemplatesResponse = SearchSystemTemplatesResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain summary information about each system
    -- deployment in the result set.
    summaries :: Prelude.Maybe [SystemTemplateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSystemTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSystemTemplatesResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'summaries', 'searchSystemTemplatesResponse_summaries' - An array of objects that contain summary information about each system
-- deployment in the result set.
--
-- 'httpStatus', 'searchSystemTemplatesResponse_httpStatus' - The response's http status code.
newSearchSystemTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchSystemTemplatesResponse
newSearchSystemTemplatesResponse pHttpStatus_ =
  SearchSystemTemplatesResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
searchSystemTemplatesResponse_nextToken :: Lens.Lens' SearchSystemTemplatesResponse (Prelude.Maybe Prelude.Text)
searchSystemTemplatesResponse_nextToken = Lens.lens (\SearchSystemTemplatesResponse' {nextToken} -> nextToken) (\s@SearchSystemTemplatesResponse' {} a -> s {nextToken = a} :: SearchSystemTemplatesResponse)

-- | An array of objects that contain summary information about each system
-- deployment in the result set.
searchSystemTemplatesResponse_summaries :: Lens.Lens' SearchSystemTemplatesResponse (Prelude.Maybe [SystemTemplateSummary])
searchSystemTemplatesResponse_summaries = Lens.lens (\SearchSystemTemplatesResponse' {summaries} -> summaries) (\s@SearchSystemTemplatesResponse' {} a -> s {summaries = a} :: SearchSystemTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchSystemTemplatesResponse_httpStatus :: Lens.Lens' SearchSystemTemplatesResponse Prelude.Int
searchSystemTemplatesResponse_httpStatus = Lens.lens (\SearchSystemTemplatesResponse' {httpStatus} -> httpStatus) (\s@SearchSystemTemplatesResponse' {} a -> s {httpStatus = a} :: SearchSystemTemplatesResponse)

instance Prelude.NFData SearchSystemTemplatesResponse
