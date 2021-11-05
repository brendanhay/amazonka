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
-- Module      : Amazonka.IoTThingsGraph.GetFlowTemplateRevisions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets revisions of the specified workflow. Only the last 100 revisions
-- are stored. If the workflow has been deprecated, this action will return
-- revisions that occurred before the deprecation. This action won\'t work
-- for workflows that have been deleted.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.GetFlowTemplateRevisions
  ( -- * Creating a Request
    GetFlowTemplateRevisions (..),
    newGetFlowTemplateRevisions,

    -- * Request Lenses
    getFlowTemplateRevisions_nextToken,
    getFlowTemplateRevisions_maxResults,
    getFlowTemplateRevisions_id,

    -- * Destructuring the Response
    GetFlowTemplateRevisionsResponse (..),
    newGetFlowTemplateRevisionsResponse,

    -- * Response Lenses
    getFlowTemplateRevisionsResponse_nextToken,
    getFlowTemplateRevisionsResponse_summaries,
    getFlowTemplateRevisionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFlowTemplateRevisions' smart constructor.
data GetFlowTemplateRevisions = GetFlowTemplateRevisions'
  { -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the workflow.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFlowTemplateRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFlowTemplateRevisions_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'getFlowTemplateRevisions_maxResults' - The maximum number of results to return in the response.
--
-- 'id', 'getFlowTemplateRevisions_id' - The ID of the workflow.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
newGetFlowTemplateRevisions ::
  -- | 'id'
  Prelude.Text ->
  GetFlowTemplateRevisions
newGetFlowTemplateRevisions pId_ =
  GetFlowTemplateRevisions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      id = pId_
    }

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
getFlowTemplateRevisions_nextToken :: Lens.Lens' GetFlowTemplateRevisions (Prelude.Maybe Prelude.Text)
getFlowTemplateRevisions_nextToken = Lens.lens (\GetFlowTemplateRevisions' {nextToken} -> nextToken) (\s@GetFlowTemplateRevisions' {} a -> s {nextToken = a} :: GetFlowTemplateRevisions)

-- | The maximum number of results to return in the response.
getFlowTemplateRevisions_maxResults :: Lens.Lens' GetFlowTemplateRevisions (Prelude.Maybe Prelude.Natural)
getFlowTemplateRevisions_maxResults = Lens.lens (\GetFlowTemplateRevisions' {maxResults} -> maxResults) (\s@GetFlowTemplateRevisions' {} a -> s {maxResults = a} :: GetFlowTemplateRevisions)

-- | The ID of the workflow.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
getFlowTemplateRevisions_id :: Lens.Lens' GetFlowTemplateRevisions Prelude.Text
getFlowTemplateRevisions_id = Lens.lens (\GetFlowTemplateRevisions' {id} -> id) (\s@GetFlowTemplateRevisions' {} a -> s {id = a} :: GetFlowTemplateRevisions)

instance Core.AWSPager GetFlowTemplateRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getFlowTemplateRevisionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getFlowTemplateRevisionsResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getFlowTemplateRevisions_nextToken
          Lens..~ rs
          Lens.^? getFlowTemplateRevisionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetFlowTemplateRevisions where
  type
    AWSResponse GetFlowTemplateRevisions =
      GetFlowTemplateRevisionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFlowTemplateRevisionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFlowTemplateRevisions

instance Prelude.NFData GetFlowTemplateRevisions

instance Core.ToHeaders GetFlowTemplateRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetFlowTemplateRevisions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetFlowTemplateRevisions where
  toJSON GetFlowTemplateRevisions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath GetFlowTemplateRevisions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetFlowTemplateRevisions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFlowTemplateRevisionsResponse' smart constructor.
data GetFlowTemplateRevisionsResponse = GetFlowTemplateRevisionsResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that provide summary data about each revision.
    summaries :: Prelude.Maybe [FlowTemplateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFlowTemplateRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getFlowTemplateRevisionsResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'summaries', 'getFlowTemplateRevisionsResponse_summaries' - An array of objects that provide summary data about each revision.
--
-- 'httpStatus', 'getFlowTemplateRevisionsResponse_httpStatus' - The response's http status code.
newGetFlowTemplateRevisionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFlowTemplateRevisionsResponse
newGetFlowTemplateRevisionsResponse pHttpStatus_ =
  GetFlowTemplateRevisionsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
getFlowTemplateRevisionsResponse_nextToken :: Lens.Lens' GetFlowTemplateRevisionsResponse (Prelude.Maybe Prelude.Text)
getFlowTemplateRevisionsResponse_nextToken = Lens.lens (\GetFlowTemplateRevisionsResponse' {nextToken} -> nextToken) (\s@GetFlowTemplateRevisionsResponse' {} a -> s {nextToken = a} :: GetFlowTemplateRevisionsResponse)

-- | An array of objects that provide summary data about each revision.
getFlowTemplateRevisionsResponse_summaries :: Lens.Lens' GetFlowTemplateRevisionsResponse (Prelude.Maybe [FlowTemplateSummary])
getFlowTemplateRevisionsResponse_summaries = Lens.lens (\GetFlowTemplateRevisionsResponse' {summaries} -> summaries) (\s@GetFlowTemplateRevisionsResponse' {} a -> s {summaries = a} :: GetFlowTemplateRevisionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getFlowTemplateRevisionsResponse_httpStatus :: Lens.Lens' GetFlowTemplateRevisionsResponse Prelude.Int
getFlowTemplateRevisionsResponse_httpStatus = Lens.lens (\GetFlowTemplateRevisionsResponse' {httpStatus} -> httpStatus) (\s@GetFlowTemplateRevisionsResponse' {} a -> s {httpStatus = a} :: GetFlowTemplateRevisionsResponse)

instance
  Prelude.NFData
    GetFlowTemplateRevisionsResponse
