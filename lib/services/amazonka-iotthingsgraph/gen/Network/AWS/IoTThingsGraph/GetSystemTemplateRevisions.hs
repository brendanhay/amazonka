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
-- Module      : Amazonka.IoTThingsGraph.GetSystemTemplateRevisions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets revisions made to the specified system template. Only the previous
-- 100 revisions are stored. If the system has been deprecated, this action
-- will return the revisions that occurred before its deprecation. This
-- action won\'t work with systems that have been deleted.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.GetSystemTemplateRevisions
  ( -- * Creating a Request
    GetSystemTemplateRevisions (..),
    newGetSystemTemplateRevisions,

    -- * Request Lenses
    getSystemTemplateRevisions_nextToken,
    getSystemTemplateRevisions_maxResults,
    getSystemTemplateRevisions_id,

    -- * Destructuring the Response
    GetSystemTemplateRevisionsResponse (..),
    newGetSystemTemplateRevisionsResponse,

    -- * Response Lenses
    getSystemTemplateRevisionsResponse_nextToken,
    getSystemTemplateRevisionsResponse_summaries,
    getSystemTemplateRevisionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSystemTemplateRevisions' smart constructor.
data GetSystemTemplateRevisions = GetSystemTemplateRevisions'
  { -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the system template.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSystemTemplateRevisions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSystemTemplateRevisions_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'getSystemTemplateRevisions_maxResults' - The maximum number of results to return in the response.
--
-- 'id', 'getSystemTemplateRevisions_id' - The ID of the system template.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
newGetSystemTemplateRevisions ::
  -- | 'id'
  Prelude.Text ->
  GetSystemTemplateRevisions
newGetSystemTemplateRevisions pId_ =
  GetSystemTemplateRevisions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      id = pId_
    }

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
getSystemTemplateRevisions_nextToken :: Lens.Lens' GetSystemTemplateRevisions (Prelude.Maybe Prelude.Text)
getSystemTemplateRevisions_nextToken = Lens.lens (\GetSystemTemplateRevisions' {nextToken} -> nextToken) (\s@GetSystemTemplateRevisions' {} a -> s {nextToken = a} :: GetSystemTemplateRevisions)

-- | The maximum number of results to return in the response.
getSystemTemplateRevisions_maxResults :: Lens.Lens' GetSystemTemplateRevisions (Prelude.Maybe Prelude.Natural)
getSystemTemplateRevisions_maxResults = Lens.lens (\GetSystemTemplateRevisions' {maxResults} -> maxResults) (\s@GetSystemTemplateRevisions' {} a -> s {maxResults = a} :: GetSystemTemplateRevisions)

-- | The ID of the system template.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
getSystemTemplateRevisions_id :: Lens.Lens' GetSystemTemplateRevisions Prelude.Text
getSystemTemplateRevisions_id = Lens.lens (\GetSystemTemplateRevisions' {id} -> id) (\s@GetSystemTemplateRevisions' {} a -> s {id = a} :: GetSystemTemplateRevisions)

instance Core.AWSPager GetSystemTemplateRevisions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSystemTemplateRevisionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSystemTemplateRevisionsResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getSystemTemplateRevisions_nextToken
          Lens..~ rs
          Lens.^? getSystemTemplateRevisionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetSystemTemplateRevisions where
  type
    AWSResponse GetSystemTemplateRevisions =
      GetSystemTemplateRevisionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSystemTemplateRevisionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSystemTemplateRevisions

instance Prelude.NFData GetSystemTemplateRevisions

instance Core.ToHeaders GetSystemTemplateRevisions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.GetSystemTemplateRevisions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSystemTemplateRevisions where
  toJSON GetSystemTemplateRevisions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath GetSystemTemplateRevisions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSystemTemplateRevisions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSystemTemplateRevisionsResponse' smart constructor.
data GetSystemTemplateRevisionsResponse = GetSystemTemplateRevisionsResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain summary data about the system template
    -- revisions.
    summaries :: Prelude.Maybe [SystemTemplateSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSystemTemplateRevisionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSystemTemplateRevisionsResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'summaries', 'getSystemTemplateRevisionsResponse_summaries' - An array of objects that contain summary data about the system template
-- revisions.
--
-- 'httpStatus', 'getSystemTemplateRevisionsResponse_httpStatus' - The response's http status code.
newGetSystemTemplateRevisionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSystemTemplateRevisionsResponse
newGetSystemTemplateRevisionsResponse pHttpStatus_ =
  GetSystemTemplateRevisionsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
getSystemTemplateRevisionsResponse_nextToken :: Lens.Lens' GetSystemTemplateRevisionsResponse (Prelude.Maybe Prelude.Text)
getSystemTemplateRevisionsResponse_nextToken = Lens.lens (\GetSystemTemplateRevisionsResponse' {nextToken} -> nextToken) (\s@GetSystemTemplateRevisionsResponse' {} a -> s {nextToken = a} :: GetSystemTemplateRevisionsResponse)

-- | An array of objects that contain summary data about the system template
-- revisions.
getSystemTemplateRevisionsResponse_summaries :: Lens.Lens' GetSystemTemplateRevisionsResponse (Prelude.Maybe [SystemTemplateSummary])
getSystemTemplateRevisionsResponse_summaries = Lens.lens (\GetSystemTemplateRevisionsResponse' {summaries} -> summaries) (\s@GetSystemTemplateRevisionsResponse' {} a -> s {summaries = a} :: GetSystemTemplateRevisionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSystemTemplateRevisionsResponse_httpStatus :: Lens.Lens' GetSystemTemplateRevisionsResponse Prelude.Int
getSystemTemplateRevisionsResponse_httpStatus = Lens.lens (\GetSystemTemplateRevisionsResponse' {httpStatus} -> httpStatus) (\s@GetSystemTemplateRevisionsResponse' {} a -> s {httpStatus = a} :: GetSystemTemplateRevisionsResponse)

instance
  Prelude.NFData
    GetSystemTemplateRevisionsResponse
