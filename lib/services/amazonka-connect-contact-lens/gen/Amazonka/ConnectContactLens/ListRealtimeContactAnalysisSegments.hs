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
-- Module      : Amazonka.ConnectContactLens.ListRealtimeContactAnalysisSegments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of analysis segments for a real-time analysis session.
module Amazonka.ConnectContactLens.ListRealtimeContactAnalysisSegments
  ( -- * Creating a Request
    ListRealtimeContactAnalysisSegments (..),
    newListRealtimeContactAnalysisSegments,

    -- * Request Lenses
    listRealtimeContactAnalysisSegments_maxResults,
    listRealtimeContactAnalysisSegments_nextToken,
    listRealtimeContactAnalysisSegments_instanceId,
    listRealtimeContactAnalysisSegments_contactId,

    -- * Destructuring the Response
    ListRealtimeContactAnalysisSegmentsResponse (..),
    newListRealtimeContactAnalysisSegmentsResponse,

    -- * Response Lenses
    listRealtimeContactAnalysisSegmentsResponse_nextToken,
    listRealtimeContactAnalysisSegmentsResponse_httpStatus,
    listRealtimeContactAnalysisSegmentsResponse_segments,
  )
where

import Amazonka.ConnectContactLens.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRealtimeContactAnalysisSegments' smart constructor.
data ListRealtimeContactAnalysisSegments = ListRealtimeContactAnalysisSegments'
  { -- | The maximimum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRealtimeContactAnalysisSegments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRealtimeContactAnalysisSegments_maxResults' - The maximimum number of results to return per page.
--
-- 'nextToken', 'listRealtimeContactAnalysisSegments_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listRealtimeContactAnalysisSegments_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactId', 'listRealtimeContactAnalysisSegments_contactId' - The identifier of the contact.
newListRealtimeContactAnalysisSegments ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  ListRealtimeContactAnalysisSegments
newListRealtimeContactAnalysisSegments
  pInstanceId_
  pContactId_ =
    ListRealtimeContactAnalysisSegments'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        contactId = pContactId_
      }

-- | The maximimum number of results to return per page.
listRealtimeContactAnalysisSegments_maxResults :: Lens.Lens' ListRealtimeContactAnalysisSegments (Prelude.Maybe Prelude.Natural)
listRealtimeContactAnalysisSegments_maxResults = Lens.lens (\ListRealtimeContactAnalysisSegments' {maxResults} -> maxResults) (\s@ListRealtimeContactAnalysisSegments' {} a -> s {maxResults = a} :: ListRealtimeContactAnalysisSegments)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listRealtimeContactAnalysisSegments_nextToken :: Lens.Lens' ListRealtimeContactAnalysisSegments (Prelude.Maybe Prelude.Text)
listRealtimeContactAnalysisSegments_nextToken = Lens.lens (\ListRealtimeContactAnalysisSegments' {nextToken} -> nextToken) (\s@ListRealtimeContactAnalysisSegments' {} a -> s {nextToken = a} :: ListRealtimeContactAnalysisSegments)

-- | The identifier of the Amazon Connect instance.
listRealtimeContactAnalysisSegments_instanceId :: Lens.Lens' ListRealtimeContactAnalysisSegments Prelude.Text
listRealtimeContactAnalysisSegments_instanceId = Lens.lens (\ListRealtimeContactAnalysisSegments' {instanceId} -> instanceId) (\s@ListRealtimeContactAnalysisSegments' {} a -> s {instanceId = a} :: ListRealtimeContactAnalysisSegments)

-- | The identifier of the contact.
listRealtimeContactAnalysisSegments_contactId :: Lens.Lens' ListRealtimeContactAnalysisSegments Prelude.Text
listRealtimeContactAnalysisSegments_contactId = Lens.lens (\ListRealtimeContactAnalysisSegments' {contactId} -> contactId) (\s@ListRealtimeContactAnalysisSegments' {} a -> s {contactId = a} :: ListRealtimeContactAnalysisSegments)

instance
  Core.AWSRequest
    ListRealtimeContactAnalysisSegments
  where
  type
    AWSResponse ListRealtimeContactAnalysisSegments =
      ListRealtimeContactAnalysisSegmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRealtimeContactAnalysisSegmentsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..?> "Segments" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    ListRealtimeContactAnalysisSegments
  where
  hashWithSalt
    _salt
    ListRealtimeContactAnalysisSegments' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` contactId

instance
  Prelude.NFData
    ListRealtimeContactAnalysisSegments
  where
  rnf ListRealtimeContactAnalysisSegments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId

instance
  Data.ToHeaders
    ListRealtimeContactAnalysisSegments
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListRealtimeContactAnalysisSegments
  where
  toJSON ListRealtimeContactAnalysisSegments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("ContactId" Data..= contactId)
          ]
      )

instance
  Data.ToPath
    ListRealtimeContactAnalysisSegments
  where
  toPath =
    Prelude.const
      "/realtime-contact-analysis/analysis-segments"

instance
  Data.ToQuery
    ListRealtimeContactAnalysisSegments
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRealtimeContactAnalysisSegmentsResponse' smart constructor.
data ListRealtimeContactAnalysisSegmentsResponse = ListRealtimeContactAnalysisSegmentsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results. If response includes @nextToken@ there are two possible
    -- scenarios:
    --
    -- -   There are more segments so another call is required to get them.
    --
    -- -   There are no more segments at this time, but more may be available
    --     later (real-time analysis is in progress) so the client should call
    --     the operation again to get new segments.
    --
    -- If response does not include @nextToken@, the analysis is completed
    -- (successfully or failed) and there are no more segments to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An analyzed transcript or category.
    segments :: [RealtimeContactAnalysisSegment]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRealtimeContactAnalysisSegmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRealtimeContactAnalysisSegmentsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results. If response includes @nextToken@ there are two possible
-- scenarios:
--
-- -   There are more segments so another call is required to get them.
--
-- -   There are no more segments at this time, but more may be available
--     later (real-time analysis is in progress) so the client should call
--     the operation again to get new segments.
--
-- If response does not include @nextToken@, the analysis is completed
-- (successfully or failed) and there are no more segments to retrieve.
--
-- 'httpStatus', 'listRealtimeContactAnalysisSegmentsResponse_httpStatus' - The response's http status code.
--
-- 'segments', 'listRealtimeContactAnalysisSegmentsResponse_segments' - An analyzed transcript or category.
newListRealtimeContactAnalysisSegmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRealtimeContactAnalysisSegmentsResponse
newListRealtimeContactAnalysisSegmentsResponse
  pHttpStatus_ =
    ListRealtimeContactAnalysisSegmentsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        segments = Prelude.mempty
      }

-- | If there are additional results, this is the token for the next set of
-- results. If response includes @nextToken@ there are two possible
-- scenarios:
--
-- -   There are more segments so another call is required to get them.
--
-- -   There are no more segments at this time, but more may be available
--     later (real-time analysis is in progress) so the client should call
--     the operation again to get new segments.
--
-- If response does not include @nextToken@, the analysis is completed
-- (successfully or failed) and there are no more segments to retrieve.
listRealtimeContactAnalysisSegmentsResponse_nextToken :: Lens.Lens' ListRealtimeContactAnalysisSegmentsResponse (Prelude.Maybe Prelude.Text)
listRealtimeContactAnalysisSegmentsResponse_nextToken = Lens.lens (\ListRealtimeContactAnalysisSegmentsResponse' {nextToken} -> nextToken) (\s@ListRealtimeContactAnalysisSegmentsResponse' {} a -> s {nextToken = a} :: ListRealtimeContactAnalysisSegmentsResponse)

-- | The response's http status code.
listRealtimeContactAnalysisSegmentsResponse_httpStatus :: Lens.Lens' ListRealtimeContactAnalysisSegmentsResponse Prelude.Int
listRealtimeContactAnalysisSegmentsResponse_httpStatus = Lens.lens (\ListRealtimeContactAnalysisSegmentsResponse' {httpStatus} -> httpStatus) (\s@ListRealtimeContactAnalysisSegmentsResponse' {} a -> s {httpStatus = a} :: ListRealtimeContactAnalysisSegmentsResponse)

-- | An analyzed transcript or category.
listRealtimeContactAnalysisSegmentsResponse_segments :: Lens.Lens' ListRealtimeContactAnalysisSegmentsResponse [RealtimeContactAnalysisSegment]
listRealtimeContactAnalysisSegmentsResponse_segments = Lens.lens (\ListRealtimeContactAnalysisSegmentsResponse' {segments} -> segments) (\s@ListRealtimeContactAnalysisSegmentsResponse' {} a -> s {segments = a} :: ListRealtimeContactAnalysisSegmentsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListRealtimeContactAnalysisSegmentsResponse
  where
  rnf ListRealtimeContactAnalysisSegmentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segments
