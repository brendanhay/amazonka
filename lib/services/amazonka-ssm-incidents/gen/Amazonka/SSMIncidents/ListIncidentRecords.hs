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
-- Module      : Amazonka.SSMIncidents.ListIncidentRecords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all incident records in your account. Use this command to retrieve
-- the Amazon Resource Name (ARN) of the incident record you want to
-- update.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.ListIncidentRecords
  ( -- * Creating a Request
    ListIncidentRecords (..),
    newListIncidentRecords,

    -- * Request Lenses
    listIncidentRecords_filters,
    listIncidentRecords_maxResults,
    listIncidentRecords_nextToken,

    -- * Destructuring the Response
    ListIncidentRecordsResponse (..),
    newListIncidentRecordsResponse,

    -- * Response Lenses
    listIncidentRecordsResponse_nextToken,
    listIncidentRecordsResponse_httpStatus,
    listIncidentRecordsResponse_incidentRecordSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newListIncidentRecords' smart constructor.
data ListIncidentRecords = ListIncidentRecords'
  { -- | Filters the list of incident records through which you are searching.
    -- You can filter on the following keys:
    --
    -- -   @creationTime@
    --
    -- -   @impact@
    --
    -- -   @status@
    --
    -- -   @createdBy@
    --
    -- Note the following when deciding how to use Filters:
    --
    -- -   If you don\'t specify a Filter, the response includes all incident
    --     records.
    --
    -- -   If you specify more than one filter in a single request, the
    --     response returns incident records that match all filters.
    --
    -- -   If you specify a filter with more than one value, the response
    --     returns incident records that match any of the values provided.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIncidentRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listIncidentRecords_filters' - Filters the list of incident records through which you are searching.
-- You can filter on the following keys:
--
-- -   @creationTime@
--
-- -   @impact@
--
-- -   @status@
--
-- -   @createdBy@
--
-- Note the following when deciding how to use Filters:
--
-- -   If you don\'t specify a Filter, the response includes all incident
--     records.
--
-- -   If you specify more than one filter in a single request, the
--     response returns incident records that match all filters.
--
-- -   If you specify a filter with more than one value, the response
--     returns incident records that match any of the values provided.
--
-- 'maxResults', 'listIncidentRecords_maxResults' - The maximum number of results per page.
--
-- 'nextToken', 'listIncidentRecords_nextToken' - The pagination token to continue to the next page of results.
newListIncidentRecords ::
  ListIncidentRecords
newListIncidentRecords =
  ListIncidentRecords'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filters the list of incident records through which you are searching.
-- You can filter on the following keys:
--
-- -   @creationTime@
--
-- -   @impact@
--
-- -   @status@
--
-- -   @createdBy@
--
-- Note the following when deciding how to use Filters:
--
-- -   If you don\'t specify a Filter, the response includes all incident
--     records.
--
-- -   If you specify more than one filter in a single request, the
--     response returns incident records that match all filters.
--
-- -   If you specify a filter with more than one value, the response
--     returns incident records that match any of the values provided.
listIncidentRecords_filters :: Lens.Lens' ListIncidentRecords (Prelude.Maybe [Filter])
listIncidentRecords_filters = Lens.lens (\ListIncidentRecords' {filters} -> filters) (\s@ListIncidentRecords' {} a -> s {filters = a} :: ListIncidentRecords) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results per page.
listIncidentRecords_maxResults :: Lens.Lens' ListIncidentRecords (Prelude.Maybe Prelude.Natural)
listIncidentRecords_maxResults = Lens.lens (\ListIncidentRecords' {maxResults} -> maxResults) (\s@ListIncidentRecords' {} a -> s {maxResults = a} :: ListIncidentRecords)

-- | The pagination token to continue to the next page of results.
listIncidentRecords_nextToken :: Lens.Lens' ListIncidentRecords (Prelude.Maybe Prelude.Text)
listIncidentRecords_nextToken = Lens.lens (\ListIncidentRecords' {nextToken} -> nextToken) (\s@ListIncidentRecords' {} a -> s {nextToken = a} :: ListIncidentRecords)

instance Core.AWSPager ListIncidentRecords where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIncidentRecordsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listIncidentRecordsResponse_incidentRecordSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIncidentRecords_nextToken
          Lens..~ rs
          Lens.^? listIncidentRecordsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListIncidentRecords where
  type
    AWSResponse ListIncidentRecords =
      ListIncidentRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIncidentRecordsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "incidentRecordSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListIncidentRecords where
  hashWithSalt _salt ListIncidentRecords' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListIncidentRecords where
  rnf ListIncidentRecords' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListIncidentRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIncidentRecords where
  toJSON ListIncidentRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListIncidentRecords where
  toPath = Prelude.const "/listIncidentRecords"

instance Data.ToQuery ListIncidentRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIncidentRecordsResponse' smart constructor.
data ListIncidentRecordsResponse = ListIncidentRecordsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The details of each listed incident record.
    incidentRecordSummaries :: [IncidentRecordSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIncidentRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listIncidentRecordsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listIncidentRecordsResponse_httpStatus' - The response's http status code.
--
-- 'incidentRecordSummaries', 'listIncidentRecordsResponse_incidentRecordSummaries' - The details of each listed incident record.
newListIncidentRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIncidentRecordsResponse
newListIncidentRecordsResponse pHttpStatus_ =
  ListIncidentRecordsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      incidentRecordSummaries = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listIncidentRecordsResponse_nextToken :: Lens.Lens' ListIncidentRecordsResponse (Prelude.Maybe Prelude.Text)
listIncidentRecordsResponse_nextToken = Lens.lens (\ListIncidentRecordsResponse' {nextToken} -> nextToken) (\s@ListIncidentRecordsResponse' {} a -> s {nextToken = a} :: ListIncidentRecordsResponse)

-- | The response's http status code.
listIncidentRecordsResponse_httpStatus :: Lens.Lens' ListIncidentRecordsResponse Prelude.Int
listIncidentRecordsResponse_httpStatus = Lens.lens (\ListIncidentRecordsResponse' {httpStatus} -> httpStatus) (\s@ListIncidentRecordsResponse' {} a -> s {httpStatus = a} :: ListIncidentRecordsResponse)

-- | The details of each listed incident record.
listIncidentRecordsResponse_incidentRecordSummaries :: Lens.Lens' ListIncidentRecordsResponse [IncidentRecordSummary]
listIncidentRecordsResponse_incidentRecordSummaries = Lens.lens (\ListIncidentRecordsResponse' {incidentRecordSummaries} -> incidentRecordSummaries) (\s@ListIncidentRecordsResponse' {} a -> s {incidentRecordSummaries = a} :: ListIncidentRecordsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListIncidentRecordsResponse where
  rnf ListIncidentRecordsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf incidentRecordSummaries
