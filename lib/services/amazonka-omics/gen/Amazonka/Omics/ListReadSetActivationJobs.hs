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
-- Module      : Amazonka.Omics.ListReadSetActivationJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of read set activation jobs.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReadSetActivationJobs
  ( -- * Creating a Request
    ListReadSetActivationJobs (..),
    newListReadSetActivationJobs,

    -- * Request Lenses
    listReadSetActivationJobs_filter,
    listReadSetActivationJobs_maxResults,
    listReadSetActivationJobs_nextToken,
    listReadSetActivationJobs_sequenceStoreId,

    -- * Destructuring the Response
    ListReadSetActivationJobsResponse (..),
    newListReadSetActivationJobsResponse,

    -- * Response Lenses
    listReadSetActivationJobsResponse_activationJobs,
    listReadSetActivationJobsResponse_nextToken,
    listReadSetActivationJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReadSetActivationJobs' smart constructor.
data ListReadSetActivationJobs = ListReadSetActivationJobs'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ActivateReadSetFilter,
    -- | The maximum number of read set activation jobs to return in one page of
    -- results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The read set\'s sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetActivationJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReadSetActivationJobs_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReadSetActivationJobs_maxResults' - The maximum number of read set activation jobs to return in one page of
-- results.
--
-- 'nextToken', 'listReadSetActivationJobs_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'sequenceStoreId', 'listReadSetActivationJobs_sequenceStoreId' - The read set\'s sequence store ID.
newListReadSetActivationJobs ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  ListReadSetActivationJobs
newListReadSetActivationJobs pSequenceStoreId_ =
  ListReadSetActivationJobs'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sequenceStoreId = pSequenceStoreId_
    }

-- | A filter to apply to the list.
listReadSetActivationJobs_filter :: Lens.Lens' ListReadSetActivationJobs (Prelude.Maybe ActivateReadSetFilter)
listReadSetActivationJobs_filter = Lens.lens (\ListReadSetActivationJobs' {filter'} -> filter') (\s@ListReadSetActivationJobs' {} a -> s {filter' = a} :: ListReadSetActivationJobs)

-- | The maximum number of read set activation jobs to return in one page of
-- results.
listReadSetActivationJobs_maxResults :: Lens.Lens' ListReadSetActivationJobs (Prelude.Maybe Prelude.Natural)
listReadSetActivationJobs_maxResults = Lens.lens (\ListReadSetActivationJobs' {maxResults} -> maxResults) (\s@ListReadSetActivationJobs' {} a -> s {maxResults = a} :: ListReadSetActivationJobs)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReadSetActivationJobs_nextToken :: Lens.Lens' ListReadSetActivationJobs (Prelude.Maybe Prelude.Text)
listReadSetActivationJobs_nextToken = Lens.lens (\ListReadSetActivationJobs' {nextToken} -> nextToken) (\s@ListReadSetActivationJobs' {} a -> s {nextToken = a} :: ListReadSetActivationJobs)

-- | The read set\'s sequence store ID.
listReadSetActivationJobs_sequenceStoreId :: Lens.Lens' ListReadSetActivationJobs Prelude.Text
listReadSetActivationJobs_sequenceStoreId = Lens.lens (\ListReadSetActivationJobs' {sequenceStoreId} -> sequenceStoreId) (\s@ListReadSetActivationJobs' {} a -> s {sequenceStoreId = a} :: ListReadSetActivationJobs)

instance Core.AWSPager ListReadSetActivationJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReadSetActivationJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listReadSetActivationJobsResponse_activationJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listReadSetActivationJobs_nextToken
          Lens..~ rs
          Lens.^? listReadSetActivationJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListReadSetActivationJobs where
  type
    AWSResponse ListReadSetActivationJobs =
      ListReadSetActivationJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadSetActivationJobsResponse'
            Prelude.<$> (x Data..?> "activationJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReadSetActivationJobs where
  hashWithSalt _salt ListReadSetActivationJobs' {..} =
    _salt `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData ListReadSetActivationJobs where
  rnf ListReadSetActivationJobs' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders ListReadSetActivationJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReadSetActivationJobs where
  toJSON ListReadSetActivationJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReadSetActivationJobs where
  toPath ListReadSetActivationJobs' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/activationjobs"
      ]

instance Data.ToQuery ListReadSetActivationJobs where
  toQuery ListReadSetActivationJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReadSetActivationJobsResponse' smart constructor.
data ListReadSetActivationJobsResponse = ListReadSetActivationJobsResponse'
  { -- | A list of jobs.
    activationJobs :: Prelude.Maybe [ActivateReadSetJobItem],
    -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetActivationJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationJobs', 'listReadSetActivationJobsResponse_activationJobs' - A list of jobs.
--
-- 'nextToken', 'listReadSetActivationJobsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReadSetActivationJobsResponse_httpStatus' - The response's http status code.
newListReadSetActivationJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadSetActivationJobsResponse
newListReadSetActivationJobsResponse pHttpStatus_ =
  ListReadSetActivationJobsResponse'
    { activationJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of jobs.
listReadSetActivationJobsResponse_activationJobs :: Lens.Lens' ListReadSetActivationJobsResponse (Prelude.Maybe [ActivateReadSetJobItem])
listReadSetActivationJobsResponse_activationJobs = Lens.lens (\ListReadSetActivationJobsResponse' {activationJobs} -> activationJobs) (\s@ListReadSetActivationJobsResponse' {} a -> s {activationJobs = a} :: ListReadSetActivationJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token that\'s included if more results are available.
listReadSetActivationJobsResponse_nextToken :: Lens.Lens' ListReadSetActivationJobsResponse (Prelude.Maybe Prelude.Text)
listReadSetActivationJobsResponse_nextToken = Lens.lens (\ListReadSetActivationJobsResponse' {nextToken} -> nextToken) (\s@ListReadSetActivationJobsResponse' {} a -> s {nextToken = a} :: ListReadSetActivationJobsResponse)

-- | The response's http status code.
listReadSetActivationJobsResponse_httpStatus :: Lens.Lens' ListReadSetActivationJobsResponse Prelude.Int
listReadSetActivationJobsResponse_httpStatus = Lens.lens (\ListReadSetActivationJobsResponse' {httpStatus} -> httpStatus) (\s@ListReadSetActivationJobsResponse' {} a -> s {httpStatus = a} :: ListReadSetActivationJobsResponse)

instance
  Prelude.NFData
    ListReadSetActivationJobsResponse
  where
  rnf ListReadSetActivationJobsResponse' {..} =
    Prelude.rnf activationJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
