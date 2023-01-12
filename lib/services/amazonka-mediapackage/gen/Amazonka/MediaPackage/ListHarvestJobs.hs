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
-- Module      : Amazonka.MediaPackage.ListHarvestJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of HarvestJob records.
--
-- This operation returns paginated results.
module Amazonka.MediaPackage.ListHarvestJobs
  ( -- * Creating a Request
    ListHarvestJobs (..),
    newListHarvestJobs,

    -- * Request Lenses
    listHarvestJobs_includeChannelId,
    listHarvestJobs_includeStatus,
    listHarvestJobs_maxResults,
    listHarvestJobs_nextToken,

    -- * Destructuring the Response
    ListHarvestJobsResponse (..),
    newListHarvestJobsResponse,

    -- * Response Lenses
    listHarvestJobsResponse_harvestJobs,
    listHarvestJobsResponse_nextToken,
    listHarvestJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHarvestJobs' smart constructor.
data ListHarvestJobs = ListHarvestJobs'
  { -- | When specified, the request will return only HarvestJobs associated with
    -- the given Channel ID.
    includeChannelId :: Prelude.Maybe Prelude.Text,
    -- | When specified, the request will return only HarvestJobs in the given
    -- status.
    includeStatus :: Prelude.Maybe Prelude.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHarvestJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeChannelId', 'listHarvestJobs_includeChannelId' - When specified, the request will return only HarvestJobs associated with
-- the given Channel ID.
--
-- 'includeStatus', 'listHarvestJobs_includeStatus' - When specified, the request will return only HarvestJobs in the given
-- status.
--
-- 'maxResults', 'listHarvestJobs_maxResults' - The upper bound on the number of records to return.
--
-- 'nextToken', 'listHarvestJobs_nextToken' - A token used to resume pagination from the end of a previous request.
newListHarvestJobs ::
  ListHarvestJobs
newListHarvestJobs =
  ListHarvestJobs'
    { includeChannelId =
        Prelude.Nothing,
      includeStatus = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | When specified, the request will return only HarvestJobs associated with
-- the given Channel ID.
listHarvestJobs_includeChannelId :: Lens.Lens' ListHarvestJobs (Prelude.Maybe Prelude.Text)
listHarvestJobs_includeChannelId = Lens.lens (\ListHarvestJobs' {includeChannelId} -> includeChannelId) (\s@ListHarvestJobs' {} a -> s {includeChannelId = a} :: ListHarvestJobs)

-- | When specified, the request will return only HarvestJobs in the given
-- status.
listHarvestJobs_includeStatus :: Lens.Lens' ListHarvestJobs (Prelude.Maybe Prelude.Text)
listHarvestJobs_includeStatus = Lens.lens (\ListHarvestJobs' {includeStatus} -> includeStatus) (\s@ListHarvestJobs' {} a -> s {includeStatus = a} :: ListHarvestJobs)

-- | The upper bound on the number of records to return.
listHarvestJobs_maxResults :: Lens.Lens' ListHarvestJobs (Prelude.Maybe Prelude.Natural)
listHarvestJobs_maxResults = Lens.lens (\ListHarvestJobs' {maxResults} -> maxResults) (\s@ListHarvestJobs' {} a -> s {maxResults = a} :: ListHarvestJobs)

-- | A token used to resume pagination from the end of a previous request.
listHarvestJobs_nextToken :: Lens.Lens' ListHarvestJobs (Prelude.Maybe Prelude.Text)
listHarvestJobs_nextToken = Lens.lens (\ListHarvestJobs' {nextToken} -> nextToken) (\s@ListHarvestJobs' {} a -> s {nextToken = a} :: ListHarvestJobs)

instance Core.AWSPager ListHarvestJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHarvestJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listHarvestJobsResponse_harvestJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHarvestJobs_nextToken
          Lens..~ rs
          Lens.^? listHarvestJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListHarvestJobs where
  type
    AWSResponse ListHarvestJobs =
      ListHarvestJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHarvestJobsResponse'
            Prelude.<$> (x Data..?> "harvestJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHarvestJobs where
  hashWithSalt _salt ListHarvestJobs' {..} =
    _salt `Prelude.hashWithSalt` includeChannelId
      `Prelude.hashWithSalt` includeStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListHarvestJobs where
  rnf ListHarvestJobs' {..} =
    Prelude.rnf includeChannelId
      `Prelude.seq` Prelude.rnf includeStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListHarvestJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListHarvestJobs where
  toPath = Prelude.const "/harvest_jobs"

instance Data.ToQuery ListHarvestJobs where
  toQuery ListHarvestJobs' {..} =
    Prelude.mconcat
      [ "includeChannelId" Data.=: includeChannelId,
        "includeStatus" Data.=: includeStatus,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListHarvestJobsResponse' smart constructor.
data ListHarvestJobsResponse = ListHarvestJobsResponse'
  { -- | A list of HarvestJob records.
    harvestJobs :: Prelude.Maybe [HarvestJob],
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHarvestJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'harvestJobs', 'listHarvestJobsResponse_harvestJobs' - A list of HarvestJob records.
--
-- 'nextToken', 'listHarvestJobsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'listHarvestJobsResponse_httpStatus' - The response's http status code.
newListHarvestJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHarvestJobsResponse
newListHarvestJobsResponse pHttpStatus_ =
  ListHarvestJobsResponse'
    { harvestJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of HarvestJob records.
listHarvestJobsResponse_harvestJobs :: Lens.Lens' ListHarvestJobsResponse (Prelude.Maybe [HarvestJob])
listHarvestJobsResponse_harvestJobs = Lens.lens (\ListHarvestJobsResponse' {harvestJobs} -> harvestJobs) (\s@ListHarvestJobsResponse' {} a -> s {harvestJobs = a} :: ListHarvestJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to resume pagination from the end of the
-- collection.
listHarvestJobsResponse_nextToken :: Lens.Lens' ListHarvestJobsResponse (Prelude.Maybe Prelude.Text)
listHarvestJobsResponse_nextToken = Lens.lens (\ListHarvestJobsResponse' {nextToken} -> nextToken) (\s@ListHarvestJobsResponse' {} a -> s {nextToken = a} :: ListHarvestJobsResponse)

-- | The response's http status code.
listHarvestJobsResponse_httpStatus :: Lens.Lens' ListHarvestJobsResponse Prelude.Int
listHarvestJobsResponse_httpStatus = Lens.lens (\ListHarvestJobsResponse' {httpStatus} -> httpStatus) (\s@ListHarvestJobsResponse' {} a -> s {httpStatus = a} :: ListHarvestJobsResponse)

instance Prelude.NFData ListHarvestJobsResponse where
  rnf ListHarvestJobsResponse' {..} =
    Prelude.rnf harvestJobs
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
