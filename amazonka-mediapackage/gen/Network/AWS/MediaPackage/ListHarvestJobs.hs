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
-- Module      : Network.AWS.MediaPackage.ListHarvestJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of HarvestJob records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListHarvestJobs
  ( -- * Creating a Request
    ListHarvestJobs (..),
    newListHarvestJobs,

    -- * Request Lenses
    listHarvestJobs_nextToken,
    listHarvestJobs_maxResults,
    listHarvestJobs_includeStatus,
    listHarvestJobs_includeChannelId,

    -- * Destructuring the Response
    ListHarvestJobsResponse (..),
    newListHarvestJobsResponse,

    -- * Response Lenses
    listHarvestJobsResponse_nextToken,
    listHarvestJobsResponse_harvestJobs,
    listHarvestJobsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHarvestJobs' smart constructor.
data ListHarvestJobs = ListHarvestJobs'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Core.Maybe Core.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | When specified, the request will return only HarvestJobs in the given
    -- status.
    includeStatus :: Core.Maybe Core.Text,
    -- | When specified, the request will return only HarvestJobs associated with
    -- the given Channel ID.
    includeChannelId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHarvestJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHarvestJobs_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listHarvestJobs_maxResults' - The upper bound on the number of records to return.
--
-- 'includeStatus', 'listHarvestJobs_includeStatus' - When specified, the request will return only HarvestJobs in the given
-- status.
--
-- 'includeChannelId', 'listHarvestJobs_includeChannelId' - When specified, the request will return only HarvestJobs associated with
-- the given Channel ID.
newListHarvestJobs ::
  ListHarvestJobs
newListHarvestJobs =
  ListHarvestJobs'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      includeStatus = Core.Nothing,
      includeChannelId = Core.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listHarvestJobs_nextToken :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Text)
listHarvestJobs_nextToken = Lens.lens (\ListHarvestJobs' {nextToken} -> nextToken) (\s@ListHarvestJobs' {} a -> s {nextToken = a} :: ListHarvestJobs)

-- | The upper bound on the number of records to return.
listHarvestJobs_maxResults :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Natural)
listHarvestJobs_maxResults = Lens.lens (\ListHarvestJobs' {maxResults} -> maxResults) (\s@ListHarvestJobs' {} a -> s {maxResults = a} :: ListHarvestJobs)

-- | When specified, the request will return only HarvestJobs in the given
-- status.
listHarvestJobs_includeStatus :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Text)
listHarvestJobs_includeStatus = Lens.lens (\ListHarvestJobs' {includeStatus} -> includeStatus) (\s@ListHarvestJobs' {} a -> s {includeStatus = a} :: ListHarvestJobs)

-- | When specified, the request will return only HarvestJobs associated with
-- the given Channel ID.
listHarvestJobs_includeChannelId :: Lens.Lens' ListHarvestJobs (Core.Maybe Core.Text)
listHarvestJobs_includeChannelId = Lens.lens (\ListHarvestJobs' {includeChannelId} -> includeChannelId) (\s@ListHarvestJobs' {} a -> s {includeChannelId = a} :: ListHarvestJobs)

instance Core.AWSPager ListHarvestJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHarvestJobsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listHarvestJobsResponse_harvestJobs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHarvestJobs_nextToken
          Lens..~ rs
          Lens.^? listHarvestJobsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListHarvestJobs where
  type
    AWSResponse ListHarvestJobs =
      ListHarvestJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHarvestJobsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "harvestJobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListHarvestJobs

instance Core.NFData ListHarvestJobs

instance Core.ToHeaders ListHarvestJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListHarvestJobs where
  toPath = Core.const "/harvest_jobs"

instance Core.ToQuery ListHarvestJobs where
  toQuery ListHarvestJobs' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "includeStatus" Core.=: includeStatus,
        "includeChannelId" Core.=: includeChannelId
      ]

-- | /See:/ 'newListHarvestJobsResponse' smart constructor.
data ListHarvestJobsResponse = ListHarvestJobsResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of HarvestJob records.
    harvestJobs :: Core.Maybe [HarvestJob],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHarvestJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHarvestJobsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'harvestJobs', 'listHarvestJobsResponse_harvestJobs' - A list of HarvestJob records.
--
-- 'httpStatus', 'listHarvestJobsResponse_httpStatus' - The response's http status code.
newListHarvestJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListHarvestJobsResponse
newListHarvestJobsResponse pHttpStatus_ =
  ListHarvestJobsResponse'
    { nextToken = Core.Nothing,
      harvestJobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to resume pagination from the end of the
-- collection.
listHarvestJobsResponse_nextToken :: Lens.Lens' ListHarvestJobsResponse (Core.Maybe Core.Text)
listHarvestJobsResponse_nextToken = Lens.lens (\ListHarvestJobsResponse' {nextToken} -> nextToken) (\s@ListHarvestJobsResponse' {} a -> s {nextToken = a} :: ListHarvestJobsResponse)

-- | A list of HarvestJob records.
listHarvestJobsResponse_harvestJobs :: Lens.Lens' ListHarvestJobsResponse (Core.Maybe [HarvestJob])
listHarvestJobsResponse_harvestJobs = Lens.lens (\ListHarvestJobsResponse' {harvestJobs} -> harvestJobs) (\s@ListHarvestJobsResponse' {} a -> s {harvestJobs = a} :: ListHarvestJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listHarvestJobsResponse_httpStatus :: Lens.Lens' ListHarvestJobsResponse Core.Int
listHarvestJobsResponse_httpStatus = Lens.lens (\ListHarvestJobsResponse' {httpStatus} -> httpStatus) (\s@ListHarvestJobsResponse' {} a -> s {httpStatus = a} :: ListHarvestJobsResponse)

instance Core.NFData ListHarvestJobsResponse
