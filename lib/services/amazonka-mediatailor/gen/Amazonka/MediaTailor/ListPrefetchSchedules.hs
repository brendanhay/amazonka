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
-- Module      : Amazonka.MediaTailor.ListPrefetchSchedules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the prefetch schedules for a playback configuration.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.ListPrefetchSchedules
  ( -- * Creating a Request
    ListPrefetchSchedules (..),
    newListPrefetchSchedules,

    -- * Request Lenses
    listPrefetchSchedules_nextToken,
    listPrefetchSchedules_streamId,
    listPrefetchSchedules_maxResults,
    listPrefetchSchedules_playbackConfigurationName,

    -- * Destructuring the Response
    ListPrefetchSchedulesResponse (..),
    newListPrefetchSchedulesResponse,

    -- * Response Lenses
    listPrefetchSchedulesResponse_items,
    listPrefetchSchedulesResponse_nextToken,
    listPrefetchSchedulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPrefetchSchedules' smart constructor.
data ListPrefetchSchedules = ListPrefetchSchedules'
  { -- | (Optional) If the playback configuration has more than @MaxResults@
    -- prefetch schedules, use @NextToken@ to get the second and subsequent
    -- pages of results.
    --
    -- For the first @ListPrefetchSchedulesRequest@ request, omit this value.
    --
    -- For the second and subsequent requests, get the value of @NextToken@
    -- from the previous response and specify that value for @NextToken@ in the
    -- request.
    --
    -- If the previous response didn\'t include a @NextToken@ element, there
    -- are no more prefetch schedules to get.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional filtering parameter whereby MediaTailor filters the prefetch
    -- schedules to include only specific streams.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of prefetch schedules that you want MediaTailor to
    -- return in response to the current request. If there are more than
    -- @MaxResults@ prefetch schedules, use the value of @NextToken@ in the
    -- response to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Retrieves the prefetch schedule(s) for a specific playback
    -- configuration.
    playbackConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrefetchSchedules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPrefetchSchedules_nextToken' - (Optional) If the playback configuration has more than @MaxResults@
-- prefetch schedules, use @NextToken@ to get the second and subsequent
-- pages of results.
--
-- For the first @ListPrefetchSchedulesRequest@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- If the previous response didn\'t include a @NextToken@ element, there
-- are no more prefetch schedules to get.
--
-- 'streamId', 'listPrefetchSchedules_streamId' - An optional filtering parameter whereby MediaTailor filters the prefetch
-- schedules to include only specific streams.
--
-- 'maxResults', 'listPrefetchSchedules_maxResults' - The maximum number of prefetch schedules that you want MediaTailor to
-- return in response to the current request. If there are more than
-- @MaxResults@ prefetch schedules, use the value of @NextToken@ in the
-- response to get the next page of results.
--
-- 'playbackConfigurationName', 'listPrefetchSchedules_playbackConfigurationName' - Retrieves the prefetch schedule(s) for a specific playback
-- configuration.
newListPrefetchSchedules ::
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  ListPrefetchSchedules
newListPrefetchSchedules pPlaybackConfigurationName_ =
  ListPrefetchSchedules'
    { nextToken = Prelude.Nothing,
      streamId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      playbackConfigurationName =
        pPlaybackConfigurationName_
    }

-- | (Optional) If the playback configuration has more than @MaxResults@
-- prefetch schedules, use @NextToken@ to get the second and subsequent
-- pages of results.
--
-- For the first @ListPrefetchSchedulesRequest@ request, omit this value.
--
-- For the second and subsequent requests, get the value of @NextToken@
-- from the previous response and specify that value for @NextToken@ in the
-- request.
--
-- If the previous response didn\'t include a @NextToken@ element, there
-- are no more prefetch schedules to get.
listPrefetchSchedules_nextToken :: Lens.Lens' ListPrefetchSchedules (Prelude.Maybe Prelude.Text)
listPrefetchSchedules_nextToken = Lens.lens (\ListPrefetchSchedules' {nextToken} -> nextToken) (\s@ListPrefetchSchedules' {} a -> s {nextToken = a} :: ListPrefetchSchedules)

-- | An optional filtering parameter whereby MediaTailor filters the prefetch
-- schedules to include only specific streams.
listPrefetchSchedules_streamId :: Lens.Lens' ListPrefetchSchedules (Prelude.Maybe Prelude.Text)
listPrefetchSchedules_streamId = Lens.lens (\ListPrefetchSchedules' {streamId} -> streamId) (\s@ListPrefetchSchedules' {} a -> s {streamId = a} :: ListPrefetchSchedules)

-- | The maximum number of prefetch schedules that you want MediaTailor to
-- return in response to the current request. If there are more than
-- @MaxResults@ prefetch schedules, use the value of @NextToken@ in the
-- response to get the next page of results.
listPrefetchSchedules_maxResults :: Lens.Lens' ListPrefetchSchedules (Prelude.Maybe Prelude.Natural)
listPrefetchSchedules_maxResults = Lens.lens (\ListPrefetchSchedules' {maxResults} -> maxResults) (\s@ListPrefetchSchedules' {} a -> s {maxResults = a} :: ListPrefetchSchedules)

-- | Retrieves the prefetch schedule(s) for a specific playback
-- configuration.
listPrefetchSchedules_playbackConfigurationName :: Lens.Lens' ListPrefetchSchedules Prelude.Text
listPrefetchSchedules_playbackConfigurationName = Lens.lens (\ListPrefetchSchedules' {playbackConfigurationName} -> playbackConfigurationName) (\s@ListPrefetchSchedules' {} a -> s {playbackConfigurationName = a} :: ListPrefetchSchedules)

instance Core.AWSPager ListPrefetchSchedules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrefetchSchedulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrefetchSchedulesResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPrefetchSchedules_nextToken
          Lens..~ rs
          Lens.^? listPrefetchSchedulesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPrefetchSchedules where
  type
    AWSResponse ListPrefetchSchedules =
      ListPrefetchSchedulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrefetchSchedulesResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPrefetchSchedules where
  hashWithSalt _salt ListPrefetchSchedules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` playbackConfigurationName

instance Prelude.NFData ListPrefetchSchedules where
  rnf ListPrefetchSchedules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf playbackConfigurationName

instance Core.ToHeaders ListPrefetchSchedules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListPrefetchSchedules where
  toJSON ListPrefetchSchedules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("StreamId" Core..=) Prelude.<$> streamId,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListPrefetchSchedules where
  toPath ListPrefetchSchedules' {..} =
    Prelude.mconcat
      [ "/prefetchSchedule/",
        Core.toBS playbackConfigurationName
      ]

instance Core.ToQuery ListPrefetchSchedules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPrefetchSchedulesResponse' smart constructor.
data ListPrefetchSchedulesResponse = ListPrefetchSchedulesResponse'
  { -- | Lists the prefetch schedules. An empty @Items@ list doesn\'t mean there
    -- aren\'t more items to fetch, just that that page was empty.
    items :: Prelude.Maybe [PrefetchSchedule],
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrefetchSchedulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listPrefetchSchedulesResponse_items' - Lists the prefetch schedules. An empty @Items@ list doesn\'t mean there
-- aren\'t more items to fetch, just that that page was empty.
--
-- 'nextToken', 'listPrefetchSchedulesResponse_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
--
-- 'httpStatus', 'listPrefetchSchedulesResponse_httpStatus' - The response's http status code.
newListPrefetchSchedulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPrefetchSchedulesResponse
newListPrefetchSchedulesResponse pHttpStatus_ =
  ListPrefetchSchedulesResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the prefetch schedules. An empty @Items@ list doesn\'t mean there
-- aren\'t more items to fetch, just that that page was empty.
listPrefetchSchedulesResponse_items :: Lens.Lens' ListPrefetchSchedulesResponse (Prelude.Maybe [PrefetchSchedule])
listPrefetchSchedulesResponse_items = Lens.lens (\ListPrefetchSchedulesResponse' {items} -> items) (\s@ListPrefetchSchedulesResponse' {} a -> s {items = a} :: ListPrefetchSchedulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listPrefetchSchedulesResponse_nextToken :: Lens.Lens' ListPrefetchSchedulesResponse (Prelude.Maybe Prelude.Text)
listPrefetchSchedulesResponse_nextToken = Lens.lens (\ListPrefetchSchedulesResponse' {nextToken} -> nextToken) (\s@ListPrefetchSchedulesResponse' {} a -> s {nextToken = a} :: ListPrefetchSchedulesResponse)

-- | The response's http status code.
listPrefetchSchedulesResponse_httpStatus :: Lens.Lens' ListPrefetchSchedulesResponse Prelude.Int
listPrefetchSchedulesResponse_httpStatus = Lens.lens (\ListPrefetchSchedulesResponse' {httpStatus} -> httpStatus) (\s@ListPrefetchSchedulesResponse' {} a -> s {httpStatus = a} :: ListPrefetchSchedulesResponse)

instance Prelude.NFData ListPrefetchSchedulesResponse where
  rnf ListPrefetchSchedulesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
