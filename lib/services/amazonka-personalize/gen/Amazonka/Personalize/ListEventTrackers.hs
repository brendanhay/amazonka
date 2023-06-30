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
-- Module      : Amazonka.Personalize.ListEventTrackers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of event trackers associated with the account. The
-- response provides the properties for each event tracker, including the
-- Amazon Resource Name (ARN) and tracking ID. For more information on
-- event trackers, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateEventTracker.html CreateEventTracker>.
--
-- This operation returns paginated results.
module Amazonka.Personalize.ListEventTrackers
  ( -- * Creating a Request
    ListEventTrackers (..),
    newListEventTrackers,

    -- * Request Lenses
    listEventTrackers_datasetGroupArn,
    listEventTrackers_maxResults,
    listEventTrackers_nextToken,

    -- * Destructuring the Response
    ListEventTrackersResponse (..),
    newListEventTrackersResponse,

    -- * Response Lenses
    listEventTrackersResponse_eventTrackers,
    listEventTrackersResponse_nextToken,
    listEventTrackersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventTrackers' smart constructor.
data ListEventTrackers = ListEventTrackers'
  { -- | The ARN of a dataset group used to filter the response.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of event trackers to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token returned from the previous call to @ListEventTrackers@ for
    -- getting the next set of event trackers (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventTrackers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetGroupArn', 'listEventTrackers_datasetGroupArn' - The ARN of a dataset group used to filter the response.
--
-- 'maxResults', 'listEventTrackers_maxResults' - The maximum number of event trackers to return.
--
-- 'nextToken', 'listEventTrackers_nextToken' - A token returned from the previous call to @ListEventTrackers@ for
-- getting the next set of event trackers (if they exist).
newListEventTrackers ::
  ListEventTrackers
newListEventTrackers =
  ListEventTrackers'
    { datasetGroupArn =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ARN of a dataset group used to filter the response.
listEventTrackers_datasetGroupArn :: Lens.Lens' ListEventTrackers (Prelude.Maybe Prelude.Text)
listEventTrackers_datasetGroupArn = Lens.lens (\ListEventTrackers' {datasetGroupArn} -> datasetGroupArn) (\s@ListEventTrackers' {} a -> s {datasetGroupArn = a} :: ListEventTrackers)

-- | The maximum number of event trackers to return.
listEventTrackers_maxResults :: Lens.Lens' ListEventTrackers (Prelude.Maybe Prelude.Natural)
listEventTrackers_maxResults = Lens.lens (\ListEventTrackers' {maxResults} -> maxResults) (\s@ListEventTrackers' {} a -> s {maxResults = a} :: ListEventTrackers)

-- | A token returned from the previous call to @ListEventTrackers@ for
-- getting the next set of event trackers (if they exist).
listEventTrackers_nextToken :: Lens.Lens' ListEventTrackers (Prelude.Maybe Prelude.Text)
listEventTrackers_nextToken = Lens.lens (\ListEventTrackers' {nextToken} -> nextToken) (\s@ListEventTrackers' {} a -> s {nextToken = a} :: ListEventTrackers)

instance Core.AWSPager ListEventTrackers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventTrackersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEventTrackersResponse_eventTrackers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEventTrackers_nextToken
          Lens..~ rs
          Lens.^? listEventTrackersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEventTrackers where
  type
    AWSResponse ListEventTrackers =
      ListEventTrackersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventTrackersResponse'
            Prelude.<$> (x Data..?> "eventTrackers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventTrackers where
  hashWithSalt _salt ListEventTrackers' {..} =
    _salt
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEventTrackers where
  rnf ListEventTrackers' {..} =
    Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEventTrackers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.ListEventTrackers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEventTrackers where
  toJSON ListEventTrackers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("datasetGroupArn" Data..=)
              Prelude.<$> datasetGroupArn,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEventTrackers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEventTrackers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEventTrackersResponse' smart constructor.
data ListEventTrackersResponse = ListEventTrackersResponse'
  { -- | A list of event trackers.
    eventTrackers :: Prelude.Maybe [EventTrackerSummary],
    -- | A token for getting the next set of event trackers (if they exist).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventTrackersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTrackers', 'listEventTrackersResponse_eventTrackers' - A list of event trackers.
--
-- 'nextToken', 'listEventTrackersResponse_nextToken' - A token for getting the next set of event trackers (if they exist).
--
-- 'httpStatus', 'listEventTrackersResponse_httpStatus' - The response's http status code.
newListEventTrackersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventTrackersResponse
newListEventTrackersResponse pHttpStatus_ =
  ListEventTrackersResponse'
    { eventTrackers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event trackers.
listEventTrackersResponse_eventTrackers :: Lens.Lens' ListEventTrackersResponse (Prelude.Maybe [EventTrackerSummary])
listEventTrackersResponse_eventTrackers = Lens.lens (\ListEventTrackersResponse' {eventTrackers} -> eventTrackers) (\s@ListEventTrackersResponse' {} a -> s {eventTrackers = a} :: ListEventTrackersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of event trackers (if they exist).
listEventTrackersResponse_nextToken :: Lens.Lens' ListEventTrackersResponse (Prelude.Maybe Prelude.Text)
listEventTrackersResponse_nextToken = Lens.lens (\ListEventTrackersResponse' {nextToken} -> nextToken) (\s@ListEventTrackersResponse' {} a -> s {nextToken = a} :: ListEventTrackersResponse)

-- | The response's http status code.
listEventTrackersResponse_httpStatus :: Lens.Lens' ListEventTrackersResponse Prelude.Int
listEventTrackersResponse_httpStatus = Lens.lens (\ListEventTrackersResponse' {httpStatus} -> httpStatus) (\s@ListEventTrackersResponse' {} a -> s {httpStatus = a} :: ListEventTrackersResponse)

instance Prelude.NFData ListEventTrackersResponse where
  rnf ListEventTrackersResponse' {..} =
    Prelude.rnf eventTrackers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
