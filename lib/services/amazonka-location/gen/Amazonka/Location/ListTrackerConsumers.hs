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
-- Module      : Amazonka.Location.ListTrackerConsumers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists geofence collections currently associated to the given tracker
-- resource.
--
-- This operation returns paginated results.
module Amazonka.Location.ListTrackerConsumers
  ( -- * Creating a Request
    ListTrackerConsumers (..),
    newListTrackerConsumers,

    -- * Request Lenses
    listTrackerConsumers_maxResults,
    listTrackerConsumers_nextToken,
    listTrackerConsumers_trackerName,

    -- * Destructuring the Response
    ListTrackerConsumersResponse (..),
    newListTrackerConsumersResponse,

    -- * Response Lenses
    listTrackerConsumersResponse_nextToken,
    listTrackerConsumersResponse_httpStatus,
    listTrackerConsumersResponse_consumerArns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTrackerConsumers' smart constructor.
data ListTrackerConsumers = ListTrackerConsumers'
  { -- | An optional limit for the number of resources returned in a single call.
    --
    -- Default value: @100@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token specifying which page of results to return in the
    -- response. If no token is provided, the default page is the first page.
    --
    -- Default value: @null@
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The tracker resource whose associated geofence collections you want to
    -- list.
    trackerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrackerConsumers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTrackerConsumers_maxResults' - An optional limit for the number of resources returned in a single call.
--
-- Default value: @100@
--
-- 'nextToken', 'listTrackerConsumers_nextToken' - The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
--
-- 'trackerName', 'listTrackerConsumers_trackerName' - The tracker resource whose associated geofence collections you want to
-- list.
newListTrackerConsumers ::
  -- | 'trackerName'
  Prelude.Text ->
  ListTrackerConsumers
newListTrackerConsumers pTrackerName_ =
  ListTrackerConsumers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      trackerName = pTrackerName_
    }

-- | An optional limit for the number of resources returned in a single call.
--
-- Default value: @100@
listTrackerConsumers_maxResults :: Lens.Lens' ListTrackerConsumers (Prelude.Maybe Prelude.Natural)
listTrackerConsumers_maxResults = Lens.lens (\ListTrackerConsumers' {maxResults} -> maxResults) (\s@ListTrackerConsumers' {} a -> s {maxResults = a} :: ListTrackerConsumers)

-- | The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
listTrackerConsumers_nextToken :: Lens.Lens' ListTrackerConsumers (Prelude.Maybe Prelude.Text)
listTrackerConsumers_nextToken = Lens.lens (\ListTrackerConsumers' {nextToken} -> nextToken) (\s@ListTrackerConsumers' {} a -> s {nextToken = a} :: ListTrackerConsumers)

-- | The tracker resource whose associated geofence collections you want to
-- list.
listTrackerConsumers_trackerName :: Lens.Lens' ListTrackerConsumers Prelude.Text
listTrackerConsumers_trackerName = Lens.lens (\ListTrackerConsumers' {trackerName} -> trackerName) (\s@ListTrackerConsumers' {} a -> s {trackerName = a} :: ListTrackerConsumers)

instance Core.AWSPager ListTrackerConsumers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrackerConsumersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTrackerConsumersResponse_consumerArns
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTrackerConsumers_nextToken
          Lens..~ rs
          Lens.^? listTrackerConsumersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTrackerConsumers where
  type
    AWSResponse ListTrackerConsumers =
      ListTrackerConsumersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrackerConsumersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ConsumerArns" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTrackerConsumers where
  hashWithSalt _salt ListTrackerConsumers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` trackerName

instance Prelude.NFData ListTrackerConsumers where
  rnf ListTrackerConsumers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trackerName

instance Data.ToHeaders ListTrackerConsumers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTrackerConsumers where
  toJSON ListTrackerConsumers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListTrackerConsumers where
  toPath ListTrackerConsumers' {..} =
    Prelude.mconcat
      [ "/tracking/v0/trackers/",
        Data.toBS trackerName,
        "/list-consumers"
      ]

instance Data.ToQuery ListTrackerConsumers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTrackerConsumersResponse' smart constructor.
data ListTrackerConsumersResponse = ListTrackerConsumersResponse'
  { -- | A pagination token indicating there are additional pages available. You
    -- can use the token in a following request to fetch the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains the list of geofence collection ARNs associated to the tracker
    -- resource.
    consumerArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrackerConsumersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrackerConsumersResponse_nextToken' - A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
--
-- 'httpStatus', 'listTrackerConsumersResponse_httpStatus' - The response's http status code.
--
-- 'consumerArns', 'listTrackerConsumersResponse_consumerArns' - Contains the list of geofence collection ARNs associated to the tracker
-- resource.
newListTrackerConsumersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrackerConsumersResponse
newListTrackerConsumersResponse pHttpStatus_ =
  ListTrackerConsumersResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      consumerArns = Prelude.mempty
    }

-- | A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
listTrackerConsumersResponse_nextToken :: Lens.Lens' ListTrackerConsumersResponse (Prelude.Maybe Prelude.Text)
listTrackerConsumersResponse_nextToken = Lens.lens (\ListTrackerConsumersResponse' {nextToken} -> nextToken) (\s@ListTrackerConsumersResponse' {} a -> s {nextToken = a} :: ListTrackerConsumersResponse)

-- | The response's http status code.
listTrackerConsumersResponse_httpStatus :: Lens.Lens' ListTrackerConsumersResponse Prelude.Int
listTrackerConsumersResponse_httpStatus = Lens.lens (\ListTrackerConsumersResponse' {httpStatus} -> httpStatus) (\s@ListTrackerConsumersResponse' {} a -> s {httpStatus = a} :: ListTrackerConsumersResponse)

-- | Contains the list of geofence collection ARNs associated to the tracker
-- resource.
listTrackerConsumersResponse_consumerArns :: Lens.Lens' ListTrackerConsumersResponse [Prelude.Text]
listTrackerConsumersResponse_consumerArns = Lens.lens (\ListTrackerConsumersResponse' {consumerArns} -> consumerArns) (\s@ListTrackerConsumersResponse' {} a -> s {consumerArns = a} :: ListTrackerConsumersResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTrackerConsumersResponse where
  rnf ListTrackerConsumersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf consumerArns
