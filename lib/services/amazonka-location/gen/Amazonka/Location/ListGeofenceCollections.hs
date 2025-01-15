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
-- Module      : Amazonka.Location.ListGeofenceCollections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists geofence collections in your AWS account.
--
-- This operation returns paginated results.
module Amazonka.Location.ListGeofenceCollections
  ( -- * Creating a Request
    ListGeofenceCollections (..),
    newListGeofenceCollections,

    -- * Request Lenses
    listGeofenceCollections_maxResults,
    listGeofenceCollections_nextToken,

    -- * Destructuring the Response
    ListGeofenceCollectionsResponse (..),
    newListGeofenceCollectionsResponse,

    -- * Response Lenses
    listGeofenceCollectionsResponse_nextToken,
    listGeofenceCollectionsResponse_httpStatus,
    listGeofenceCollectionsResponse_entries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGeofenceCollections' smart constructor.
data ListGeofenceCollections = ListGeofenceCollections'
  { -- | An optional limit for the number of resources returned in a single call.
    --
    -- Default value: @100@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token specifying which page of results to return in the
    -- response. If no token is provided, the default page is the first page.
    --
    -- Default value: @null@
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeofenceCollections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGeofenceCollections_maxResults' - An optional limit for the number of resources returned in a single call.
--
-- Default value: @100@
--
-- 'nextToken', 'listGeofenceCollections_nextToken' - The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
newListGeofenceCollections ::
  ListGeofenceCollections
newListGeofenceCollections =
  ListGeofenceCollections'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An optional limit for the number of resources returned in a single call.
--
-- Default value: @100@
listGeofenceCollections_maxResults :: Lens.Lens' ListGeofenceCollections (Prelude.Maybe Prelude.Natural)
listGeofenceCollections_maxResults = Lens.lens (\ListGeofenceCollections' {maxResults} -> maxResults) (\s@ListGeofenceCollections' {} a -> s {maxResults = a} :: ListGeofenceCollections)

-- | The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
listGeofenceCollections_nextToken :: Lens.Lens' ListGeofenceCollections (Prelude.Maybe Prelude.Text)
listGeofenceCollections_nextToken = Lens.lens (\ListGeofenceCollections' {nextToken} -> nextToken) (\s@ListGeofenceCollections' {} a -> s {nextToken = a} :: ListGeofenceCollections)

instance Core.AWSPager ListGeofenceCollections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGeofenceCollectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listGeofenceCollectionsResponse_entries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listGeofenceCollections_nextToken
              Lens..~ rs
              Lens.^? listGeofenceCollectionsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListGeofenceCollections where
  type
    AWSResponse ListGeofenceCollections =
      ListGeofenceCollectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGeofenceCollectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListGeofenceCollections where
  hashWithSalt _salt ListGeofenceCollections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListGeofenceCollections where
  rnf ListGeofenceCollections' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListGeofenceCollections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGeofenceCollections where
  toJSON ListGeofenceCollections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListGeofenceCollections where
  toPath =
    Prelude.const "/geofencing/v0/list-collections"

instance Data.ToQuery ListGeofenceCollections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGeofenceCollectionsResponse' smart constructor.
data ListGeofenceCollectionsResponse = ListGeofenceCollectionsResponse'
  { -- | A pagination token indicating there are additional pages available. You
    -- can use the token in a following request to fetch the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists the geofence collections that exist in your AWS account.
    entries :: [ListGeofenceCollectionsResponseEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeofenceCollectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGeofenceCollectionsResponse_nextToken' - A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
--
-- 'httpStatus', 'listGeofenceCollectionsResponse_httpStatus' - The response's http status code.
--
-- 'entries', 'listGeofenceCollectionsResponse_entries' - Lists the geofence collections that exist in your AWS account.
newListGeofenceCollectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGeofenceCollectionsResponse
newListGeofenceCollectionsResponse pHttpStatus_ =
  ListGeofenceCollectionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entries = Prelude.mempty
    }

-- | A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
listGeofenceCollectionsResponse_nextToken :: Lens.Lens' ListGeofenceCollectionsResponse (Prelude.Maybe Prelude.Text)
listGeofenceCollectionsResponse_nextToken = Lens.lens (\ListGeofenceCollectionsResponse' {nextToken} -> nextToken) (\s@ListGeofenceCollectionsResponse' {} a -> s {nextToken = a} :: ListGeofenceCollectionsResponse)

-- | The response's http status code.
listGeofenceCollectionsResponse_httpStatus :: Lens.Lens' ListGeofenceCollectionsResponse Prelude.Int
listGeofenceCollectionsResponse_httpStatus = Lens.lens (\ListGeofenceCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListGeofenceCollectionsResponse' {} a -> s {httpStatus = a} :: ListGeofenceCollectionsResponse)

-- | Lists the geofence collections that exist in your AWS account.
listGeofenceCollectionsResponse_entries :: Lens.Lens' ListGeofenceCollectionsResponse [ListGeofenceCollectionsResponseEntry]
listGeofenceCollectionsResponse_entries = Lens.lens (\ListGeofenceCollectionsResponse' {entries} -> entries) (\s@ListGeofenceCollectionsResponse' {} a -> s {entries = a} :: ListGeofenceCollectionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListGeofenceCollectionsResponse
  where
  rnf ListGeofenceCollectionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf entries
