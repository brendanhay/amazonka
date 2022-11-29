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
-- Module      : Amazonka.Location.ListGeofences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists geofences stored in a given geofence collection.
--
-- This operation returns paginated results.
module Amazonka.Location.ListGeofences
  ( -- * Creating a Request
    ListGeofences (..),
    newListGeofences,

    -- * Request Lenses
    listGeofences_nextToken,
    listGeofences_maxResults,
    listGeofences_collectionName,

    -- * Destructuring the Response
    ListGeofencesResponse (..),
    newListGeofencesResponse,

    -- * Response Lenses
    listGeofencesResponse_nextToken,
    listGeofencesResponse_httpStatus,
    listGeofencesResponse_entries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGeofences' smart constructor.
data ListGeofences = ListGeofences'
  { -- | The pagination token specifying which page of results to return in the
    -- response. If no token is provided, the default page is the first page.
    --
    -- Default value: @null@
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional limit for the number of geofences returned in a single call.
    --
    -- Default value: @100@
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the geofence collection storing the list of geofences.
    collectionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeofences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGeofences_nextToken' - The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
--
-- 'maxResults', 'listGeofences_maxResults' - An optional limit for the number of geofences returned in a single call.
--
-- Default value: @100@
--
-- 'collectionName', 'listGeofences_collectionName' - The name of the geofence collection storing the list of geofences.
newListGeofences ::
  -- | 'collectionName'
  Prelude.Text ->
  ListGeofences
newListGeofences pCollectionName_ =
  ListGeofences'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      collectionName = pCollectionName_
    }

-- | The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
listGeofences_nextToken :: Lens.Lens' ListGeofences (Prelude.Maybe Prelude.Text)
listGeofences_nextToken = Lens.lens (\ListGeofences' {nextToken} -> nextToken) (\s@ListGeofences' {} a -> s {nextToken = a} :: ListGeofences)

-- | An optional limit for the number of geofences returned in a single call.
--
-- Default value: @100@
listGeofences_maxResults :: Lens.Lens' ListGeofences (Prelude.Maybe Prelude.Natural)
listGeofences_maxResults = Lens.lens (\ListGeofences' {maxResults} -> maxResults) (\s@ListGeofences' {} a -> s {maxResults = a} :: ListGeofences)

-- | The name of the geofence collection storing the list of geofences.
listGeofences_collectionName :: Lens.Lens' ListGeofences Prelude.Text
listGeofences_collectionName = Lens.lens (\ListGeofences' {collectionName} -> collectionName) (\s@ListGeofences' {} a -> s {collectionName = a} :: ListGeofences)

instance Core.AWSPager ListGeofences where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGeofencesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listGeofencesResponse_entries) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGeofences_nextToken
          Lens..~ rs
          Lens.^? listGeofencesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListGeofences where
  type
    AWSResponse ListGeofences =
      ListGeofencesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGeofencesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Entries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListGeofences where
  hashWithSalt _salt ListGeofences' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` collectionName

instance Prelude.NFData ListGeofences where
  rnf ListGeofences' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf collectionName

instance Core.ToHeaders ListGeofences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListGeofences where
  toJSON ListGeofences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListGeofences where
  toPath ListGeofences' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Core.toBS collectionName,
        "/list-geofences"
      ]

instance Core.ToQuery ListGeofences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGeofencesResponse' smart constructor.
data ListGeofencesResponse = ListGeofencesResponse'
  { -- | A pagination token indicating there are additional pages available. You
    -- can use the token in a following request to fetch the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains a list of geofences stored in the geofence collection.
    entries :: [ListGeofenceResponseEntry]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeofencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGeofencesResponse_nextToken' - A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
--
-- 'httpStatus', 'listGeofencesResponse_httpStatus' - The response's http status code.
--
-- 'entries', 'listGeofencesResponse_entries' - Contains a list of geofences stored in the geofence collection.
newListGeofencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGeofencesResponse
newListGeofencesResponse pHttpStatus_ =
  ListGeofencesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entries = Prelude.mempty
    }

-- | A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
listGeofencesResponse_nextToken :: Lens.Lens' ListGeofencesResponse (Prelude.Maybe Prelude.Text)
listGeofencesResponse_nextToken = Lens.lens (\ListGeofencesResponse' {nextToken} -> nextToken) (\s@ListGeofencesResponse' {} a -> s {nextToken = a} :: ListGeofencesResponse)

-- | The response's http status code.
listGeofencesResponse_httpStatus :: Lens.Lens' ListGeofencesResponse Prelude.Int
listGeofencesResponse_httpStatus = Lens.lens (\ListGeofencesResponse' {httpStatus} -> httpStatus) (\s@ListGeofencesResponse' {} a -> s {httpStatus = a} :: ListGeofencesResponse)

-- | Contains a list of geofences stored in the geofence collection.
listGeofencesResponse_entries :: Lens.Lens' ListGeofencesResponse [ListGeofenceResponseEntry]
listGeofencesResponse_entries = Lens.lens (\ListGeofencesResponse' {entries} -> entries) (\s@ListGeofencesResponse' {} a -> s {entries = a} :: ListGeofencesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListGeofencesResponse where
  rnf ListGeofencesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entries
