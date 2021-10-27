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
-- Module      : Network.AWS.MediaTailor.ListPlaybackConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the playback configurations defined in AWS Elemental
-- MediaTailor. You can specify a maximum number of configurations to
-- return at a time. The default maximum is 50. Results are returned in
-- pagefuls. If MediaTailor has more configurations than the specified
-- maximum, it provides parameters in the response that you can use to
-- retrieve the next pageful.
--
-- This operation returns paginated results.
module Network.AWS.MediaTailor.ListPlaybackConfigurations
  ( -- * Creating a Request
    ListPlaybackConfigurations (..),
    newListPlaybackConfigurations,

    -- * Request Lenses
    listPlaybackConfigurations_nextToken,
    listPlaybackConfigurations_maxResults,

    -- * Destructuring the Response
    ListPlaybackConfigurationsResponse (..),
    newListPlaybackConfigurationsResponse,

    -- * Response Lenses
    listPlaybackConfigurationsResponse_items,
    listPlaybackConfigurationsResponse_nextToken,
    listPlaybackConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaTailor.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPlaybackConfigurations' smart constructor.
data ListPlaybackConfigurations = ListPlaybackConfigurations'
  { -- | Pagination token returned by the GET list request when results exceed
    -- the maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaybackConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlaybackConfigurations_nextToken' - Pagination token returned by the GET list request when results exceed
-- the maximum allowed. Use the token to fetch the next page of results.
--
-- 'maxResults', 'listPlaybackConfigurations_maxResults' - Maximum number of records to return.
newListPlaybackConfigurations ::
  ListPlaybackConfigurations
newListPlaybackConfigurations =
  ListPlaybackConfigurations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Pagination token returned by the GET list request when results exceed
-- the maximum allowed. Use the token to fetch the next page of results.
listPlaybackConfigurations_nextToken :: Lens.Lens' ListPlaybackConfigurations (Prelude.Maybe Prelude.Text)
listPlaybackConfigurations_nextToken = Lens.lens (\ListPlaybackConfigurations' {nextToken} -> nextToken) (\s@ListPlaybackConfigurations' {} a -> s {nextToken = a} :: ListPlaybackConfigurations)

-- | Maximum number of records to return.
listPlaybackConfigurations_maxResults :: Lens.Lens' ListPlaybackConfigurations (Prelude.Maybe Prelude.Natural)
listPlaybackConfigurations_maxResults = Lens.lens (\ListPlaybackConfigurations' {maxResults} -> maxResults) (\s@ListPlaybackConfigurations' {} a -> s {maxResults = a} :: ListPlaybackConfigurations)

instance Core.AWSPager ListPlaybackConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlaybackConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPlaybackConfigurationsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPlaybackConfigurations_nextToken
          Lens..~ rs
          Lens.^? listPlaybackConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPlaybackConfigurations where
  type
    AWSResponse ListPlaybackConfigurations =
      ListPlaybackConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPlaybackConfigurationsResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPlaybackConfigurations

instance Prelude.NFData ListPlaybackConfigurations

instance Core.ToHeaders ListPlaybackConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListPlaybackConfigurations where
  toPath = Prelude.const "/playbackConfigurations"

instance Core.ToQuery ListPlaybackConfigurations where
  toQuery ListPlaybackConfigurations' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListPlaybackConfigurationsResponse' smart constructor.
data ListPlaybackConfigurationsResponse = ListPlaybackConfigurationsResponse'
  { -- | Array of playback configurations. This might be all the available
    -- configurations or a subset, depending on the settings that you provide
    -- and the total number of configurations stored.
    items :: Prelude.Maybe [PlaybackConfiguration],
    -- | Pagination token returned by the GET list request when results exceed
    -- the maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaybackConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listPlaybackConfigurationsResponse_items' - Array of playback configurations. This might be all the available
-- configurations or a subset, depending on the settings that you provide
-- and the total number of configurations stored.
--
-- 'nextToken', 'listPlaybackConfigurationsResponse_nextToken' - Pagination token returned by the GET list request when results exceed
-- the maximum allowed. Use the token to fetch the next page of results.
--
-- 'httpStatus', 'listPlaybackConfigurationsResponse_httpStatus' - The response's http status code.
newListPlaybackConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPlaybackConfigurationsResponse
newListPlaybackConfigurationsResponse pHttpStatus_ =
  ListPlaybackConfigurationsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Array of playback configurations. This might be all the available
-- configurations or a subset, depending on the settings that you provide
-- and the total number of configurations stored.
listPlaybackConfigurationsResponse_items :: Lens.Lens' ListPlaybackConfigurationsResponse (Prelude.Maybe [PlaybackConfiguration])
listPlaybackConfigurationsResponse_items = Lens.lens (\ListPlaybackConfigurationsResponse' {items} -> items) (\s@ListPlaybackConfigurationsResponse' {} a -> s {items = a} :: ListPlaybackConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Pagination token returned by the GET list request when results exceed
-- the maximum allowed. Use the token to fetch the next page of results.
listPlaybackConfigurationsResponse_nextToken :: Lens.Lens' ListPlaybackConfigurationsResponse (Prelude.Maybe Prelude.Text)
listPlaybackConfigurationsResponse_nextToken = Lens.lens (\ListPlaybackConfigurationsResponse' {nextToken} -> nextToken) (\s@ListPlaybackConfigurationsResponse' {} a -> s {nextToken = a} :: ListPlaybackConfigurationsResponse)

-- | The response's http status code.
listPlaybackConfigurationsResponse_httpStatus :: Lens.Lens' ListPlaybackConfigurationsResponse Prelude.Int
listPlaybackConfigurationsResponse_httpStatus = Lens.lens (\ListPlaybackConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListPlaybackConfigurationsResponse' {} a -> s {httpStatus = a} :: ListPlaybackConfigurationsResponse)

instance
  Prelude.NFData
    ListPlaybackConfigurationsResponse
