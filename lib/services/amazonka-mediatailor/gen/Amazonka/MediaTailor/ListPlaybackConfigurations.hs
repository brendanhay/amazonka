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
-- Module      : Amazonka.MediaTailor.ListPlaybackConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves existing playback configurations. For information about
-- MediaTailor configurations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with Configurations in AWS Elemental MediaTailor>.
--
-- This operation returns paginated results.
module Amazonka.MediaTailor.ListPlaybackConfigurations
  ( -- * Creating a Request
    ListPlaybackConfigurations (..),
    newListPlaybackConfigurations,

    -- * Request Lenses
    listPlaybackConfigurations_maxResults,
    listPlaybackConfigurations_nextToken,

    -- * Destructuring the Response
    ListPlaybackConfigurationsResponse (..),
    newListPlaybackConfigurationsResponse,

    -- * Response Lenses
    listPlaybackConfigurationsResponse_items,
    listPlaybackConfigurationsResponse_nextToken,
    listPlaybackConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPlaybackConfigurations' smart constructor.
data ListPlaybackConfigurations = ListPlaybackConfigurations'
  { -- | The maximum number of playback configurations that you want MediaTailor
    -- to return in response to the current request. If there are more than
    -- @MaxResults@ playback configurations, use the value of @NextToken@ in
    -- the response to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Pagination token returned by the list request when results exceed the
    -- maximum allowed. Use the token to fetch the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listPlaybackConfigurations_maxResults' - The maximum number of playback configurations that you want MediaTailor
-- to return in response to the current request. If there are more than
-- @MaxResults@ playback configurations, use the value of @NextToken@ in
-- the response to get the next page of results.
--
-- 'nextToken', 'listPlaybackConfigurations_nextToken' - Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
newListPlaybackConfigurations ::
  ListPlaybackConfigurations
newListPlaybackConfigurations =
  ListPlaybackConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of playback configurations that you want MediaTailor
-- to return in response to the current request. If there are more than
-- @MaxResults@ playback configurations, use the value of @NextToken@ in
-- the response to get the next page of results.
listPlaybackConfigurations_maxResults :: Lens.Lens' ListPlaybackConfigurations (Prelude.Maybe Prelude.Natural)
listPlaybackConfigurations_maxResults = Lens.lens (\ListPlaybackConfigurations' {maxResults} -> maxResults) (\s@ListPlaybackConfigurations' {} a -> s {maxResults = a} :: ListPlaybackConfigurations)

-- | Pagination token returned by the list request when results exceed the
-- maximum allowed. Use the token to fetch the next page of results.
listPlaybackConfigurations_nextToken :: Lens.Lens' ListPlaybackConfigurations (Prelude.Maybe Prelude.Text)
listPlaybackConfigurations_nextToken = Lens.lens (\ListPlaybackConfigurations' {nextToken} -> nextToken) (\s@ListPlaybackConfigurations' {} a -> s {nextToken = a} :: ListPlaybackConfigurations)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPlaybackConfigurationsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPlaybackConfigurations where
  hashWithSalt _salt ListPlaybackConfigurations' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPlaybackConfigurations where
  rnf ListPlaybackConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPlaybackConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPlaybackConfigurations where
  toPath = Prelude.const "/playbackConfigurations"

instance Data.ToQuery ListPlaybackConfigurations where
  toQuery ListPlaybackConfigurations' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
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
  where
  rnf ListPlaybackConfigurationsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
