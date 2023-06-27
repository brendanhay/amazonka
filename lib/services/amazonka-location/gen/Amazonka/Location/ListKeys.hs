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
-- Module      : Amazonka.Location.ListKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists API key resources in your Amazon Web Services account.
--
-- The API keys feature is in preview. We may add, change, or remove
-- features before announcing general availability. For more information,
-- see
-- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html Using API keys>.
--
-- This operation returns paginated results.
module Amazonka.Location.ListKeys
  ( -- * Creating a Request
    ListKeys (..),
    newListKeys,

    -- * Request Lenses
    listKeys_filter,
    listKeys_maxResults,
    listKeys_nextToken,

    -- * Destructuring the Response
    ListKeysResponse (..),
    newListKeysResponse,

    -- * Response Lenses
    listKeysResponse_nextToken,
    listKeysResponse_httpStatus,
    listKeysResponse_entries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeys' smart constructor.
data ListKeys = ListKeys'
  { -- | Optionally filter the list to only @Active@ or @Expired@ API keys.
    filter' :: Prelude.Maybe ApiKeyFilter,
    -- | An optional limit for the number of resources returned in a single call.
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
-- Create a value of 'ListKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listKeys_filter' - Optionally filter the list to only @Active@ or @Expired@ API keys.
--
-- 'maxResults', 'listKeys_maxResults' - An optional limit for the number of resources returned in a single call.
--
-- Default value: @100@
--
-- 'nextToken', 'listKeys_nextToken' - The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
newListKeys ::
  ListKeys
newListKeys =
  ListKeys'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Optionally filter the list to only @Active@ or @Expired@ API keys.
listKeys_filter :: Lens.Lens' ListKeys (Prelude.Maybe ApiKeyFilter)
listKeys_filter = Lens.lens (\ListKeys' {filter'} -> filter') (\s@ListKeys' {} a -> s {filter' = a} :: ListKeys)

-- | An optional limit for the number of resources returned in a single call.
--
-- Default value: @100@
listKeys_maxResults :: Lens.Lens' ListKeys (Prelude.Maybe Prelude.Natural)
listKeys_maxResults = Lens.lens (\ListKeys' {maxResults} -> maxResults) (\s@ListKeys' {} a -> s {maxResults = a} :: ListKeys)

-- | The pagination token specifying which page of results to return in the
-- response. If no token is provided, the default page is the first page.
--
-- Default value: @null@
listKeys_nextToken :: Lens.Lens' ListKeys (Prelude.Maybe Prelude.Text)
listKeys_nextToken = Lens.lens (\ListKeys' {nextToken} -> nextToken) (\s@ListKeys' {} a -> s {nextToken = a} :: ListKeys)

instance Core.AWSPager ListKeys where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeysResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listKeysResponse_entries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listKeys_nextToken
          Lens..~ rs
          Lens.^? listKeysResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListKeys where
  type AWSResponse ListKeys = ListKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeysResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Entries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListKeys where
  hashWithSalt _salt ListKeys' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListKeys where
  rnf ListKeys' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListKeys where
  toJSON ListKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListKeys where
  toPath = Prelude.const "/metadata/v0/list-keys"

instance Data.ToQuery ListKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeysResponse' smart constructor.
data ListKeysResponse = ListKeysResponse'
  { -- | A pagination token indicating there are additional pages available. You
    -- can use the token in a following request to fetch the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Contains API key resources in your Amazon Web Services account. Details
    -- include API key name, allowed referers and timestamp for when the API
    -- key will expire.
    entries :: [ListKeysResponseEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKeysResponse_nextToken' - A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
--
-- 'httpStatus', 'listKeysResponse_httpStatus' - The response's http status code.
--
-- 'entries', 'listKeysResponse_entries' - Contains API key resources in your Amazon Web Services account. Details
-- include API key name, allowed referers and timestamp for when the API
-- key will expire.
newListKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeysResponse
newListKeysResponse pHttpStatus_ =
  ListKeysResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entries = Prelude.mempty
    }

-- | A pagination token indicating there are additional pages available. You
-- can use the token in a following request to fetch the next set of
-- results.
listKeysResponse_nextToken :: Lens.Lens' ListKeysResponse (Prelude.Maybe Prelude.Text)
listKeysResponse_nextToken = Lens.lens (\ListKeysResponse' {nextToken} -> nextToken) (\s@ListKeysResponse' {} a -> s {nextToken = a} :: ListKeysResponse)

-- | The response's http status code.
listKeysResponse_httpStatus :: Lens.Lens' ListKeysResponse Prelude.Int
listKeysResponse_httpStatus = Lens.lens (\ListKeysResponse' {httpStatus} -> httpStatus) (\s@ListKeysResponse' {} a -> s {httpStatus = a} :: ListKeysResponse)

-- | Contains API key resources in your Amazon Web Services account. Details
-- include API key name, allowed referers and timestamp for when the API
-- key will expire.
listKeysResponse_entries :: Lens.Lens' ListKeysResponse [ListKeysResponseEntry]
listKeysResponse_entries = Lens.lens (\ListKeysResponse' {entries} -> entries) (\s@ListKeysResponse' {} a -> s {entries = a} :: ListKeysResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListKeysResponse where
  rnf ListKeysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entries
