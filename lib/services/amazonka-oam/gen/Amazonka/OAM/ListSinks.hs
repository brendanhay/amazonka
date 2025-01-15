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
-- Module      : Amazonka.OAM.ListSinks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation in a monitoring account to return the list of sinks
-- created in that account.
--
-- This operation returns paginated results.
module Amazonka.OAM.ListSinks
  ( -- * Creating a Request
    ListSinks (..),
    newListSinks,

    -- * Request Lenses
    listSinks_maxResults,
    listSinks_nextToken,

    -- * Destructuring the Response
    ListSinksResponse (..),
    newListSinksResponse,

    -- * Response Lenses
    listSinksResponse_nextToken,
    listSinksResponse_httpStatus,
    listSinksResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSinks' smart constructor.
data ListSinks = ListSinks'
  { -- | Limits the number of returned links to the specified number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. You received this token
    -- from a previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSinks_maxResults' - Limits the number of returned links to the specified number.
--
-- 'nextToken', 'listSinks_nextToken' - The token for the next set of items to return. You received this token
-- from a previous call.
newListSinks ::
  ListSinks
newListSinks =
  ListSinks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Limits the number of returned links to the specified number.
listSinks_maxResults :: Lens.Lens' ListSinks (Prelude.Maybe Prelude.Natural)
listSinks_maxResults = Lens.lens (\ListSinks' {maxResults} -> maxResults) (\s@ListSinks' {} a -> s {maxResults = a} :: ListSinks)

-- | The token for the next set of items to return. You received this token
-- from a previous call.
listSinks_nextToken :: Lens.Lens' ListSinks (Prelude.Maybe Prelude.Text)
listSinks_nextToken = Lens.lens (\ListSinks' {nextToken} -> nextToken) (\s@ListSinks' {} a -> s {nextToken = a} :: ListSinks)

instance Core.AWSPager ListSinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSinksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listSinksResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listSinks_nextToken
              Lens..~ rs
              Lens.^? listSinksResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListSinks where
  type AWSResponse ListSinks = ListSinksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSinksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListSinks where
  hashWithSalt _salt ListSinks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSinks where
  rnf ListSinks' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListSinks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSinks where
  toJSON ListSinks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListSinks where
  toPath = Prelude.const "/ListSinks"

instance Data.ToQuery ListSinks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSinksResponse' smart constructor.
data ListSinksResponse = ListSinksResponse'
  { -- | The token to use when requesting the next set of sinks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of structures that contain the information about the returned
    -- sinks.
    items :: [ListSinksItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSinksResponse_nextToken' - The token to use when requesting the next set of sinks.
--
-- 'httpStatus', 'listSinksResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listSinksResponse_items' - An array of structures that contain the information about the returned
-- sinks.
newListSinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSinksResponse
newListSinksResponse pHttpStatus_ =
  ListSinksResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | The token to use when requesting the next set of sinks.
listSinksResponse_nextToken :: Lens.Lens' ListSinksResponse (Prelude.Maybe Prelude.Text)
listSinksResponse_nextToken = Lens.lens (\ListSinksResponse' {nextToken} -> nextToken) (\s@ListSinksResponse' {} a -> s {nextToken = a} :: ListSinksResponse)

-- | The response's http status code.
listSinksResponse_httpStatus :: Lens.Lens' ListSinksResponse Prelude.Int
listSinksResponse_httpStatus = Lens.lens (\ListSinksResponse' {httpStatus} -> httpStatus) (\s@ListSinksResponse' {} a -> s {httpStatus = a} :: ListSinksResponse)

-- | An array of structures that contain the information about the returned
-- sinks.
listSinksResponse_items :: Lens.Lens' ListSinksResponse [ListSinksItem]
listSinksResponse_items = Lens.lens (\ListSinksResponse' {items} -> items) (\s@ListSinksResponse' {} a -> s {items = a} :: ListSinksResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListSinksResponse where
  rnf ListSinksResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf items
