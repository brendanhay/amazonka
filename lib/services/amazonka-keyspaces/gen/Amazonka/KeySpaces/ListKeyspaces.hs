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
-- Module      : Amazonka.KeySpaces.ListKeyspaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of keyspaces.
--
-- This operation returns paginated results.
module Amazonka.KeySpaces.ListKeyspaces
  ( -- * Creating a Request
    ListKeyspaces (..),
    newListKeyspaces,

    -- * Request Lenses
    listKeyspaces_maxResults,
    listKeyspaces_nextToken,

    -- * Destructuring the Response
    ListKeyspacesResponse (..),
    newListKeyspacesResponse,

    -- * Response Lenses
    listKeyspacesResponse_nextToken,
    listKeyspacesResponse_httpStatus,
    listKeyspacesResponse_keyspaces,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeyspaces' smart constructor.
data ListKeyspaces = ListKeyspaces'
  { -- | The total number of keyspaces to return in the output. If the total
    -- number of keyspaces available is more than the value specified, a
    -- @NextToken@ is provided in the output. To resume pagination, provide the
    -- @NextToken@ value as an argument of a subsequent API invocation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token. To resume pagination, provide the @NextToken@
    -- value as argument of a subsequent API invocation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeyspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKeyspaces_maxResults' - The total number of keyspaces to return in the output. If the total
-- number of keyspaces available is more than the value specified, a
-- @NextToken@ is provided in the output. To resume pagination, provide the
-- @NextToken@ value as an argument of a subsequent API invocation.
--
-- 'nextToken', 'listKeyspaces_nextToken' - The pagination token. To resume pagination, provide the @NextToken@
-- value as argument of a subsequent API invocation.
newListKeyspaces ::
  ListKeyspaces
newListKeyspaces =
  ListKeyspaces'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The total number of keyspaces to return in the output. If the total
-- number of keyspaces available is more than the value specified, a
-- @NextToken@ is provided in the output. To resume pagination, provide the
-- @NextToken@ value as an argument of a subsequent API invocation.
listKeyspaces_maxResults :: Lens.Lens' ListKeyspaces (Prelude.Maybe Prelude.Natural)
listKeyspaces_maxResults = Lens.lens (\ListKeyspaces' {maxResults} -> maxResults) (\s@ListKeyspaces' {} a -> s {maxResults = a} :: ListKeyspaces)

-- | The pagination token. To resume pagination, provide the @NextToken@
-- value as argument of a subsequent API invocation.
listKeyspaces_nextToken :: Lens.Lens' ListKeyspaces (Prelude.Maybe Prelude.Text)
listKeyspaces_nextToken = Lens.lens (\ListKeyspaces' {nextToken} -> nextToken) (\s@ListKeyspaces' {} a -> s {nextToken = a} :: ListKeyspaces)

instance Core.AWSPager ListKeyspaces where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listKeyspacesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listKeyspacesResponse_keyspaces) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listKeyspaces_nextToken
          Lens..~ rs
          Lens.^? listKeyspacesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListKeyspaces where
  type
    AWSResponse ListKeyspaces =
      ListKeyspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeyspacesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "keyspaces" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListKeyspaces where
  hashWithSalt _salt ListKeyspaces' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListKeyspaces where
  rnf ListKeyspaces' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListKeyspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.ListKeyspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListKeyspaces where
  toJSON ListKeyspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListKeyspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery ListKeyspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListKeyspacesResponse' smart constructor.
data ListKeyspacesResponse = ListKeyspacesResponse'
  { -- | A token to specify where to start paginating. This is the @NextToken@
    -- from a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of keyspaces.
    keyspaces :: [KeyspaceSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeyspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listKeyspacesResponse_nextToken' - A token to specify where to start paginating. This is the @NextToken@
-- from a previously truncated response.
--
-- 'httpStatus', 'listKeyspacesResponse_httpStatus' - The response's http status code.
--
-- 'keyspaces', 'listKeyspacesResponse_keyspaces' - A list of keyspaces.
newListKeyspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeyspacesResponse
newListKeyspacesResponse pHttpStatus_ =
  ListKeyspacesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      keyspaces = Prelude.mempty
    }

-- | A token to specify where to start paginating. This is the @NextToken@
-- from a previously truncated response.
listKeyspacesResponse_nextToken :: Lens.Lens' ListKeyspacesResponse (Prelude.Maybe Prelude.Text)
listKeyspacesResponse_nextToken = Lens.lens (\ListKeyspacesResponse' {nextToken} -> nextToken) (\s@ListKeyspacesResponse' {} a -> s {nextToken = a} :: ListKeyspacesResponse)

-- | The response's http status code.
listKeyspacesResponse_httpStatus :: Lens.Lens' ListKeyspacesResponse Prelude.Int
listKeyspacesResponse_httpStatus = Lens.lens (\ListKeyspacesResponse' {httpStatus} -> httpStatus) (\s@ListKeyspacesResponse' {} a -> s {httpStatus = a} :: ListKeyspacesResponse)

-- | A list of keyspaces.
listKeyspacesResponse_keyspaces :: Lens.Lens' ListKeyspacesResponse [KeyspaceSummary]
listKeyspacesResponse_keyspaces = Lens.lens (\ListKeyspacesResponse' {keyspaces} -> keyspaces) (\s@ListKeyspacesResponse' {} a -> s {keyspaces = a} :: ListKeyspacesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListKeyspacesResponse where
  rnf ListKeyspacesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyspaces
