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
-- Module      : Amazonka.MediaLive.ListMultiplexes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the existing multiplexes.
--
-- This operation returns paginated results.
module Amazonka.MediaLive.ListMultiplexes
  ( -- * Creating a Request
    ListMultiplexes (..),
    newListMultiplexes,

    -- * Request Lenses
    listMultiplexes_maxResults,
    listMultiplexes_nextToken,

    -- * Destructuring the Response
    ListMultiplexesResponse (..),
    newListMultiplexesResponse,

    -- * Response Lenses
    listMultiplexesResponse_multiplexes,
    listMultiplexesResponse_nextToken,
    listMultiplexesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListMultiplexesRequest
--
-- /See:/ 'newListMultiplexes' smart constructor.
data ListMultiplexes = ListMultiplexes'
  { -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultiplexes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMultiplexes_maxResults' - The maximum number of items to return.
--
-- 'nextToken', 'listMultiplexes_nextToken' - The token to retrieve the next page of results.
newListMultiplexes ::
  ListMultiplexes
newListMultiplexes =
  ListMultiplexes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return.
listMultiplexes_maxResults :: Lens.Lens' ListMultiplexes (Prelude.Maybe Prelude.Natural)
listMultiplexes_maxResults = Lens.lens (\ListMultiplexes' {maxResults} -> maxResults) (\s@ListMultiplexes' {} a -> s {maxResults = a} :: ListMultiplexes)

-- | The token to retrieve the next page of results.
listMultiplexes_nextToken :: Lens.Lens' ListMultiplexes (Prelude.Maybe Prelude.Text)
listMultiplexes_nextToken = Lens.lens (\ListMultiplexes' {nextToken} -> nextToken) (\s@ListMultiplexes' {} a -> s {nextToken = a} :: ListMultiplexes)

instance Core.AWSPager ListMultiplexes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMultiplexesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMultiplexesResponse_multiplexes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listMultiplexes_nextToken
              Lens..~ rs
              Lens.^? listMultiplexesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListMultiplexes where
  type
    AWSResponse ListMultiplexes =
      ListMultiplexesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultiplexesResponse'
            Prelude.<$> (x Data..?> "multiplexes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultiplexes where
  hashWithSalt _salt ListMultiplexes' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMultiplexes where
  rnf ListMultiplexes' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListMultiplexes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMultiplexes where
  toPath = Prelude.const "/prod/multiplexes"

instance Data.ToQuery ListMultiplexes where
  toQuery ListMultiplexes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Placeholder documentation for ListMultiplexesResponse
--
-- /See:/ 'newListMultiplexesResponse' smart constructor.
data ListMultiplexesResponse = ListMultiplexesResponse'
  { -- | List of multiplexes.
    multiplexes :: Prelude.Maybe [MultiplexSummary],
    -- | Token for the next ListMultiplexes request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultiplexesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexes', 'listMultiplexesResponse_multiplexes' - List of multiplexes.
--
-- 'nextToken', 'listMultiplexesResponse_nextToken' - Token for the next ListMultiplexes request.
--
-- 'httpStatus', 'listMultiplexesResponse_httpStatus' - The response's http status code.
newListMultiplexesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMultiplexesResponse
newListMultiplexesResponse pHttpStatus_ =
  ListMultiplexesResponse'
    { multiplexes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of multiplexes.
listMultiplexesResponse_multiplexes :: Lens.Lens' ListMultiplexesResponse (Prelude.Maybe [MultiplexSummary])
listMultiplexesResponse_multiplexes = Lens.lens (\ListMultiplexesResponse' {multiplexes} -> multiplexes) (\s@ListMultiplexesResponse' {} a -> s {multiplexes = a} :: ListMultiplexesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next ListMultiplexes request.
listMultiplexesResponse_nextToken :: Lens.Lens' ListMultiplexesResponse (Prelude.Maybe Prelude.Text)
listMultiplexesResponse_nextToken = Lens.lens (\ListMultiplexesResponse' {nextToken} -> nextToken) (\s@ListMultiplexesResponse' {} a -> s {nextToken = a} :: ListMultiplexesResponse)

-- | The response's http status code.
listMultiplexesResponse_httpStatus :: Lens.Lens' ListMultiplexesResponse Prelude.Int
listMultiplexesResponse_httpStatus = Lens.lens (\ListMultiplexesResponse' {httpStatus} -> httpStatus) (\s@ListMultiplexesResponse' {} a -> s {httpStatus = a} :: ListMultiplexesResponse)

instance Prelude.NFData ListMultiplexesResponse where
  rnf ListMultiplexesResponse' {..} =
    Prelude.rnf multiplexes `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
