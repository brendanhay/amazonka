{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.ListMultiplexes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the existing multiplexes.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexes
  ( -- * Creating a Request
    ListMultiplexes (..),
    newListMultiplexes,

    -- * Request Lenses
    listMultiplexes_nextToken,
    listMultiplexes_maxResults,

    -- * Destructuring the Response
    ListMultiplexesResponse (..),
    newListMultiplexesResponse,

    -- * Response Lenses
    listMultiplexesResponse_nextToken,
    listMultiplexesResponse_multiplexes,
    listMultiplexesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListMultiplexesRequest
--
-- /See:/ 'newListMultiplexes' smart constructor.
data ListMultiplexes = ListMultiplexes'
  { -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListMultiplexes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMultiplexes_nextToken' - The token to retrieve the next page of results.
--
-- 'maxResults', 'listMultiplexes_maxResults' - The maximum number of items to return.
newListMultiplexes ::
  ListMultiplexes
newListMultiplexes =
  ListMultiplexes'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to retrieve the next page of results.
listMultiplexes_nextToken :: Lens.Lens' ListMultiplexes (Prelude.Maybe Prelude.Text)
listMultiplexes_nextToken = Lens.lens (\ListMultiplexes' {nextToken} -> nextToken) (\s@ListMultiplexes' {} a -> s {nextToken = a} :: ListMultiplexes)

-- | The maximum number of items to return.
listMultiplexes_maxResults :: Lens.Lens' ListMultiplexes (Prelude.Maybe Prelude.Natural)
listMultiplexes_maxResults = Lens.lens (\ListMultiplexes' {maxResults} -> maxResults) (\s@ListMultiplexes' {} a -> s {maxResults = a} :: ListMultiplexes)

instance Pager.AWSPager ListMultiplexes where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listMultiplexesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listMultiplexesResponse_multiplexes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listMultiplexes_nextToken
          Lens..~ rs
          Lens.^? listMultiplexesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListMultiplexes where
  type Rs ListMultiplexes = ListMultiplexesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultiplexesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "multiplexes"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultiplexes

instance Prelude.NFData ListMultiplexes

instance Prelude.ToHeaders ListMultiplexes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListMultiplexes where
  toPath = Prelude.const "/prod/multiplexes"

instance Prelude.ToQuery ListMultiplexes where
  toQuery ListMultiplexes' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | Placeholder documentation for ListMultiplexesResponse
--
-- /See:/ 'newListMultiplexesResponse' smart constructor.
data ListMultiplexesResponse = ListMultiplexesResponse'
  { -- | Token for the next ListMultiplexes request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of multiplexes.
    multiplexes :: Prelude.Maybe [MultiplexSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListMultiplexesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMultiplexesResponse_nextToken' - Token for the next ListMultiplexes request.
--
-- 'multiplexes', 'listMultiplexesResponse_multiplexes' - List of multiplexes.
--
-- 'httpStatus', 'listMultiplexesResponse_httpStatus' - The response's http status code.
newListMultiplexesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMultiplexesResponse
newListMultiplexesResponse pHttpStatus_ =
  ListMultiplexesResponse'
    { nextToken =
        Prelude.Nothing,
      multiplexes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next ListMultiplexes request.
listMultiplexesResponse_nextToken :: Lens.Lens' ListMultiplexesResponse (Prelude.Maybe Prelude.Text)
listMultiplexesResponse_nextToken = Lens.lens (\ListMultiplexesResponse' {nextToken} -> nextToken) (\s@ListMultiplexesResponse' {} a -> s {nextToken = a} :: ListMultiplexesResponse)

-- | List of multiplexes.
listMultiplexesResponse_multiplexes :: Lens.Lens' ListMultiplexesResponse (Prelude.Maybe [MultiplexSummary])
listMultiplexesResponse_multiplexes = Lens.lens (\ListMultiplexesResponse' {multiplexes} -> multiplexes) (\s@ListMultiplexesResponse' {} a -> s {multiplexes = a} :: ListMultiplexesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listMultiplexesResponse_httpStatus :: Lens.Lens' ListMultiplexesResponse Prelude.Int
listMultiplexesResponse_httpStatus = Lens.lens (\ListMultiplexesResponse' {httpStatus} -> httpStatus) (\s@ListMultiplexesResponse' {} a -> s {httpStatus = a} :: ListMultiplexesResponse)

instance Prelude.NFData ListMultiplexesResponse
