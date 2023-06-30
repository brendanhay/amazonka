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
-- Module      : Amazonka.MediaLive.ListMultiplexPrograms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the programs that currently exist for a specific multiplex.
--
-- This operation returns paginated results.
module Amazonka.MediaLive.ListMultiplexPrograms
  ( -- * Creating a Request
    ListMultiplexPrograms (..),
    newListMultiplexPrograms,

    -- * Request Lenses
    listMultiplexPrograms_maxResults,
    listMultiplexPrograms_nextToken,
    listMultiplexPrograms_multiplexId,

    -- * Destructuring the Response
    ListMultiplexProgramsResponse (..),
    newListMultiplexProgramsResponse,

    -- * Response Lenses
    listMultiplexProgramsResponse_multiplexPrograms,
    listMultiplexProgramsResponse_nextToken,
    listMultiplexProgramsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListMultiplexProgramsRequest
--
-- /See:/ 'newListMultiplexPrograms' smart constructor.
data ListMultiplexPrograms = ListMultiplexPrograms'
  { -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the multiplex that the programs belong to.
    multiplexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultiplexPrograms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMultiplexPrograms_maxResults' - The maximum number of items to return.
--
-- 'nextToken', 'listMultiplexPrograms_nextToken' - The token to retrieve the next page of results.
--
-- 'multiplexId', 'listMultiplexPrograms_multiplexId' - The ID of the multiplex that the programs belong to.
newListMultiplexPrograms ::
  -- | 'multiplexId'
  Prelude.Text ->
  ListMultiplexPrograms
newListMultiplexPrograms pMultiplexId_ =
  ListMultiplexPrograms'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      multiplexId = pMultiplexId_
    }

-- | The maximum number of items to return.
listMultiplexPrograms_maxResults :: Lens.Lens' ListMultiplexPrograms (Prelude.Maybe Prelude.Natural)
listMultiplexPrograms_maxResults = Lens.lens (\ListMultiplexPrograms' {maxResults} -> maxResults) (\s@ListMultiplexPrograms' {} a -> s {maxResults = a} :: ListMultiplexPrograms)

-- | The token to retrieve the next page of results.
listMultiplexPrograms_nextToken :: Lens.Lens' ListMultiplexPrograms (Prelude.Maybe Prelude.Text)
listMultiplexPrograms_nextToken = Lens.lens (\ListMultiplexPrograms' {nextToken} -> nextToken) (\s@ListMultiplexPrograms' {} a -> s {nextToken = a} :: ListMultiplexPrograms)

-- | The ID of the multiplex that the programs belong to.
listMultiplexPrograms_multiplexId :: Lens.Lens' ListMultiplexPrograms Prelude.Text
listMultiplexPrograms_multiplexId = Lens.lens (\ListMultiplexPrograms' {multiplexId} -> multiplexId) (\s@ListMultiplexPrograms' {} a -> s {multiplexId = a} :: ListMultiplexPrograms)

instance Core.AWSPager ListMultiplexPrograms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMultiplexProgramsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMultiplexProgramsResponse_multiplexPrograms
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listMultiplexPrograms_nextToken
          Lens..~ rs
          Lens.^? listMultiplexProgramsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListMultiplexPrograms where
  type
    AWSResponse ListMultiplexPrograms =
      ListMultiplexProgramsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMultiplexProgramsResponse'
            Prelude.<$> ( x
                            Data..?> "multiplexPrograms"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMultiplexPrograms where
  hashWithSalt _salt ListMultiplexPrograms' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` multiplexId

instance Prelude.NFData ListMultiplexPrograms where
  rnf ListMultiplexPrograms' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf multiplexId

instance Data.ToHeaders ListMultiplexPrograms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListMultiplexPrograms where
  toPath ListMultiplexPrograms' {..} =
    Prelude.mconcat
      [ "/prod/multiplexes/",
        Data.toBS multiplexId,
        "/programs"
      ]

instance Data.ToQuery ListMultiplexPrograms where
  toQuery ListMultiplexPrograms' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Placeholder documentation for ListMultiplexProgramsResponse
--
-- /See:/ 'newListMultiplexProgramsResponse' smart constructor.
data ListMultiplexProgramsResponse = ListMultiplexProgramsResponse'
  { -- | List of multiplex programs.
    multiplexPrograms :: Prelude.Maybe [MultiplexProgramSummary],
    -- | Token for the next ListMultiplexProgram request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMultiplexProgramsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiplexPrograms', 'listMultiplexProgramsResponse_multiplexPrograms' - List of multiplex programs.
--
-- 'nextToken', 'listMultiplexProgramsResponse_nextToken' - Token for the next ListMultiplexProgram request.
--
-- 'httpStatus', 'listMultiplexProgramsResponse_httpStatus' - The response's http status code.
newListMultiplexProgramsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMultiplexProgramsResponse
newListMultiplexProgramsResponse pHttpStatus_ =
  ListMultiplexProgramsResponse'
    { multiplexPrograms =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of multiplex programs.
listMultiplexProgramsResponse_multiplexPrograms :: Lens.Lens' ListMultiplexProgramsResponse (Prelude.Maybe [MultiplexProgramSummary])
listMultiplexProgramsResponse_multiplexPrograms = Lens.lens (\ListMultiplexProgramsResponse' {multiplexPrograms} -> multiplexPrograms) (\s@ListMultiplexProgramsResponse' {} a -> s {multiplexPrograms = a} :: ListMultiplexProgramsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next ListMultiplexProgram request.
listMultiplexProgramsResponse_nextToken :: Lens.Lens' ListMultiplexProgramsResponse (Prelude.Maybe Prelude.Text)
listMultiplexProgramsResponse_nextToken = Lens.lens (\ListMultiplexProgramsResponse' {nextToken} -> nextToken) (\s@ListMultiplexProgramsResponse' {} a -> s {nextToken = a} :: ListMultiplexProgramsResponse)

-- | The response's http status code.
listMultiplexProgramsResponse_httpStatus :: Lens.Lens' ListMultiplexProgramsResponse Prelude.Int
listMultiplexProgramsResponse_httpStatus = Lens.lens (\ListMultiplexProgramsResponse' {httpStatus} -> httpStatus) (\s@ListMultiplexProgramsResponse' {} a -> s {httpStatus = a} :: ListMultiplexProgramsResponse)

instance Prelude.NFData ListMultiplexProgramsResponse where
  rnf ListMultiplexProgramsResponse' {..} =
    Prelude.rnf multiplexPrograms
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
