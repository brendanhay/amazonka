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
-- Module      : Amazonka.Nimble.ListStudios
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List studios in your Amazon Web Services accounts in the requested
-- Amazon Web Services Region.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListStudios
  ( -- * Creating a Request
    ListStudios (..),
    newListStudios,

    -- * Request Lenses
    listStudios_nextToken,

    -- * Destructuring the Response
    ListStudiosResponse (..),
    newListStudiosResponse,

    -- * Response Lenses
    listStudiosResponse_nextToken,
    listStudiosResponse_httpStatus,
    listStudiosResponse_studios,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStudios' smart constructor.
data ListStudios = ListStudios'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudios' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStudios_nextToken' - The token for the next set of results, or null if there are no more
-- results.
newListStudios ::
  ListStudios
newListStudios =
  ListStudios' {nextToken = Prelude.Nothing}

-- | The token for the next set of results, or null if there are no more
-- results.
listStudios_nextToken :: Lens.Lens' ListStudios (Prelude.Maybe Prelude.Text)
listStudios_nextToken = Lens.lens (\ListStudios' {nextToken} -> nextToken) (\s@ListStudios' {} a -> s {nextToken = a} :: ListStudios)

instance Core.AWSPager ListStudios where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStudiosResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listStudiosResponse_studios) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStudios_nextToken
          Lens..~ rs
          Lens.^? listStudiosResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListStudios where
  type AWSResponse ListStudios = ListStudiosResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "studios" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListStudios where
  hashWithSalt _salt ListStudios' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStudios where
  rnf ListStudios' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListStudios where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStudios where
  toPath = Prelude.const "/2020-08-01/studios"

instance Data.ToQuery ListStudios where
  toQuery ListStudios' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | /See:/ 'newListStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A collection of studios.
    studios :: [Studio]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudiosResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStudiosResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listStudiosResponse_httpStatus' - The response's http status code.
--
-- 'studios', 'listStudiosResponse_studios' - A collection of studios.
newListStudiosResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudiosResponse
newListStudiosResponse pHttpStatus_ =
  ListStudiosResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      studios = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStudiosResponse_nextToken :: Lens.Lens' ListStudiosResponse (Prelude.Maybe Prelude.Text)
listStudiosResponse_nextToken = Lens.lens (\ListStudiosResponse' {nextToken} -> nextToken) (\s@ListStudiosResponse' {} a -> s {nextToken = a} :: ListStudiosResponse)

-- | The response's http status code.
listStudiosResponse_httpStatus :: Lens.Lens' ListStudiosResponse Prelude.Int
listStudiosResponse_httpStatus = Lens.lens (\ListStudiosResponse' {httpStatus} -> httpStatus) (\s@ListStudiosResponse' {} a -> s {httpStatus = a} :: ListStudiosResponse)

-- | A collection of studios.
listStudiosResponse_studios :: Lens.Lens' ListStudiosResponse [Studio]
listStudiosResponse_studios = Lens.lens (\ListStudiosResponse' {studios} -> studios) (\s@ListStudiosResponse' {} a -> s {studios = a} :: ListStudiosResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListStudiosResponse where
  rnf ListStudiosResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf studios
