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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List studios in your Amazon Web Services account in the requested Amazon
-- Web Services Region.
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
    listStudiosResponse_studios,
    listStudiosResponse_nextToken,
    listStudiosResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
            Lens.^? listStudiosResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStudiosResponse_studios Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStudios_nextToken
          Lens..~ rs
          Lens.^? listStudiosResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListStudios where
  type AWSResponse ListStudios = ListStudiosResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStudiosResponse'
            Prelude.<$> (x Core..?> "studios" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStudios

instance Prelude.NFData ListStudios

instance Core.ToHeaders ListStudios where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListStudios where
  toPath = Prelude.const "/2020-08-01/studios"

instance Core.ToQuery ListStudios where
  toQuery ListStudios' {..} =
    Prelude.mconcat ["nextToken" Core.=: nextToken]

-- | /See:/ 'newListStudiosResponse' smart constructor.
data ListStudiosResponse = ListStudiosResponse'
  { -- | A collection of studios.
    studios :: Prelude.Maybe [Studio],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStudiosResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studios', 'listStudiosResponse_studios' - A collection of studios.
--
-- 'nextToken', 'listStudiosResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listStudiosResponse_httpStatus' - The response's http status code.
newListStudiosResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStudiosResponse
newListStudiosResponse pHttpStatus_ =
  ListStudiosResponse'
    { studios = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of studios.
listStudiosResponse_studios :: Lens.Lens' ListStudiosResponse (Prelude.Maybe [Studio])
listStudiosResponse_studios = Lens.lens (\ListStudiosResponse' {studios} -> studios) (\s@ListStudiosResponse' {} a -> s {studios = a} :: ListStudiosResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listStudiosResponse_nextToken :: Lens.Lens' ListStudiosResponse (Prelude.Maybe Prelude.Text)
listStudiosResponse_nextToken = Lens.lens (\ListStudiosResponse' {nextToken} -> nextToken) (\s@ListStudiosResponse' {} a -> s {nextToken = a} :: ListStudiosResponse)

-- | The response's http status code.
listStudiosResponse_httpStatus :: Lens.Lens' ListStudiosResponse Prelude.Int
listStudiosResponse_httpStatus = Lens.lens (\ListStudiosResponse' {httpStatus} -> httpStatus) (\s@ListStudiosResponse' {} a -> s {httpStatus = a} :: ListStudiosResponse)

instance Prelude.NFData ListStudiosResponse
