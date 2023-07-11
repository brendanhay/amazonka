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
-- Module      : Amazonka.GroundStation.ListSatellites
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of satellites.
--
-- This operation returns paginated results.
module Amazonka.GroundStation.ListSatellites
  ( -- * Creating a Request
    ListSatellites (..),
    newListSatellites,

    -- * Request Lenses
    listSatellites_maxResults,
    listSatellites_nextToken,

    -- * Destructuring the Response
    ListSatellitesResponse (..),
    newListSatellitesResponse,

    -- * Response Lenses
    listSatellitesResponse_nextToken,
    listSatellitesResponse_satellites,
    listSatellitesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newListSatellites' smart constructor.
data ListSatellites = ListSatellites'
  { -- | Maximum number of satellites returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token that can be supplied in the next call to get the next page of
    -- satellites.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSatellites' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSatellites_maxResults' - Maximum number of satellites returned.
--
-- 'nextToken', 'listSatellites_nextToken' - Next token that can be supplied in the next call to get the next page of
-- satellites.
newListSatellites ::
  ListSatellites
newListSatellites =
  ListSatellites'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of satellites returned.
listSatellites_maxResults :: Lens.Lens' ListSatellites (Prelude.Maybe Prelude.Natural)
listSatellites_maxResults = Lens.lens (\ListSatellites' {maxResults} -> maxResults) (\s@ListSatellites' {} a -> s {maxResults = a} :: ListSatellites)

-- | Next token that can be supplied in the next call to get the next page of
-- satellites.
listSatellites_nextToken :: Lens.Lens' ListSatellites (Prelude.Maybe Prelude.Text)
listSatellites_nextToken = Lens.lens (\ListSatellites' {nextToken} -> nextToken) (\s@ListSatellites' {} a -> s {nextToken = a} :: ListSatellites)

instance Core.AWSPager ListSatellites where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSatellitesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSatellitesResponse_satellites
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSatellites_nextToken
          Lens..~ rs
          Lens.^? listSatellitesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSatellites where
  type
    AWSResponse ListSatellites =
      ListSatellitesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSatellitesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "satellites" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSatellites where
  hashWithSalt _salt ListSatellites' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSatellites where
  rnf ListSatellites' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSatellites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSatellites where
  toPath = Prelude.const "/satellite"

instance Data.ToQuery ListSatellites where
  toQuery ListSatellites' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- |
--
-- /See:/ 'newListSatellitesResponse' smart constructor.
data ListSatellitesResponse = ListSatellitesResponse'
  { -- | Next token that can be supplied in the next call to get the next page of
    -- satellites.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of satellites.
    satellites :: Prelude.Maybe [SatelliteListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSatellitesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSatellitesResponse_nextToken' - Next token that can be supplied in the next call to get the next page of
-- satellites.
--
-- 'satellites', 'listSatellitesResponse_satellites' - List of satellites.
--
-- 'httpStatus', 'listSatellitesResponse_httpStatus' - The response's http status code.
newListSatellitesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSatellitesResponse
newListSatellitesResponse pHttpStatus_ =
  ListSatellitesResponse'
    { nextToken =
        Prelude.Nothing,
      satellites = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Next token that can be supplied in the next call to get the next page of
-- satellites.
listSatellitesResponse_nextToken :: Lens.Lens' ListSatellitesResponse (Prelude.Maybe Prelude.Text)
listSatellitesResponse_nextToken = Lens.lens (\ListSatellitesResponse' {nextToken} -> nextToken) (\s@ListSatellitesResponse' {} a -> s {nextToken = a} :: ListSatellitesResponse)

-- | List of satellites.
listSatellitesResponse_satellites :: Lens.Lens' ListSatellitesResponse (Prelude.Maybe [SatelliteListItem])
listSatellitesResponse_satellites = Lens.lens (\ListSatellitesResponse' {satellites} -> satellites) (\s@ListSatellitesResponse' {} a -> s {satellites = a} :: ListSatellitesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSatellitesResponse_httpStatus :: Lens.Lens' ListSatellitesResponse Prelude.Int
listSatellitesResponse_httpStatus = Lens.lens (\ListSatellitesResponse' {httpStatus} -> httpStatus) (\s@ListSatellitesResponse' {} a -> s {httpStatus = a} :: ListSatellitesResponse)

instance Prelude.NFData ListSatellitesResponse where
  rnf ListSatellitesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf satellites
      `Prelude.seq` Prelude.rnf httpStatus
