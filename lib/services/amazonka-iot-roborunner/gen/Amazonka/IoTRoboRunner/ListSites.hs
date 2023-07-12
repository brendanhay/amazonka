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
-- Module      : Amazonka.IoTRoboRunner.ListSites
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to list sites
--
-- This operation returns paginated results.
module Amazonka.IoTRoboRunner.ListSites
  ( -- * Creating a Request
    ListSites (..),
    newListSites,

    -- * Request Lenses
    listSites_maxResults,
    listSites_nextToken,

    -- * Destructuring the Response
    ListSitesResponse (..),
    newListSitesResponse,

    -- * Response Lenses
    listSitesResponse_nextToken,
    listSitesResponse_sites,
    listSitesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSites' smart constructor.
data ListSites = ListSites'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSites' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSites_maxResults' - Undocumented member.
--
-- 'nextToken', 'listSites_nextToken' - Undocumented member.
newListSites ::
  ListSites
newListSites =
  ListSites'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listSites_maxResults :: Lens.Lens' ListSites (Prelude.Maybe Prelude.Natural)
listSites_maxResults = Lens.lens (\ListSites' {maxResults} -> maxResults) (\s@ListSites' {} a -> s {maxResults = a} :: ListSites)

-- | Undocumented member.
listSites_nextToken :: Lens.Lens' ListSites (Prelude.Maybe Prelude.Text)
listSites_nextToken = Lens.lens (\ListSites' {nextToken} -> nextToken) (\s@ListSites' {} a -> s {nextToken = a} :: ListSites)

instance Core.AWSPager ListSites where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSitesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSitesResponse_sites
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSites_nextToken
          Lens..~ rs
          Lens.^? listSitesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSites where
  type AWSResponse ListSites = ListSitesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSitesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "sites" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSites where
  hashWithSalt _salt ListSites' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListSites where
  rnf ListSites' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListSites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSites where
  toPath = Prelude.const "/listSites"

instance Data.ToQuery ListSites where
  toQuery ListSites' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSitesResponse' smart constructor.
data ListSitesResponse = ListSitesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    sites :: Prelude.Maybe [Site],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSitesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSitesResponse_nextToken' - Undocumented member.
--
-- 'sites', 'listSitesResponse_sites' - Undocumented member.
--
-- 'httpStatus', 'listSitesResponse_httpStatus' - The response's http status code.
newListSitesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSitesResponse
newListSitesResponse pHttpStatus_ =
  ListSitesResponse'
    { nextToken = Prelude.Nothing,
      sites = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listSitesResponse_nextToken :: Lens.Lens' ListSitesResponse (Prelude.Maybe Prelude.Text)
listSitesResponse_nextToken = Lens.lens (\ListSitesResponse' {nextToken} -> nextToken) (\s@ListSitesResponse' {} a -> s {nextToken = a} :: ListSitesResponse)

-- | Undocumented member.
listSitesResponse_sites :: Lens.Lens' ListSitesResponse (Prelude.Maybe [Site])
listSitesResponse_sites = Lens.lens (\ListSitesResponse' {sites} -> sites) (\s@ListSitesResponse' {} a -> s {sites = a} :: ListSitesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSitesResponse_httpStatus :: Lens.Lens' ListSitesResponse Prelude.Int
listSitesResponse_httpStatus = Lens.lens (\ListSitesResponse' {httpStatus} -> httpStatus) (\s@ListSitesResponse' {} a -> s {httpStatus = a} :: ListSitesResponse)

instance Prelude.NFData ListSitesResponse where
  rnf ListSitesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sites
      `Prelude.seq` Prelude.rnf httpStatus
