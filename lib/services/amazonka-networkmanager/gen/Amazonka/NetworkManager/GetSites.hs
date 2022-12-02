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
-- Module      : Amazonka.NetworkManager.GetSites
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more of your sites in a global network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetSites
  ( -- * Creating a Request
    GetSites (..),
    newGetSites,

    -- * Request Lenses
    getSites_nextToken,
    getSites_siteIds,
    getSites_maxResults,
    getSites_globalNetworkId,

    -- * Destructuring the Response
    GetSitesResponse (..),
    newGetSitesResponse,

    -- * Response Lenses
    getSitesResponse_sites,
    getSitesResponse_nextToken,
    getSitesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSites' smart constructor.
data GetSites = GetSites'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more site IDs. The maximum is 10.
    siteIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSites' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSites_nextToken' - The token for the next page of results.
--
-- 'siteIds', 'getSites_siteIds' - One or more site IDs. The maximum is 10.
--
-- 'maxResults', 'getSites_maxResults' - The maximum number of results to return.
--
-- 'globalNetworkId', 'getSites_globalNetworkId' - The ID of the global network.
newGetSites ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetSites
newGetSites pGlobalNetworkId_ =
  GetSites'
    { nextToken = Prelude.Nothing,
      siteIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The token for the next page of results.
getSites_nextToken :: Lens.Lens' GetSites (Prelude.Maybe Prelude.Text)
getSites_nextToken = Lens.lens (\GetSites' {nextToken} -> nextToken) (\s@GetSites' {} a -> s {nextToken = a} :: GetSites)

-- | One or more site IDs. The maximum is 10.
getSites_siteIds :: Lens.Lens' GetSites (Prelude.Maybe [Prelude.Text])
getSites_siteIds = Lens.lens (\GetSites' {siteIds} -> siteIds) (\s@GetSites' {} a -> s {siteIds = a} :: GetSites) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
getSites_maxResults :: Lens.Lens' GetSites (Prelude.Maybe Prelude.Natural)
getSites_maxResults = Lens.lens (\GetSites' {maxResults} -> maxResults) (\s@GetSites' {} a -> s {maxResults = a} :: GetSites)

-- | The ID of the global network.
getSites_globalNetworkId :: Lens.Lens' GetSites Prelude.Text
getSites_globalNetworkId = Lens.lens (\GetSites' {globalNetworkId} -> globalNetworkId) (\s@GetSites' {} a -> s {globalNetworkId = a} :: GetSites)

instance Core.AWSPager GetSites where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSitesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSitesResponse_sites Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getSites_nextToken
          Lens..~ rs
          Lens.^? getSitesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetSites where
  type AWSResponse GetSites = GetSitesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSitesResponse'
            Prelude.<$> (x Data..?> "Sites" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSites where
  hashWithSalt _salt GetSites' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` siteIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData GetSites where
  rnf GetSites' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf siteIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders GetSites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSites where
  toPath GetSites' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/sites"
      ]

instance Data.ToQuery GetSites where
  toQuery GetSites' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "siteIds"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> siteIds),
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newGetSitesResponse' smart constructor.
data GetSitesResponse = GetSitesResponse'
  { -- | The sites.
    sites :: Prelude.Maybe [Site],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSitesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sites', 'getSitesResponse_sites' - The sites.
--
-- 'nextToken', 'getSitesResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getSitesResponse_httpStatus' - The response's http status code.
newGetSitesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSitesResponse
newGetSitesResponse pHttpStatus_ =
  GetSitesResponse'
    { sites = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The sites.
getSitesResponse_sites :: Lens.Lens' GetSitesResponse (Prelude.Maybe [Site])
getSitesResponse_sites = Lens.lens (\GetSitesResponse' {sites} -> sites) (\s@GetSitesResponse' {} a -> s {sites = a} :: GetSitesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getSitesResponse_nextToken :: Lens.Lens' GetSitesResponse (Prelude.Maybe Prelude.Text)
getSitesResponse_nextToken = Lens.lens (\GetSitesResponse' {nextToken} -> nextToken) (\s@GetSitesResponse' {} a -> s {nextToken = a} :: GetSitesResponse)

-- | The response's http status code.
getSitesResponse_httpStatus :: Lens.Lens' GetSitesResponse Prelude.Int
getSitesResponse_httpStatus = Lens.lens (\GetSitesResponse' {httpStatus} -> httpStatus) (\s@GetSitesResponse' {} a -> s {httpStatus = a} :: GetSitesResponse)

instance Prelude.NFData GetSitesResponse where
  rnf GetSitesResponse' {..} =
    Prelude.rnf sites
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
