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
-- Module      : Amazonka.Outposts.ListSites
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the sites for the specified AWS account.
module Amazonka.Outposts.ListSites
  ( -- * Creating a Request
    ListSites (..),
    newListSites,

    -- * Request Lenses
    listSites_nextToken,
    listSites_maxResults,

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
import qualified Amazonka.Lens as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSites' smart constructor.
data ListSites = ListSites'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listSites_nextToken' - Undocumented member.
--
-- 'maxResults', 'listSites_maxResults' - Undocumented member.
newListSites ::
  ListSites
newListSites =
  ListSites'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
listSites_nextToken :: Lens.Lens' ListSites (Prelude.Maybe Prelude.Text)
listSites_nextToken = Lens.lens (\ListSites' {nextToken} -> nextToken) (\s@ListSites' {} a -> s {nextToken = a} :: ListSites)

-- | Undocumented member.
listSites_maxResults :: Lens.Lens' ListSites (Prelude.Maybe Prelude.Natural)
listSites_maxResults = Lens.lens (\ListSites' {maxResults} -> maxResults) (\s@ListSites' {} a -> s {maxResults = a} :: ListSites)

instance Core.AWSRequest ListSites where
  type AWSResponse ListSites = ListSitesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSitesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Sites" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSites where
  hashWithSalt _salt ListSites' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListSites where
  rnf ListSites' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListSites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListSites where
  toPath = Prelude.const "/sites"

instance Core.ToQuery ListSites where
  toQuery ListSites' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
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
