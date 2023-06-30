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
-- Module      : Amazonka.IoTRoboRunner.ListWorkers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to list workers
--
-- This operation returns paginated results.
module Amazonka.IoTRoboRunner.ListWorkers
  ( -- * Creating a Request
    ListWorkers (..),
    newListWorkers,

    -- * Request Lenses
    listWorkers_fleet,
    listWorkers_maxResults,
    listWorkers_nextToken,
    listWorkers_site,

    -- * Destructuring the Response
    ListWorkersResponse (..),
    newListWorkersResponse,

    -- * Response Lenses
    listWorkersResponse_nextToken,
    listWorkersResponse_workers,
    listWorkersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkers' smart constructor.
data ListWorkers = ListWorkers'
  { fleet :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleet', 'listWorkers_fleet' - Undocumented member.
--
-- 'maxResults', 'listWorkers_maxResults' - Undocumented member.
--
-- 'nextToken', 'listWorkers_nextToken' - Undocumented member.
--
-- 'site', 'listWorkers_site' - Undocumented member.
newListWorkers ::
  -- | 'site'
  Prelude.Text ->
  ListWorkers
newListWorkers pSite_ =
  ListWorkers'
    { fleet = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      site = pSite_
    }

-- | Undocumented member.
listWorkers_fleet :: Lens.Lens' ListWorkers (Prelude.Maybe Prelude.Text)
listWorkers_fleet = Lens.lens (\ListWorkers' {fleet} -> fleet) (\s@ListWorkers' {} a -> s {fleet = a} :: ListWorkers)

-- | Undocumented member.
listWorkers_maxResults :: Lens.Lens' ListWorkers (Prelude.Maybe Prelude.Natural)
listWorkers_maxResults = Lens.lens (\ListWorkers' {maxResults} -> maxResults) (\s@ListWorkers' {} a -> s {maxResults = a} :: ListWorkers)

-- | Undocumented member.
listWorkers_nextToken :: Lens.Lens' ListWorkers (Prelude.Maybe Prelude.Text)
listWorkers_nextToken = Lens.lens (\ListWorkers' {nextToken} -> nextToken) (\s@ListWorkers' {} a -> s {nextToken = a} :: ListWorkers)

-- | Undocumented member.
listWorkers_site :: Lens.Lens' ListWorkers Prelude.Text
listWorkers_site = Lens.lens (\ListWorkers' {site} -> site) (\s@ListWorkers' {} a -> s {site = a} :: ListWorkers)

instance Core.AWSPager ListWorkers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkersResponse_workers
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listWorkers_nextToken
          Lens..~ rs
          Lens.^? listWorkersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListWorkers where
  type AWSResponse ListWorkers = ListWorkersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkersResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "workers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkers where
  hashWithSalt _salt ListWorkers' {..} =
    _salt
      `Prelude.hashWithSalt` fleet
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` site

instance Prelude.NFData ListWorkers where
  rnf ListWorkers' {..} =
    Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf site

instance Data.ToHeaders ListWorkers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkers where
  toPath = Prelude.const "/listWorkers"

instance Data.ToQuery ListWorkers where
  toQuery ListWorkers' {..} =
    Prelude.mconcat
      [ "fleet" Data.=: fleet,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "site" Data.=: site
      ]

-- | /See:/ 'newListWorkersResponse' smart constructor.
data ListWorkersResponse = ListWorkersResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    workers :: Prelude.Maybe [Worker],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkersResponse_nextToken' - Undocumented member.
--
-- 'workers', 'listWorkersResponse_workers' - Undocumented member.
--
-- 'httpStatus', 'listWorkersResponse_httpStatus' - The response's http status code.
newListWorkersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkersResponse
newListWorkersResponse pHttpStatus_ =
  ListWorkersResponse'
    { nextToken = Prelude.Nothing,
      workers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listWorkersResponse_nextToken :: Lens.Lens' ListWorkersResponse (Prelude.Maybe Prelude.Text)
listWorkersResponse_nextToken = Lens.lens (\ListWorkersResponse' {nextToken} -> nextToken) (\s@ListWorkersResponse' {} a -> s {nextToken = a} :: ListWorkersResponse)

-- | Undocumented member.
listWorkersResponse_workers :: Lens.Lens' ListWorkersResponse (Prelude.Maybe [Worker])
listWorkersResponse_workers = Lens.lens (\ListWorkersResponse' {workers} -> workers) (\s@ListWorkersResponse' {} a -> s {workers = a} :: ListWorkersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkersResponse_httpStatus :: Lens.Lens' ListWorkersResponse Prelude.Int
listWorkersResponse_httpStatus = Lens.lens (\ListWorkersResponse' {httpStatus} -> httpStatus) (\s@ListWorkersResponse' {} a -> s {httpStatus = a} :: ListWorkersResponse)

instance Prelude.NFData ListWorkersResponse where
  rnf ListWorkersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workers
      `Prelude.seq` Prelude.rnf httpStatus
