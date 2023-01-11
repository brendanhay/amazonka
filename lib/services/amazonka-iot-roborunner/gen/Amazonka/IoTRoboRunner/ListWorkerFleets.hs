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
-- Module      : Amazonka.IoTRoboRunner.ListWorkerFleets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to list worker fleets
--
-- This operation returns paginated results.
module Amazonka.IoTRoboRunner.ListWorkerFleets
  ( -- * Creating a Request
    ListWorkerFleets (..),
    newListWorkerFleets,

    -- * Request Lenses
    listWorkerFleets_maxResults,
    listWorkerFleets_nextToken,
    listWorkerFleets_site,

    -- * Destructuring the Response
    ListWorkerFleetsResponse (..),
    newListWorkerFleetsResponse,

    -- * Response Lenses
    listWorkerFleetsResponse_nextToken,
    listWorkerFleetsResponse_workerFleets,
    listWorkerFleetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkerFleets' smart constructor.
data ListWorkerFleets = ListWorkerFleets'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkerFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listWorkerFleets_maxResults' - Undocumented member.
--
-- 'nextToken', 'listWorkerFleets_nextToken' - Undocumented member.
--
-- 'site', 'listWorkerFleets_site' - Undocumented member.
newListWorkerFleets ::
  -- | 'site'
  Prelude.Text ->
  ListWorkerFleets
newListWorkerFleets pSite_ =
  ListWorkerFleets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      site = pSite_
    }

-- | Undocumented member.
listWorkerFleets_maxResults :: Lens.Lens' ListWorkerFleets (Prelude.Maybe Prelude.Natural)
listWorkerFleets_maxResults = Lens.lens (\ListWorkerFleets' {maxResults} -> maxResults) (\s@ListWorkerFleets' {} a -> s {maxResults = a} :: ListWorkerFleets)

-- | Undocumented member.
listWorkerFleets_nextToken :: Lens.Lens' ListWorkerFleets (Prelude.Maybe Prelude.Text)
listWorkerFleets_nextToken = Lens.lens (\ListWorkerFleets' {nextToken} -> nextToken) (\s@ListWorkerFleets' {} a -> s {nextToken = a} :: ListWorkerFleets)

-- | Undocumented member.
listWorkerFleets_site :: Lens.Lens' ListWorkerFleets Prelude.Text
listWorkerFleets_site = Lens.lens (\ListWorkerFleets' {site} -> site) (\s@ListWorkerFleets' {} a -> s {site = a} :: ListWorkerFleets)

instance Core.AWSPager ListWorkerFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkerFleetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorkerFleetsResponse_workerFleets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkerFleets_nextToken
          Lens..~ rs
          Lens.^? listWorkerFleetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListWorkerFleets where
  type
    AWSResponse ListWorkerFleets =
      ListWorkerFleetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkerFleetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "workerFleets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorkerFleets where
  hashWithSalt _salt ListWorkerFleets' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` site

instance Prelude.NFData ListWorkerFleets where
  rnf ListWorkerFleets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf site

instance Data.ToHeaders ListWorkerFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkerFleets where
  toPath = Prelude.const "/listWorkerFleets"

instance Data.ToQuery ListWorkerFleets where
  toQuery ListWorkerFleets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "site" Data.=: site
      ]

-- | /See:/ 'newListWorkerFleetsResponse' smart constructor.
data ListWorkerFleetsResponse = ListWorkerFleetsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    workerFleets :: Prelude.Maybe [WorkerFleet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkerFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkerFleetsResponse_nextToken' - Undocumented member.
--
-- 'workerFleets', 'listWorkerFleetsResponse_workerFleets' - Undocumented member.
--
-- 'httpStatus', 'listWorkerFleetsResponse_httpStatus' - The response's http status code.
newListWorkerFleetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkerFleetsResponse
newListWorkerFleetsResponse pHttpStatus_ =
  ListWorkerFleetsResponse'
    { nextToken =
        Prelude.Nothing,
      workerFleets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listWorkerFleetsResponse_nextToken :: Lens.Lens' ListWorkerFleetsResponse (Prelude.Maybe Prelude.Text)
listWorkerFleetsResponse_nextToken = Lens.lens (\ListWorkerFleetsResponse' {nextToken} -> nextToken) (\s@ListWorkerFleetsResponse' {} a -> s {nextToken = a} :: ListWorkerFleetsResponse)

-- | Undocumented member.
listWorkerFleetsResponse_workerFleets :: Lens.Lens' ListWorkerFleetsResponse (Prelude.Maybe [WorkerFleet])
listWorkerFleetsResponse_workerFleets = Lens.lens (\ListWorkerFleetsResponse' {workerFleets} -> workerFleets) (\s@ListWorkerFleetsResponse' {} a -> s {workerFleets = a} :: ListWorkerFleetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorkerFleetsResponse_httpStatus :: Lens.Lens' ListWorkerFleetsResponse Prelude.Int
listWorkerFleetsResponse_httpStatus = Lens.lens (\ListWorkerFleetsResponse' {httpStatus} -> httpStatus) (\s@ListWorkerFleetsResponse' {} a -> s {httpStatus = a} :: ListWorkerFleetsResponse)

instance Prelude.NFData ListWorkerFleetsResponse where
  rnf ListWorkerFleetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workerFleets
      `Prelude.seq` Prelude.rnf httpStatus
