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
-- Module      : Amazonka.IoTSiteWise.ListPortals
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of IoT SiteWise Monitor portals.
--
-- This operation returns paginated results.
module Amazonka.IoTSiteWise.ListPortals
  ( -- * Creating a Request
    ListPortals (..),
    newListPortals,

    -- * Request Lenses
    listPortals_nextToken,
    listPortals_maxResults,

    -- * Destructuring the Response
    ListPortalsResponse (..),
    newListPortalsResponse,

    -- * Response Lenses
    listPortalsResponse_nextToken,
    listPortalsResponse_portalSummaries,
    listPortalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPortals' smart constructor.
data ListPortals = ListPortals'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for each paginated request.
    --
    -- Default: 50
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPortals_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'listPortals_maxResults' - The maximum number of results to return for each paginated request.
--
-- Default: 50
newListPortals ::
  ListPortals
newListPortals =
  ListPortals'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results.
listPortals_nextToken :: Lens.Lens' ListPortals (Prelude.Maybe Prelude.Text)
listPortals_nextToken = Lens.lens (\ListPortals' {nextToken} -> nextToken) (\s@ListPortals' {} a -> s {nextToken = a} :: ListPortals)

-- | The maximum number of results to return for each paginated request.
--
-- Default: 50
listPortals_maxResults :: Lens.Lens' ListPortals (Prelude.Maybe Prelude.Natural)
listPortals_maxResults = Lens.lens (\ListPortals' {maxResults} -> maxResults) (\s@ListPortals' {} a -> s {maxResults = a} :: ListPortals)

instance Core.AWSPager ListPortals where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPortalsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPortalsResponse_portalSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPortals_nextToken
          Lens..~ rs
          Lens.^? listPortalsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPortals where
  type AWSResponse ListPortals = ListPortalsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPortalsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "portalSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPortals where
  hashWithSalt _salt ListPortals' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListPortals where
  rnf ListPortals' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListPortals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListPortals where
  toPath = Prelude.const "/portals"

instance Data.ToQuery ListPortals where
  toQuery ListPortals' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListPortalsResponse' smart constructor.
data ListPortalsResponse = ListPortalsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each portal.
    portalSummaries :: Prelude.Maybe [PortalSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPortalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPortalsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'portalSummaries', 'listPortalsResponse_portalSummaries' - A list that summarizes each portal.
--
-- 'httpStatus', 'listPortalsResponse_httpStatus' - The response's http status code.
newListPortalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPortalsResponse
newListPortalsResponse pHttpStatus_ =
  ListPortalsResponse'
    { nextToken = Prelude.Nothing,
      portalSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listPortalsResponse_nextToken :: Lens.Lens' ListPortalsResponse (Prelude.Maybe Prelude.Text)
listPortalsResponse_nextToken = Lens.lens (\ListPortalsResponse' {nextToken} -> nextToken) (\s@ListPortalsResponse' {} a -> s {nextToken = a} :: ListPortalsResponse)

-- | A list that summarizes each portal.
listPortalsResponse_portalSummaries :: Lens.Lens' ListPortalsResponse (Prelude.Maybe [PortalSummary])
listPortalsResponse_portalSummaries = Lens.lens (\ListPortalsResponse' {portalSummaries} -> portalSummaries) (\s@ListPortalsResponse' {} a -> s {portalSummaries = a} :: ListPortalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPortalsResponse_httpStatus :: Lens.Lens' ListPortalsResponse Prelude.Int
listPortalsResponse_httpStatus = Lens.lens (\ListPortalsResponse' {httpStatus} -> httpStatus) (\s@ListPortalsResponse' {} a -> s {httpStatus = a} :: ListPortalsResponse)

instance Prelude.NFData ListPortalsResponse where
  rnf ListPortalsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf portalSummaries
      `Prelude.seq` Prelude.rnf httpStatus
