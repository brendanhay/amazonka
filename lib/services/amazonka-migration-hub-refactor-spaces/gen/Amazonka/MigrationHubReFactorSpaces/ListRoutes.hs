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
-- Module      : Amazonka.MigrationHubReFactorSpaces.ListRoutes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the Amazon Web Services Migration Hub Refactor Spaces routes
-- within an application.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubReFactorSpaces.ListRoutes
  ( -- * Creating a Request
    ListRoutes (..),
    newListRoutes,

    -- * Request Lenses
    listRoutes_nextToken,
    listRoutes_maxResults,
    listRoutes_applicationIdentifier,
    listRoutes_environmentIdentifier,

    -- * Destructuring the Response
    ListRoutesResponse (..),
    newListRoutesResponse,

    -- * Response Lenses
    listRoutesResponse_nextToken,
    listRoutesResponse_routeSummaryList,
    listRoutesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRoutes' smart constructor.
data ListRoutes = ListRoutes'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the application.
    applicationIdentifier :: Prelude.Text,
    -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutes_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listRoutes_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'applicationIdentifier', 'listRoutes_applicationIdentifier' - The ID of the application.
--
-- 'environmentIdentifier', 'listRoutes_environmentIdentifier' - The ID of the environment.
newListRoutes ::
  -- | 'applicationIdentifier'
  Prelude.Text ->
  -- | 'environmentIdentifier'
  Prelude.Text ->
  ListRoutes
newListRoutes
  pApplicationIdentifier_
  pEnvironmentIdentifier_ =
    ListRoutes'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        applicationIdentifier = pApplicationIdentifier_,
        environmentIdentifier = pEnvironmentIdentifier_
      }

-- | The token for the next page of results.
listRoutes_nextToken :: Lens.Lens' ListRoutes (Prelude.Maybe Prelude.Text)
listRoutes_nextToken = Lens.lens (\ListRoutes' {nextToken} -> nextToken) (\s@ListRoutes' {} a -> s {nextToken = a} :: ListRoutes)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
listRoutes_maxResults :: Lens.Lens' ListRoutes (Prelude.Maybe Prelude.Natural)
listRoutes_maxResults = Lens.lens (\ListRoutes' {maxResults} -> maxResults) (\s@ListRoutes' {} a -> s {maxResults = a} :: ListRoutes)

-- | The ID of the application.
listRoutes_applicationIdentifier :: Lens.Lens' ListRoutes Prelude.Text
listRoutes_applicationIdentifier = Lens.lens (\ListRoutes' {applicationIdentifier} -> applicationIdentifier) (\s@ListRoutes' {} a -> s {applicationIdentifier = a} :: ListRoutes)

-- | The ID of the environment.
listRoutes_environmentIdentifier :: Lens.Lens' ListRoutes Prelude.Text
listRoutes_environmentIdentifier = Lens.lens (\ListRoutes' {environmentIdentifier} -> environmentIdentifier) (\s@ListRoutes' {} a -> s {environmentIdentifier = a} :: ListRoutes)

instance Core.AWSPager ListRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoutesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRoutesResponse_routeSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRoutes_nextToken
          Lens..~ rs
          Lens.^? listRoutesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListRoutes where
  type AWSResponse ListRoutes = ListRoutesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoutesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "RouteSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoutes where
  hashWithSalt _salt ListRoutes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` applicationIdentifier
      `Prelude.hashWithSalt` environmentIdentifier

instance Prelude.NFData ListRoutes where
  rnf ListRoutes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf applicationIdentifier
      `Prelude.seq` Prelude.rnf environmentIdentifier

instance Data.ToHeaders ListRoutes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRoutes where
  toPath ListRoutes' {..} =
    Prelude.mconcat
      [ "/environments/",
        Data.toBS environmentIdentifier,
        "/applications/",
        Data.toBS applicationIdentifier,
        "/routes"
      ]

instance Data.ToQuery ListRoutes where
  toQuery ListRoutes' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListRoutesResponse' smart constructor.
data ListRoutesResponse = ListRoutesResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of @RouteSummary@ objects.
    routeSummaryList :: Prelude.Maybe [RouteSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRoutesResponse_nextToken' - The token for the next page of results.
--
-- 'routeSummaryList', 'listRoutesResponse_routeSummaryList' - The list of @RouteSummary@ objects.
--
-- 'httpStatus', 'listRoutesResponse_httpStatus' - The response's http status code.
newListRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoutesResponse
newListRoutesResponse pHttpStatus_ =
  ListRoutesResponse'
    { nextToken = Prelude.Nothing,
      routeSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listRoutesResponse_nextToken :: Lens.Lens' ListRoutesResponse (Prelude.Maybe Prelude.Text)
listRoutesResponse_nextToken = Lens.lens (\ListRoutesResponse' {nextToken} -> nextToken) (\s@ListRoutesResponse' {} a -> s {nextToken = a} :: ListRoutesResponse)

-- | The list of @RouteSummary@ objects.
listRoutesResponse_routeSummaryList :: Lens.Lens' ListRoutesResponse (Prelude.Maybe [RouteSummary])
listRoutesResponse_routeSummaryList = Lens.lens (\ListRoutesResponse' {routeSummaryList} -> routeSummaryList) (\s@ListRoutesResponse' {} a -> s {routeSummaryList = a} :: ListRoutesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRoutesResponse_httpStatus :: Lens.Lens' ListRoutesResponse Prelude.Int
listRoutesResponse_httpStatus = Lens.lens (\ListRoutesResponse' {httpStatus} -> httpStatus) (\s@ListRoutesResponse' {} a -> s {httpStatus = a} :: ListRoutesResponse)

instance Prelude.NFData ListRoutesResponse where
  rnf ListRoutesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf routeSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
