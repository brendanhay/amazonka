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
-- Module      : Amazonka.SimSpaceWeaver.ListApps
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all custom apps or service apps for the given simulation and
-- domain.
module Amazonka.SimSpaceWeaver.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_domain,
    listApps_maxResults,
    listApps_nextToken,
    listApps_simulation,

    -- * Destructuring the Response
    ListAppsResponse (..),
    newListAppsResponse,

    -- * Response Lenses
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SimSpaceWeaver.Types

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | The name of the domain that you want to list apps for.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of apps to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If SimSpace Weaver returns @nextToken@, there are more results
    -- available. The value of @nextToken@ is a unique pagination token for
    -- each page. To retrieve the next page, call the operation again using the
    -- returned token. Keep all other arguments unchanged. If no results
    -- remain, @nextToken@ is set to @null@. Each pagination token expires
    -- after 24 hours. If you provide a token that isn\'t valid, you receive an
    -- /HTTP 400 ValidationException/ error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the simulation that you want to list apps for.
    simulation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'listApps_domain' - The name of the domain that you want to list apps for.
--
-- 'maxResults', 'listApps_maxResults' - The maximum number of apps to list.
--
-- 'nextToken', 'listApps_nextToken' - If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
--
-- 'simulation', 'listApps_simulation' - The name of the simulation that you want to list apps for.
newListApps ::
  -- | 'simulation'
  Prelude.Text ->
  ListApps
newListApps pSimulation_ =
  ListApps'
    { domain = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      simulation = pSimulation_
    }

-- | The name of the domain that you want to list apps for.
listApps_domain :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_domain = Lens.lens (\ListApps' {domain} -> domain) (\s@ListApps' {} a -> s {domain = a} :: ListApps)

-- | The maximum number of apps to list.
listApps_maxResults :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Natural)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

-- | If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
listApps_nextToken :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

-- | The name of the simulation that you want to list apps for.
listApps_simulation :: Lens.Lens' ListApps Prelude.Text
listApps_simulation = Lens.lens (\ListApps' {simulation} -> simulation) (\s@ListApps' {} a -> s {simulation = a} :: ListApps)

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Prelude.<$> (x Data..?> "Apps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApps where
  hashWithSalt _salt ListApps' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` simulation

instance Prelude.NFData ListApps where
  rnf ListApps' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf simulation

instance Data.ToHeaders ListApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApps where
  toPath = Prelude.const "/listapps"

instance Data.ToQuery ListApps where
  toQuery ListApps' {..} =
    Prelude.mconcat
      [ "domain" Data.=: domain,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "simulation" Data.=: simulation
      ]

-- | /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The list of apps for the given simulation and domain.
    apps :: Prelude.Maybe [SimulationAppMetadata],
    -- | If SimSpace Weaver returns @nextToken@, there are more results
    -- available. The value of @nextToken@ is a unique pagination token for
    -- each page. To retrieve the next page, call the operation again using the
    -- returned token. Keep all other arguments unchanged. If no results
    -- remain, @nextToken@ is set to @null@. Each pagination token expires
    -- after 24 hours. If you provide a token that isn\'t valid, you receive an
    -- /HTTP 400 ValidationException/ error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apps', 'listAppsResponse_apps' - The list of apps for the given simulation and domain.
--
-- 'nextToken', 'listAppsResponse_nextToken' - If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
--
-- 'httpStatus', 'listAppsResponse_httpStatus' - The response's http status code.
newListAppsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppsResponse
newListAppsResponse pHttpStatus_ =
  ListAppsResponse'
    { apps = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of apps for the given simulation and domain.
listAppsResponse_apps :: Lens.Lens' ListAppsResponse (Prelude.Maybe [SimulationAppMetadata])
listAppsResponse_apps = Lens.lens (\ListAppsResponse' {apps} -> apps) (\s@ListAppsResponse' {} a -> s {apps = a} :: ListAppsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If SimSpace Weaver returns @nextToken@, there are more results
-- available. The value of @nextToken@ is a unique pagination token for
-- each page. To retrieve the next page, call the operation again using the
-- returned token. Keep all other arguments unchanged. If no results
-- remain, @nextToken@ is set to @null@. Each pagination token expires
-- after 24 hours. If you provide a token that isn\'t valid, you receive an
-- /HTTP 400 ValidationException/ error.
listAppsResponse_nextToken :: Lens.Lens' ListAppsResponse (Prelude.Maybe Prelude.Text)
listAppsResponse_nextToken = Lens.lens (\ListAppsResponse' {nextToken} -> nextToken) (\s@ListAppsResponse' {} a -> s {nextToken = a} :: ListAppsResponse)

-- | The response's http status code.
listAppsResponse_httpStatus :: Lens.Lens' ListAppsResponse Prelude.Int
listAppsResponse_httpStatus = Lens.lens (\ListAppsResponse' {httpStatus} -> httpStatus) (\s@ListAppsResponse' {} a -> s {httpStatus = a} :: ListAppsResponse)

instance Prelude.NFData ListAppsResponse where
  rnf ListAppsResponse' {..} =
    Prelude.rnf apps
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
