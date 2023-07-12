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
-- Module      : Amazonka.SMS.ListApps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves summaries for all applications.
--
-- This operation returns paginated results.
module Amazonka.SMS.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_appIds,
    listApps_maxResults,
    listApps_nextToken,

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
import Amazonka.SMS.Types

-- | /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | The unique application IDs.
    appIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return in a single call. The default
    -- value is 100. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'appIds', 'listApps_appIds' - The unique application IDs.
--
-- 'maxResults', 'listApps_maxResults' - The maximum number of results to return in a single call. The default
-- value is 100. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
--
-- 'nextToken', 'listApps_nextToken' - The token for the next set of results.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { appIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The unique application IDs.
listApps_appIds :: Lens.Lens' ListApps (Prelude.Maybe [Prelude.Text])
listApps_appIds = Lens.lens (\ListApps' {appIds} -> appIds) (\s@ListApps' {} a -> s {appIds = a} :: ListApps) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. The default
-- value is 100. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
listApps_maxResults :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Int)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

-- | The token for the next set of results.
listApps_nextToken :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

instance Core.AWSPager ListApps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_apps
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApps_nextToken
          Lens..~ rs
          Lens.^? listAppsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Prelude.<$> (x Data..?> "apps" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApps where
  hashWithSalt _salt ListApps' {..} =
    _salt
      `Prelude.hashWithSalt` appIds
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListApps where
  rnf ListApps' {..} =
    Prelude.rnf appIds
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.ListApps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListApps where
  toJSON ListApps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appIds" Data..=) Prelude.<$> appIds,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListApps where
  toPath = Prelude.const "/"

instance Data.ToQuery ListApps where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | The application summaries.
    apps :: Prelude.Maybe [AppSummary],
    -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
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
-- 'apps', 'listAppsResponse_apps' - The application summaries.
--
-- 'nextToken', 'listAppsResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
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

-- | The application summaries.
listAppsResponse_apps :: Lens.Lens' ListAppsResponse (Prelude.Maybe [AppSummary])
listAppsResponse_apps = Lens.lens (\ListAppsResponse' {apps} -> apps) (\s@ListAppsResponse' {} a -> s {apps = a} :: ListAppsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
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
