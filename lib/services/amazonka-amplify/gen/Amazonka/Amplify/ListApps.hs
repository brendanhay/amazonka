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
-- Module      : Amazonka.Amplify.ListApps
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the existing Amplify apps.
--
-- This operation returns paginated results.
module Amazonka.Amplify.ListApps
  ( -- * Creating a Request
    ListApps (..),
    newListApps,

    -- * Request Lenses
    listApps_nextToken,
    listApps_maxResults,

    -- * Destructuring the Response
    ListAppsResponse (..),
    newListAppsResponse,

    -- * Response Lenses
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
    listAppsResponse_apps,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the list apps request.
--
-- /See:/ 'newListApps' smart constructor.
data ListApps = ListApps'
  { -- | A pagination token. If non-null, the pagination token is returned in a
    -- result. Pass its value in another request to retrieve more entries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to list in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listApps_nextToken' - A pagination token. If non-null, the pagination token is returned in a
-- result. Pass its value in another request to retrieve more entries.
--
-- 'maxResults', 'listApps_maxResults' - The maximum number of records to list in a single response.
newListApps ::
  ListApps
newListApps =
  ListApps'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token. If non-null, the pagination token is returned in a
-- result. Pass its value in another request to retrieve more entries.
listApps_nextToken :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Text)
listApps_nextToken = Lens.lens (\ListApps' {nextToken} -> nextToken) (\s@ListApps' {} a -> s {nextToken = a} :: ListApps)

-- | The maximum number of records to list in a single response.
listApps_maxResults :: Lens.Lens' ListApps (Prelude.Maybe Prelude.Natural)
listApps_maxResults = Lens.lens (\ListApps' {maxResults} -> maxResults) (\s@ListApps' {} a -> s {maxResults = a} :: ListApps)

instance Core.AWSPager ListApps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listAppsResponse_apps) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listApps_nextToken
          Lens..~ rs
          Lens.^? listAppsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListApps where
  type AWSResponse ListApps = ListAppsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "apps" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListApps where
  hashWithSalt _salt ListApps' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListApps where
  rnf ListApps' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListApps where
  toPath = Prelude.const "/apps"

instance Core.ToQuery ListApps where
  toQuery ListApps' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | The result structure for an Amplify app list request.
--
-- /See:/ 'newListAppsResponse' smart constructor.
data ListAppsResponse = ListAppsResponse'
  { -- | A pagination token. Set to null to start listing apps from start. If
    -- non-null, the pagination token is returned in a result. Pass its value
    -- in here to list more projects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of Amplify apps.
    apps :: [App]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppsResponse_nextToken' - A pagination token. Set to null to start listing apps from start. If
-- non-null, the pagination token is returned in a result. Pass its value
-- in here to list more projects.
--
-- 'httpStatus', 'listAppsResponse_httpStatus' - The response's http status code.
--
-- 'apps', 'listAppsResponse_apps' - A list of Amplify apps.
newListAppsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppsResponse
newListAppsResponse pHttpStatus_ =
  ListAppsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      apps = Prelude.mempty
    }

-- | A pagination token. Set to null to start listing apps from start. If
-- non-null, the pagination token is returned in a result. Pass its value
-- in here to list more projects.
listAppsResponse_nextToken :: Lens.Lens' ListAppsResponse (Prelude.Maybe Prelude.Text)
listAppsResponse_nextToken = Lens.lens (\ListAppsResponse' {nextToken} -> nextToken) (\s@ListAppsResponse' {} a -> s {nextToken = a} :: ListAppsResponse)

-- | The response's http status code.
listAppsResponse_httpStatus :: Lens.Lens' ListAppsResponse Prelude.Int
listAppsResponse_httpStatus = Lens.lens (\ListAppsResponse' {httpStatus} -> httpStatus) (\s@ListAppsResponse' {} a -> s {httpStatus = a} :: ListAppsResponse)

-- | A list of Amplify apps.
listAppsResponse_apps :: Lens.Lens' ListAppsResponse [App]
listAppsResponse_apps = Lens.lens (\ListAppsResponse' {apps} -> apps) (\s@ListAppsResponse' {} a -> s {apps = a} :: ListAppsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAppsResponse where
  rnf ListAppsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf apps
