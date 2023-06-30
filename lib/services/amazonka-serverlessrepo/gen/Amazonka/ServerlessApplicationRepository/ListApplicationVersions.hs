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
-- Module      : Amazonka.ServerlessApplicationRepository.ListApplicationVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions for the specified application.
--
-- This operation returns paginated results.
module Amazonka.ServerlessApplicationRepository.ListApplicationVersions
  ( -- * Creating a Request
    ListApplicationVersions (..),
    newListApplicationVersions,

    -- * Request Lenses
    listApplicationVersions_maxItems,
    listApplicationVersions_nextToken,
    listApplicationVersions_applicationId,

    -- * Destructuring the Response
    ListApplicationVersionsResponse (..),
    newListApplicationVersionsResponse,

    -- * Response Lenses
    listApplicationVersionsResponse_nextToken,
    listApplicationVersionsResponse_versions,
    listApplicationVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newListApplicationVersions' smart constructor.
data ListApplicationVersions = ListApplicationVersions'
  { -- | The total number of items to return.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listApplicationVersions_maxItems' - The total number of items to return.
--
-- 'nextToken', 'listApplicationVersions_nextToken' - A token to specify where to start paginating.
--
-- 'applicationId', 'listApplicationVersions_applicationId' - The Amazon Resource Name (ARN) of the application.
newListApplicationVersions ::
  -- | 'applicationId'
  Prelude.Text ->
  ListApplicationVersions
newListApplicationVersions pApplicationId_ =
  ListApplicationVersions'
    { maxItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The total number of items to return.
listApplicationVersions_maxItems :: Lens.Lens' ListApplicationVersions (Prelude.Maybe Prelude.Natural)
listApplicationVersions_maxItems = Lens.lens (\ListApplicationVersions' {maxItems} -> maxItems) (\s@ListApplicationVersions' {} a -> s {maxItems = a} :: ListApplicationVersions)

-- | A token to specify where to start paginating.
listApplicationVersions_nextToken :: Lens.Lens' ListApplicationVersions (Prelude.Maybe Prelude.Text)
listApplicationVersions_nextToken = Lens.lens (\ListApplicationVersions' {nextToken} -> nextToken) (\s@ListApplicationVersions' {} a -> s {nextToken = a} :: ListApplicationVersions)

-- | The Amazon Resource Name (ARN) of the application.
listApplicationVersions_applicationId :: Lens.Lens' ListApplicationVersions Prelude.Text
listApplicationVersions_applicationId = Lens.lens (\ListApplicationVersions' {applicationId} -> applicationId) (\s@ListApplicationVersions' {} a -> s {applicationId = a} :: ListApplicationVersions)

instance Core.AWSPager ListApplicationVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApplicationVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listApplicationVersionsResponse_versions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listApplicationVersions_nextToken
          Lens..~ rs
          Lens.^? listApplicationVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListApplicationVersions where
  type
    AWSResponse ListApplicationVersions =
      ListApplicationVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApplicationVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListApplicationVersions where
  hashWithSalt _salt ListApplicationVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData ListApplicationVersions where
  rnf ListApplicationVersions' {..} =
    Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders ListApplicationVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListApplicationVersions where
  toPath ListApplicationVersions' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/versions"
      ]

instance Data.ToQuery ListApplicationVersions where
  toQuery ListApplicationVersions' {..} =
    Prelude.mconcat
      [ "maxItems" Data.=: maxItems,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListApplicationVersionsResponse' smart constructor.
data ListApplicationVersionsResponse = ListApplicationVersionsResponse'
  { -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of version summaries for the application.
    versions :: Prelude.Maybe [VersionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListApplicationVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApplicationVersionsResponse_nextToken' - The token to request the next page of results.
--
-- 'versions', 'listApplicationVersionsResponse_versions' - An array of version summaries for the application.
--
-- 'httpStatus', 'listApplicationVersionsResponse_httpStatus' - The response's http status code.
newListApplicationVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListApplicationVersionsResponse
newListApplicationVersionsResponse pHttpStatus_ =
  ListApplicationVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to request the next page of results.
listApplicationVersionsResponse_nextToken :: Lens.Lens' ListApplicationVersionsResponse (Prelude.Maybe Prelude.Text)
listApplicationVersionsResponse_nextToken = Lens.lens (\ListApplicationVersionsResponse' {nextToken} -> nextToken) (\s@ListApplicationVersionsResponse' {} a -> s {nextToken = a} :: ListApplicationVersionsResponse)

-- | An array of version summaries for the application.
listApplicationVersionsResponse_versions :: Lens.Lens' ListApplicationVersionsResponse (Prelude.Maybe [VersionSummary])
listApplicationVersionsResponse_versions = Lens.lens (\ListApplicationVersionsResponse' {versions} -> versions) (\s@ListApplicationVersionsResponse' {} a -> s {versions = a} :: ListApplicationVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listApplicationVersionsResponse_httpStatus :: Lens.Lens' ListApplicationVersionsResponse Prelude.Int
listApplicationVersionsResponse_httpStatus = Lens.lens (\ListApplicationVersionsResponse' {httpStatus} -> httpStatus) (\s@ListApplicationVersionsResponse' {} a -> s {httpStatus = a} :: ListApplicationVersionsResponse)

instance
  Prelude.NFData
    ListApplicationVersionsResponse
  where
  rnf ListApplicationVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
