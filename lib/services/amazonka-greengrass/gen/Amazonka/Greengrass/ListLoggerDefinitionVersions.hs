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
-- Module      : Amazonka.Greengrass.ListLoggerDefinitionVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a logger definition.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListLoggerDefinitionVersions
  ( -- * Creating a Request
    ListLoggerDefinitionVersions (..),
    newListLoggerDefinitionVersions,

    -- * Request Lenses
    listLoggerDefinitionVersions_nextToken,
    listLoggerDefinitionVersions_maxResults,
    listLoggerDefinitionVersions_loggerDefinitionId,

    -- * Destructuring the Response
    ListLoggerDefinitionVersionsResponse (..),
    newListLoggerDefinitionVersionsResponse,

    -- * Response Lenses
    listLoggerDefinitionVersionsResponse_nextToken,
    listLoggerDefinitionVersionsResponse_versions,
    listLoggerDefinitionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLoggerDefinitionVersions' smart constructor.
data ListLoggerDefinitionVersions = ListLoggerDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggerDefinitionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLoggerDefinitionVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'maxResults', 'listLoggerDefinitionVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'loggerDefinitionId', 'listLoggerDefinitionVersions_loggerDefinitionId' - The ID of the logger definition.
newListLoggerDefinitionVersions ::
  -- | 'loggerDefinitionId'
  Prelude.Text ->
  ListLoggerDefinitionVersions
newListLoggerDefinitionVersions pLoggerDefinitionId_ =
  ListLoggerDefinitionVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      loggerDefinitionId = pLoggerDefinitionId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listLoggerDefinitionVersions_nextToken :: Lens.Lens' ListLoggerDefinitionVersions (Prelude.Maybe Prelude.Text)
listLoggerDefinitionVersions_nextToken = Lens.lens (\ListLoggerDefinitionVersions' {nextToken} -> nextToken) (\s@ListLoggerDefinitionVersions' {} a -> s {nextToken = a} :: ListLoggerDefinitionVersions)

-- | The maximum number of results to be returned per request.
listLoggerDefinitionVersions_maxResults :: Lens.Lens' ListLoggerDefinitionVersions (Prelude.Maybe Prelude.Text)
listLoggerDefinitionVersions_maxResults = Lens.lens (\ListLoggerDefinitionVersions' {maxResults} -> maxResults) (\s@ListLoggerDefinitionVersions' {} a -> s {maxResults = a} :: ListLoggerDefinitionVersions)

-- | The ID of the logger definition.
listLoggerDefinitionVersions_loggerDefinitionId :: Lens.Lens' ListLoggerDefinitionVersions Prelude.Text
listLoggerDefinitionVersions_loggerDefinitionId = Lens.lens (\ListLoggerDefinitionVersions' {loggerDefinitionId} -> loggerDefinitionId) (\s@ListLoggerDefinitionVersions' {} a -> s {loggerDefinitionId = a} :: ListLoggerDefinitionVersions)

instance Core.AWSPager ListLoggerDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLoggerDefinitionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLoggerDefinitionVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLoggerDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listLoggerDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLoggerDefinitionVersions where
  type
    AWSResponse ListLoggerDefinitionVersions =
      ListLoggerDefinitionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggerDefinitionVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListLoggerDefinitionVersions
  where
  hashWithSalt _salt ListLoggerDefinitionVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` loggerDefinitionId

instance Prelude.NFData ListLoggerDefinitionVersions where
  rnf ListLoggerDefinitionVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf loggerDefinitionId

instance Core.ToHeaders ListLoggerDefinitionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLoggerDefinitionVersions where
  toPath ListLoggerDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListLoggerDefinitionVersions where
  toQuery ListLoggerDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLoggerDefinitionVersionsResponse' smart constructor.
data ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about a version.
    versions :: Prelude.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggerDefinitionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLoggerDefinitionVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listLoggerDefinitionVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listLoggerDefinitionVersionsResponse_httpStatus' - The response's http status code.
newListLoggerDefinitionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLoggerDefinitionVersionsResponse
newListLoggerDefinitionVersionsResponse pHttpStatus_ =
  ListLoggerDefinitionVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listLoggerDefinitionVersionsResponse_nextToken :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Prelude.Maybe Prelude.Text)
listLoggerDefinitionVersionsResponse_nextToken = Lens.lens (\ListLoggerDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListLoggerDefinitionVersionsResponse)

-- | Information about a version.
listLoggerDefinitionVersionsResponse_versions :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Prelude.Maybe [VersionInformation])
listLoggerDefinitionVersionsResponse_versions = Lens.lens (\ListLoggerDefinitionVersionsResponse' {versions} -> versions) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {versions = a} :: ListLoggerDefinitionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLoggerDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListLoggerDefinitionVersionsResponse Prelude.Int
listLoggerDefinitionVersionsResponse_httpStatus = Lens.lens (\ListLoggerDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListLoggerDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListLoggerDefinitionVersionsResponse
  where
  rnf ListLoggerDefinitionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
