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
-- Module      : Network.AWS.Greengrass.ListLoggerDefinitionVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a logger definition.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListLoggerDefinitionVersions
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

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLoggerDefinitionVersions' smart constructor.
data ListLoggerDefinitionVersions = ListLoggerDefinitionVersions'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListLoggerDefinitionVersions
newListLoggerDefinitionVersions pLoggerDefinitionId_ =
  ListLoggerDefinitionVersions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      loggerDefinitionId = pLoggerDefinitionId_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listLoggerDefinitionVersions_nextToken :: Lens.Lens' ListLoggerDefinitionVersions (Core.Maybe Core.Text)
listLoggerDefinitionVersions_nextToken = Lens.lens (\ListLoggerDefinitionVersions' {nextToken} -> nextToken) (\s@ListLoggerDefinitionVersions' {} a -> s {nextToken = a} :: ListLoggerDefinitionVersions)

-- | The maximum number of results to be returned per request.
listLoggerDefinitionVersions_maxResults :: Lens.Lens' ListLoggerDefinitionVersions (Core.Maybe Core.Text)
listLoggerDefinitionVersions_maxResults = Lens.lens (\ListLoggerDefinitionVersions' {maxResults} -> maxResults) (\s@ListLoggerDefinitionVersions' {} a -> s {maxResults = a} :: ListLoggerDefinitionVersions)

-- | The ID of the logger definition.
listLoggerDefinitionVersions_loggerDefinitionId :: Lens.Lens' ListLoggerDefinitionVersions Core.Text
listLoggerDefinitionVersions_loggerDefinitionId = Lens.lens (\ListLoggerDefinitionVersions' {loggerDefinitionId} -> loggerDefinitionId) (\s@ListLoggerDefinitionVersions' {} a -> s {loggerDefinitionId = a} :: ListLoggerDefinitionVersions)

instance Core.AWSPager ListLoggerDefinitionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLoggerDefinitionVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listLoggerDefinitionVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listLoggerDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listLoggerDefinitionVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListLoggerDefinitionVersions where
  type
    AWSResponse ListLoggerDefinitionVersions =
      ListLoggerDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggerDefinitionVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListLoggerDefinitionVersions

instance Core.NFData ListLoggerDefinitionVersions

instance Core.ToHeaders ListLoggerDefinitionVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListLoggerDefinitionVersions where
  toPath ListLoggerDefinitionVersions' {..} =
    Core.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId,
        "/versions"
      ]

instance Core.ToQuery ListLoggerDefinitionVersions where
  toQuery ListLoggerDefinitionVersions' {..} =
    Core.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListLoggerDefinitionVersionsResponse' smart constructor.
data ListLoggerDefinitionVersionsResponse = ListLoggerDefinitionVersionsResponse'
  { -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about a version.
    versions :: Core.Maybe [VersionInformation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListLoggerDefinitionVersionsResponse
newListLoggerDefinitionVersionsResponse pHttpStatus_ =
  ListLoggerDefinitionVersionsResponse'
    { nextToken =
        Core.Nothing,
      versions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listLoggerDefinitionVersionsResponse_nextToken :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Core.Maybe Core.Text)
listLoggerDefinitionVersionsResponse_nextToken = Lens.lens (\ListLoggerDefinitionVersionsResponse' {nextToken} -> nextToken) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {nextToken = a} :: ListLoggerDefinitionVersionsResponse)

-- | Information about a version.
listLoggerDefinitionVersionsResponse_versions :: Lens.Lens' ListLoggerDefinitionVersionsResponse (Core.Maybe [VersionInformation])
listLoggerDefinitionVersionsResponse_versions = Lens.lens (\ListLoggerDefinitionVersionsResponse' {versions} -> versions) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {versions = a} :: ListLoggerDefinitionVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLoggerDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListLoggerDefinitionVersionsResponse Core.Int
listLoggerDefinitionVersionsResponse_httpStatus = Lens.lens (\ListLoggerDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListLoggerDefinitionVersionsResponse)

instance
  Core.NFData
    ListLoggerDefinitionVersionsResponse
