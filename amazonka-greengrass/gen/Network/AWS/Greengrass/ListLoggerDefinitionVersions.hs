{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager ListLoggerDefinitionVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listLoggerDefinitionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listLoggerDefinitionVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listLoggerDefinitionVersions_nextToken
          Lens..~ rs
          Lens.^? listLoggerDefinitionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    ListLoggerDefinitionVersions
  where
  type
    Rs ListLoggerDefinitionVersions =
      ListLoggerDefinitionVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggerDefinitionVersionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (x Prelude..?> "Versions" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListLoggerDefinitionVersions

instance Prelude.NFData ListLoggerDefinitionVersions

instance
  Prelude.ToHeaders
    ListLoggerDefinitionVersions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListLoggerDefinitionVersions where
  toPath ListLoggerDefinitionVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Prelude.toBS loggerDefinitionId,
        "/versions"
      ]

instance Prelude.ToQuery ListLoggerDefinitionVersions where
  toQuery ListLoggerDefinitionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Prelude.=: nextToken,
        "MaxResults" Prelude.=: maxResults
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listLoggerDefinitionVersionsResponse_versions = Lens.lens (\ListLoggerDefinitionVersionsResponse' {versions} -> versions) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {versions = a} :: ListLoggerDefinitionVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listLoggerDefinitionVersionsResponse_httpStatus :: Lens.Lens' ListLoggerDefinitionVersionsResponse Prelude.Int
listLoggerDefinitionVersionsResponse_httpStatus = Lens.lens (\ListLoggerDefinitionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListLoggerDefinitionVersionsResponse' {} a -> s {httpStatus = a} :: ListLoggerDefinitionVersionsResponse)

instance
  Prelude.NFData
    ListLoggerDefinitionVersionsResponse
