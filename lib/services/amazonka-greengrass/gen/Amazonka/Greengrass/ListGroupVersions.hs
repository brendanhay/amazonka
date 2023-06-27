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
-- Module      : Amazonka.Greengrass.ListGroupVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a group.
--
-- This operation returns paginated results.
module Amazonka.Greengrass.ListGroupVersions
  ( -- * Creating a Request
    ListGroupVersions (..),
    newListGroupVersions,

    -- * Request Lenses
    listGroupVersions_maxResults,
    listGroupVersions_nextToken,
    listGroupVersions_groupId,

    -- * Destructuring the Response
    ListGroupVersionsResponse (..),
    newListGroupVersionsResponse,

    -- * Response Lenses
    listGroupVersionsResponse_nextToken,
    listGroupVersionsResponse_versions,
    listGroupVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGroupVersions' smart constructor.
data ListGroupVersions = ListGroupVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or \'\'null\'\' if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listGroupVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listGroupVersions_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'groupId', 'listGroupVersions_groupId' - The ID of the Greengrass group.
newListGroupVersions ::
  -- | 'groupId'
  Prelude.Text ->
  ListGroupVersions
newListGroupVersions pGroupId_ =
  ListGroupVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | The maximum number of results to be returned per request.
listGroupVersions_maxResults :: Lens.Lens' ListGroupVersions (Prelude.Maybe Prelude.Text)
listGroupVersions_maxResults = Lens.lens (\ListGroupVersions' {maxResults} -> maxResults) (\s@ListGroupVersions' {} a -> s {maxResults = a} :: ListGroupVersions)

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listGroupVersions_nextToken :: Lens.Lens' ListGroupVersions (Prelude.Maybe Prelude.Text)
listGroupVersions_nextToken = Lens.lens (\ListGroupVersions' {nextToken} -> nextToken) (\s@ListGroupVersions' {} a -> s {nextToken = a} :: ListGroupVersions)

-- | The ID of the Greengrass group.
listGroupVersions_groupId :: Lens.Lens' ListGroupVersions Prelude.Text
listGroupVersions_groupId = Lens.lens (\ListGroupVersions' {groupId} -> groupId) (\s@ListGroupVersions' {} a -> s {groupId = a} :: ListGroupVersions)

instance Core.AWSPager ListGroupVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGroupVersionsResponse_versions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listGroupVersions_nextToken
          Lens..~ rs
          Lens.^? listGroupVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListGroupVersions where
  type
    AWSResponse ListGroupVersions =
      ListGroupVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroupVersions where
  hashWithSalt _salt ListGroupVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData ListGroupVersions where
  rnf ListGroupVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders ListGroupVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGroupVersions where
  toPath ListGroupVersions' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/versions"
      ]

instance Data.ToQuery ListGroupVersions where
  toQuery ListGroupVersions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListGroupVersionsResponse' smart constructor.
data ListGroupVersionsResponse = ListGroupVersionsResponse'
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
-- Create a value of 'ListGroupVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupVersionsResponse_nextToken' - The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
--
-- 'versions', 'listGroupVersionsResponse_versions' - Information about a version.
--
-- 'httpStatus', 'listGroupVersionsResponse_httpStatus' - The response's http status code.
newListGroupVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupVersionsResponse
newListGroupVersionsResponse pHttpStatus_ =
  ListGroupVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or \'\'null\'\' if there are no
-- additional results.
listGroupVersionsResponse_nextToken :: Lens.Lens' ListGroupVersionsResponse (Prelude.Maybe Prelude.Text)
listGroupVersionsResponse_nextToken = Lens.lens (\ListGroupVersionsResponse' {nextToken} -> nextToken) (\s@ListGroupVersionsResponse' {} a -> s {nextToken = a} :: ListGroupVersionsResponse)

-- | Information about a version.
listGroupVersionsResponse_versions :: Lens.Lens' ListGroupVersionsResponse (Prelude.Maybe [VersionInformation])
listGroupVersionsResponse_versions = Lens.lens (\ListGroupVersionsResponse' {versions} -> versions) (\s@ListGroupVersionsResponse' {} a -> s {versions = a} :: ListGroupVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGroupVersionsResponse_httpStatus :: Lens.Lens' ListGroupVersionsResponse Prelude.Int
listGroupVersionsResponse_httpStatus = Lens.lens (\ListGroupVersionsResponse' {httpStatus} -> httpStatus) (\s@ListGroupVersionsResponse' {} a -> s {httpStatus = a} :: ListGroupVersionsResponse)

instance Prelude.NFData ListGroupVersionsResponse where
  rnf ListGroupVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
