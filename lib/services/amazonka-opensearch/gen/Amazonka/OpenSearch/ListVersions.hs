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
-- Module      : Amazonka.OpenSearch.ListVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all supported versions of OpenSearch and Elasticsearch.
module Amazonka.OpenSearch.ListVersions
  ( -- * Creating a Request
    ListVersions (..),
    newListVersions,

    -- * Request Lenses
    listVersions_nextToken,
    listVersions_maxResults,

    -- * Destructuring the Response
    ListVersionsResponse (..),
    newListVersionsResponse,

    -- * Response Lenses
    listVersionsResponse_nextToken,
    listVersionsResponse_versions,
    listVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @ ListVersions @ operation.
--
-- Use @ MaxResults @ to control the maximum number of results to retrieve
-- in a single call.
--
-- Use @ NextToken @ in response to retrieve more results. If the received
-- response does not contain a NextToken, there are no more results to
-- retrieve.
--
-- /See:/ 'newListVersions' smart constructor.
data ListVersions = ListVersions'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. Value must be
    -- greater than 10 or it won\'t be honored.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVersions_nextToken' - Undocumented member.
--
-- 'maxResults', 'listVersions_maxResults' - Set this value to limit the number of results returned. Value must be
-- greater than 10 or it won\'t be honored.
newListVersions ::
  ListVersions
newListVersions =
  ListVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
listVersions_nextToken :: Lens.Lens' ListVersions (Prelude.Maybe Prelude.Text)
listVersions_nextToken = Lens.lens (\ListVersions' {nextToken} -> nextToken) (\s@ListVersions' {} a -> s {nextToken = a} :: ListVersions)

-- | Set this value to limit the number of results returned. Value must be
-- greater than 10 or it won\'t be honored.
listVersions_maxResults :: Lens.Lens' ListVersions (Prelude.Maybe Prelude.Int)
listVersions_maxResults = Lens.lens (\ListVersions' {maxResults} -> maxResults) (\s@ListVersions' {} a -> s {maxResults = a} :: ListVersions)

instance Core.AWSRequest ListVersions where
  type AWSResponse ListVersions = ListVersionsResponse
  service _ = defaultService
  request srv = Request.get srv
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVersions where
  hashWithSalt _salt ListVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListVersions where
  rnf ListVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListVersions where
  toPath =
    Prelude.const "/2021-01-01/opensearch/versions"

instance Core.ToQuery ListVersions where
  toQuery ListVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Container for the parameters for response received from the
-- @ ListVersions @ operation.
--
-- /See:/ 'newListVersionsResponse' smart constructor.
data ListVersionsResponse = ListVersionsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    versions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVersionsResponse_nextToken' - Undocumented member.
--
-- 'versions', 'listVersionsResponse_versions' - Undocumented member.
--
-- 'httpStatus', 'listVersionsResponse_httpStatus' - The response's http status code.
newListVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVersionsResponse
newListVersionsResponse pHttpStatus_ =
  ListVersionsResponse'
    { nextToken = Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listVersionsResponse_nextToken :: Lens.Lens' ListVersionsResponse (Prelude.Maybe Prelude.Text)
listVersionsResponse_nextToken = Lens.lens (\ListVersionsResponse' {nextToken} -> nextToken) (\s@ListVersionsResponse' {} a -> s {nextToken = a} :: ListVersionsResponse)

-- | Undocumented member.
listVersionsResponse_versions :: Lens.Lens' ListVersionsResponse (Prelude.Maybe [Prelude.Text])
listVersionsResponse_versions = Lens.lens (\ListVersionsResponse' {versions} -> versions) (\s@ListVersionsResponse' {} a -> s {versions = a} :: ListVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVersionsResponse_httpStatus :: Lens.Lens' ListVersionsResponse Prelude.Int
listVersionsResponse_httpStatus = Lens.lens (\ListVersionsResponse' {httpStatus} -> httpStatus) (\s@ListVersionsResponse' {} a -> s {httpStatus = a} :: ListVersionsResponse)

instance Prelude.NFData ListVersionsResponse where
  rnf ListVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
