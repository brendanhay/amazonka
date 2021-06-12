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
-- Module      : Network.AWS.ElasticSearch.ListElasticsearchVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all supported Elasticsearch versions
--
-- This operation returns paginated results.
module Network.AWS.ElasticSearch.ListElasticsearchVersions
  ( -- * Creating a Request
    ListElasticsearchVersions (..),
    newListElasticsearchVersions,

    -- * Request Lenses
    listElasticsearchVersions_nextToken,
    listElasticsearchVersions_maxResults,

    -- * Destructuring the Response
    ListElasticsearchVersionsResponse (..),
    newListElasticsearchVersionsResponse,

    -- * Response Lenses
    listElasticsearchVersionsResponse_nextToken,
    listElasticsearchVersionsResponse_elasticsearchVersions,
    listElasticsearchVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @ ListElasticsearchVersions @
-- operation.
--
-- Use @ MaxResults @ to control the maximum number of results to retrieve
-- in a single call.
--
-- Use @ NextToken @ in response to retrieve more results. If the received
-- response does not contain a NextToken, then there are no more results to
-- retrieve.
--
-- /See:/ 'newListElasticsearchVersions' smart constructor.
data ListElasticsearchVersions = ListElasticsearchVersions'
  { nextToken :: Core.Maybe Core.Text,
    -- | Set this value to limit the number of results returned. Value provided
    -- must be greater than 10 else it wont be honored.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListElasticsearchVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listElasticsearchVersions_nextToken' - Undocumented member.
--
-- 'maxResults', 'listElasticsearchVersions_maxResults' - Set this value to limit the number of results returned. Value provided
-- must be greater than 10 else it wont be honored.
newListElasticsearchVersions ::
  ListElasticsearchVersions
newListElasticsearchVersions =
  ListElasticsearchVersions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Undocumented member.
listElasticsearchVersions_nextToken :: Lens.Lens' ListElasticsearchVersions (Core.Maybe Core.Text)
listElasticsearchVersions_nextToken = Lens.lens (\ListElasticsearchVersions' {nextToken} -> nextToken) (\s@ListElasticsearchVersions' {} a -> s {nextToken = a} :: ListElasticsearchVersions)

-- | Set this value to limit the number of results returned. Value provided
-- must be greater than 10 else it wont be honored.
listElasticsearchVersions_maxResults :: Lens.Lens' ListElasticsearchVersions (Core.Maybe Core.Int)
listElasticsearchVersions_maxResults = Lens.lens (\ListElasticsearchVersions' {maxResults} -> maxResults) (\s@ListElasticsearchVersions' {} a -> s {maxResults = a} :: ListElasticsearchVersions)

instance Core.AWSPager ListElasticsearchVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listElasticsearchVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listElasticsearchVersionsResponse_elasticsearchVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listElasticsearchVersions_nextToken
          Lens..~ rs
          Lens.^? listElasticsearchVersionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListElasticsearchVersions where
  type
    AWSResponse ListElasticsearchVersions =
      ListElasticsearchVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListElasticsearchVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ElasticsearchVersions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListElasticsearchVersions

instance Core.NFData ListElasticsearchVersions

instance Core.ToHeaders ListElasticsearchVersions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListElasticsearchVersions where
  toPath = Core.const "/2015-01-01/es/versions"

instance Core.ToQuery ListElasticsearchVersions where
  toQuery ListElasticsearchVersions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Container for the parameters for response received from
-- @ ListElasticsearchVersions @ operation.
--
-- /See:/ 'newListElasticsearchVersionsResponse' smart constructor.
data ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse'
  { nextToken :: Core.Maybe Core.Text,
    elasticsearchVersions :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListElasticsearchVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listElasticsearchVersionsResponse_nextToken' - Undocumented member.
--
-- 'elasticsearchVersions', 'listElasticsearchVersionsResponse_elasticsearchVersions' - Undocumented member.
--
-- 'httpStatus', 'listElasticsearchVersionsResponse_httpStatus' - The response's http status code.
newListElasticsearchVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListElasticsearchVersionsResponse
newListElasticsearchVersionsResponse pHttpStatus_ =
  ListElasticsearchVersionsResponse'
    { nextToken =
        Core.Nothing,
      elasticsearchVersions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listElasticsearchVersionsResponse_nextToken :: Lens.Lens' ListElasticsearchVersionsResponse (Core.Maybe Core.Text)
listElasticsearchVersionsResponse_nextToken = Lens.lens (\ListElasticsearchVersionsResponse' {nextToken} -> nextToken) (\s@ListElasticsearchVersionsResponse' {} a -> s {nextToken = a} :: ListElasticsearchVersionsResponse)

-- | Undocumented member.
listElasticsearchVersionsResponse_elasticsearchVersions :: Lens.Lens' ListElasticsearchVersionsResponse (Core.Maybe [Core.Text])
listElasticsearchVersionsResponse_elasticsearchVersions = Lens.lens (\ListElasticsearchVersionsResponse' {elasticsearchVersions} -> elasticsearchVersions) (\s@ListElasticsearchVersionsResponse' {} a -> s {elasticsearchVersions = a} :: ListElasticsearchVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listElasticsearchVersionsResponse_httpStatus :: Lens.Lens' ListElasticsearchVersionsResponse Core.Int
listElasticsearchVersionsResponse_httpStatus = Lens.lens (\ListElasticsearchVersionsResponse' {httpStatus} -> httpStatus) (\s@ListElasticsearchVersionsResponse' {} a -> s {httpStatus = a} :: ListElasticsearchVersionsResponse)

instance
  Core.NFData
    ListElasticsearchVersionsResponse
