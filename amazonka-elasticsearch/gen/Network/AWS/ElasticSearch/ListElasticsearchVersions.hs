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

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. Value provided
    -- must be greater than 10 else it wont be honored.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Undocumented member.
listElasticsearchVersions_nextToken :: Lens.Lens' ListElasticsearchVersions (Prelude.Maybe Prelude.Text)
listElasticsearchVersions_nextToken = Lens.lens (\ListElasticsearchVersions' {nextToken} -> nextToken) (\s@ListElasticsearchVersions' {} a -> s {nextToken = a} :: ListElasticsearchVersions)

-- | Set this value to limit the number of results returned. Value provided
-- must be greater than 10 else it wont be honored.
listElasticsearchVersions_maxResults :: Lens.Lens' ListElasticsearchVersions (Prelude.Maybe Prelude.Int)
listElasticsearchVersions_maxResults = Lens.lens (\ListElasticsearchVersions' {maxResults} -> maxResults) (\s@ListElasticsearchVersions' {} a -> s {maxResults = a} :: ListElasticsearchVersions)

instance Pager.AWSPager ListElasticsearchVersions where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listElasticsearchVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listElasticsearchVersionsResponse_elasticsearchVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listElasticsearchVersions_nextToken
          Lens..~ rs
          Lens.^? listElasticsearchVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListElasticsearchVersions where
  type
    Rs ListElasticsearchVersions =
      ListElasticsearchVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListElasticsearchVersionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ElasticsearchVersions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListElasticsearchVersions

instance Prelude.NFData ListElasticsearchVersions

instance Prelude.ToHeaders ListElasticsearchVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListElasticsearchVersions where
  toPath = Prelude.const "/2015-01-01/es/versions"

instance Prelude.ToQuery ListElasticsearchVersions where
  toQuery ListElasticsearchVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | Container for the parameters for response received from
-- @ ListElasticsearchVersions @ operation.
--
-- /See:/ 'newListElasticsearchVersionsResponse' smart constructor.
data ListElasticsearchVersionsResponse = ListElasticsearchVersionsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    elasticsearchVersions :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListElasticsearchVersionsResponse
newListElasticsearchVersionsResponse pHttpStatus_ =
  ListElasticsearchVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      elasticsearchVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listElasticsearchVersionsResponse_nextToken :: Lens.Lens' ListElasticsearchVersionsResponse (Prelude.Maybe Prelude.Text)
listElasticsearchVersionsResponse_nextToken = Lens.lens (\ListElasticsearchVersionsResponse' {nextToken} -> nextToken) (\s@ListElasticsearchVersionsResponse' {} a -> s {nextToken = a} :: ListElasticsearchVersionsResponse)

-- | Undocumented member.
listElasticsearchVersionsResponse_elasticsearchVersions :: Lens.Lens' ListElasticsearchVersionsResponse (Prelude.Maybe [Prelude.Text])
listElasticsearchVersionsResponse_elasticsearchVersions = Lens.lens (\ListElasticsearchVersionsResponse' {elasticsearchVersions} -> elasticsearchVersions) (\s@ListElasticsearchVersionsResponse' {} a -> s {elasticsearchVersions = a} :: ListElasticsearchVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listElasticsearchVersionsResponse_httpStatus :: Lens.Lens' ListElasticsearchVersionsResponse Prelude.Int
listElasticsearchVersionsResponse_httpStatus = Lens.lens (\ListElasticsearchVersionsResponse' {httpStatus} -> httpStatus) (\s@ListElasticsearchVersionsResponse' {} a -> s {httpStatus = a} :: ListElasticsearchVersionsResponse)

instance
  Prelude.NFData
    ListElasticsearchVersionsResponse
