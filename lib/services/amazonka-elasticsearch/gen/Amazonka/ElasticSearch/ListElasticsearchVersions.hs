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
-- Module      : Amazonka.ElasticSearch.ListElasticsearchVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all supported Elasticsearch versions
--
-- This operation returns paginated results.
module Amazonka.ElasticSearch.ListElasticsearchVersions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSPager ListElasticsearchVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listElasticsearchVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listElasticsearchVersionsResponse_elasticsearchVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listElasticsearchVersions_nextToken
          Lens..~ rs
          Lens.^? listElasticsearchVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListElasticsearchVersions where
  type
    AWSResponse ListElasticsearchVersions =
      ListElasticsearchVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListElasticsearchVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ElasticsearchVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListElasticsearchVersions where
  hashWithSalt _salt ListElasticsearchVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListElasticsearchVersions where
  rnf ListElasticsearchVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListElasticsearchVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListElasticsearchVersions where
  toPath = Prelude.const "/2015-01-01/es/versions"

instance Data.ToQuery ListElasticsearchVersions where
  toQuery ListElasticsearchVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listElasticsearchVersionsResponse_elasticsearchVersions = Lens.lens (\ListElasticsearchVersionsResponse' {elasticsearchVersions} -> elasticsearchVersions) (\s@ListElasticsearchVersionsResponse' {} a -> s {elasticsearchVersions = a} :: ListElasticsearchVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listElasticsearchVersionsResponse_httpStatus :: Lens.Lens' ListElasticsearchVersionsResponse Prelude.Int
listElasticsearchVersionsResponse_httpStatus = Lens.lens (\ListElasticsearchVersionsResponse' {httpStatus} -> httpStatus) (\s@ListElasticsearchVersionsResponse' {} a -> s {httpStatus = a} :: ListElasticsearchVersionsResponse)

instance
  Prelude.NFData
    ListElasticsearchVersionsResponse
  where
  rnf ListElasticsearchVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf elasticsearchVersions
      `Prelude.seq` Prelude.rnf httpStatus
