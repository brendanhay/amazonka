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
-- Module      : Amazonka.Schemas.SearchSchemas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Search the schemas
--
-- This operation returns paginated results.
module Amazonka.Schemas.SearchSchemas
  ( -- * Creating a Request
    SearchSchemas (..),
    newSearchSchemas,

    -- * Request Lenses
    searchSchemas_limit,
    searchSchemas_nextToken,
    searchSchemas_registryName,
    searchSchemas_keywords,

    -- * Destructuring the Response
    SearchSchemasResponse (..),
    newSearchSchemasResponse,

    -- * Response Lenses
    searchSchemasResponse_nextToken,
    searchSchemasResponse_schemas,
    searchSchemasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newSearchSchemas' smart constructor.
data SearchSchemas = SearchSchemas'
  { limit :: Prelude.Maybe Prelude.Int,
    -- | The token that specifies the next page of results to return. To request
    -- the first page, leave NextToken empty. The token will expire in 24
    -- hours, and cannot be shared with other accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | Specifying this limits the results to only schemas that include the
    -- provided keywords.
    keywords :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSchemas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'searchSchemas_limit' - Undocumented member.
--
-- 'nextToken', 'searchSchemas_nextToken' - The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
--
-- 'registryName', 'searchSchemas_registryName' - The name of the registry.
--
-- 'keywords', 'searchSchemas_keywords' - Specifying this limits the results to only schemas that include the
-- provided keywords.
newSearchSchemas ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'keywords'
  Prelude.Text ->
  SearchSchemas
newSearchSchemas pRegistryName_ pKeywords_ =
  SearchSchemas'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      registryName = pRegistryName_,
      keywords = pKeywords_
    }

-- | Undocumented member.
searchSchemas_limit :: Lens.Lens' SearchSchemas (Prelude.Maybe Prelude.Int)
searchSchemas_limit = Lens.lens (\SearchSchemas' {limit} -> limit) (\s@SearchSchemas' {} a -> s {limit = a} :: SearchSchemas)

-- | The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
searchSchemas_nextToken :: Lens.Lens' SearchSchemas (Prelude.Maybe Prelude.Text)
searchSchemas_nextToken = Lens.lens (\SearchSchemas' {nextToken} -> nextToken) (\s@SearchSchemas' {} a -> s {nextToken = a} :: SearchSchemas)

-- | The name of the registry.
searchSchemas_registryName :: Lens.Lens' SearchSchemas Prelude.Text
searchSchemas_registryName = Lens.lens (\SearchSchemas' {registryName} -> registryName) (\s@SearchSchemas' {} a -> s {registryName = a} :: SearchSchemas)

-- | Specifying this limits the results to only schemas that include the
-- provided keywords.
searchSchemas_keywords :: Lens.Lens' SearchSchemas Prelude.Text
searchSchemas_keywords = Lens.lens (\SearchSchemas' {keywords} -> keywords) (\s@SearchSchemas' {} a -> s {keywords = a} :: SearchSchemas)

instance Core.AWSPager SearchSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchSchemasResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchSchemasResponse_schemas
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchSchemas_nextToken
          Lens..~ rs
          Lens.^? searchSchemasResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchSchemas where
  type
    AWSResponse SearchSchemas =
      SearchSchemasResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchSchemasResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchSchemas where
  hashWithSalt _salt SearchSchemas' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` keywords

instance Prelude.NFData SearchSchemas where
  rnf SearchSchemas' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf keywords

instance Data.ToHeaders SearchSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath SearchSchemas where
  toPath SearchSchemas' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Data.toBS registryName,
        "/schemas/search"
      ]

instance Data.ToQuery SearchSchemas where
  toQuery SearchSchemas' {..} =
    Prelude.mconcat
      [ "limit" Data.=: limit,
        "nextToken" Data.=: nextToken,
        "keywords" Data.=: keywords
      ]

-- | /See:/ 'newSearchSchemasResponse' smart constructor.
data SearchSchemasResponse = SearchSchemasResponse'
  { -- | The token that specifies the next page of results to return. To request
    -- the first page, leave NextToken empty. The token will expire in 24
    -- hours, and cannot be shared with other accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of SearchSchemaSummary information.
    schemas :: Prelude.Maybe [SearchSchemaSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchSchemasResponse_nextToken' - The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
--
-- 'schemas', 'searchSchemasResponse_schemas' - An array of SearchSchemaSummary information.
--
-- 'httpStatus', 'searchSchemasResponse_httpStatus' - The response's http status code.
newSearchSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchSchemasResponse
newSearchSchemasResponse pHttpStatus_ =
  SearchSchemasResponse'
    { nextToken = Prelude.Nothing,
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
searchSchemasResponse_nextToken :: Lens.Lens' SearchSchemasResponse (Prelude.Maybe Prelude.Text)
searchSchemasResponse_nextToken = Lens.lens (\SearchSchemasResponse' {nextToken} -> nextToken) (\s@SearchSchemasResponse' {} a -> s {nextToken = a} :: SearchSchemasResponse)

-- | An array of SearchSchemaSummary information.
searchSchemasResponse_schemas :: Lens.Lens' SearchSchemasResponse (Prelude.Maybe [SearchSchemaSummary])
searchSchemasResponse_schemas = Lens.lens (\SearchSchemasResponse' {schemas} -> schemas) (\s@SearchSchemasResponse' {} a -> s {schemas = a} :: SearchSchemasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchSchemasResponse_httpStatus :: Lens.Lens' SearchSchemasResponse Prelude.Int
searchSchemasResponse_httpStatus = Lens.lens (\SearchSchemasResponse' {httpStatus} -> httpStatus) (\s@SearchSchemasResponse' {} a -> s {httpStatus = a} :: SearchSchemasResponse)

instance Prelude.NFData SearchSchemasResponse where
  rnf SearchSchemasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf httpStatus
