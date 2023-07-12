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
-- Module      : Amazonka.Schemas.ListSchemaVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of the schema versions and related information.
--
-- This operation returns paginated results.
module Amazonka.Schemas.ListSchemaVersions
  ( -- * Creating a Request
    ListSchemaVersions (..),
    newListSchemaVersions,

    -- * Request Lenses
    listSchemaVersions_limit,
    listSchemaVersions_nextToken,
    listSchemaVersions_registryName,
    listSchemaVersions_schemaName,

    -- * Destructuring the Response
    ListSchemaVersionsResponse (..),
    newListSchemaVersionsResponse,

    -- * Response Lenses
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_schemaVersions,
    listSchemaVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newListSchemaVersions' smart constructor.
data ListSchemaVersions = ListSchemaVersions'
  { limit :: Prelude.Maybe Prelude.Int,
    -- | The token that specifies the next page of results to return. To request
    -- the first page, leave NextToken empty. The token will expire in 24
    -- hours, and cannot be shared with other accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemaVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listSchemaVersions_limit' - Undocumented member.
--
-- 'nextToken', 'listSchemaVersions_nextToken' - The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
--
-- 'registryName', 'listSchemaVersions_registryName' - The name of the registry.
--
-- 'schemaName', 'listSchemaVersions_schemaName' - The name of the schema.
newListSchemaVersions ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  ListSchemaVersions
newListSchemaVersions pRegistryName_ pSchemaName_ =
  ListSchemaVersions'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      registryName = pRegistryName_,
      schemaName = pSchemaName_
    }

-- | Undocumented member.
listSchemaVersions_limit :: Lens.Lens' ListSchemaVersions (Prelude.Maybe Prelude.Int)
listSchemaVersions_limit = Lens.lens (\ListSchemaVersions' {limit} -> limit) (\s@ListSchemaVersions' {} a -> s {limit = a} :: ListSchemaVersions)

-- | The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
listSchemaVersions_nextToken :: Lens.Lens' ListSchemaVersions (Prelude.Maybe Prelude.Text)
listSchemaVersions_nextToken = Lens.lens (\ListSchemaVersions' {nextToken} -> nextToken) (\s@ListSchemaVersions' {} a -> s {nextToken = a} :: ListSchemaVersions)

-- | The name of the registry.
listSchemaVersions_registryName :: Lens.Lens' ListSchemaVersions Prelude.Text
listSchemaVersions_registryName = Lens.lens (\ListSchemaVersions' {registryName} -> registryName) (\s@ListSchemaVersions' {} a -> s {registryName = a} :: ListSchemaVersions)

-- | The name of the schema.
listSchemaVersions_schemaName :: Lens.Lens' ListSchemaVersions Prelude.Text
listSchemaVersions_schemaName = Lens.lens (\ListSchemaVersions' {schemaName} -> schemaName) (\s@ListSchemaVersions' {} a -> s {schemaName = a} :: ListSchemaVersions)

instance Core.AWSPager ListSchemaVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemaVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemaVersionsResponse_schemaVersions
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSchemaVersions_nextToken
          Lens..~ rs
          Lens.^? listSchemaVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSchemaVersions where
  type
    AWSResponse ListSchemaVersions =
      ListSchemaVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemaVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SchemaVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemaVersions where
  hashWithSalt _salt ListSchemaVersions' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName

instance Prelude.NFData ListSchemaVersions where
  rnf ListSchemaVersions' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName

instance Data.ToHeaders ListSchemaVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSchemaVersions where
  toPath ListSchemaVersions' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Data.toBS registryName,
        "/schemas/name/",
        Data.toBS schemaName,
        "/versions"
      ]

instance Data.ToQuery ListSchemaVersions where
  toQuery ListSchemaVersions' {..} =
    Prelude.mconcat
      [ "limit" Data.=: limit,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSchemaVersionsResponse' smart constructor.
data ListSchemaVersionsResponse = ListSchemaVersionsResponse'
  { -- | The token that specifies the next page of results to return. To request
    -- the first page, leave NextToken empty. The token will expire in 24
    -- hours, and cannot be shared with other accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of schema version summaries.
    schemaVersions :: Prelude.Maybe [SchemaVersionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemaVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchemaVersionsResponse_nextToken' - The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
--
-- 'schemaVersions', 'listSchemaVersionsResponse_schemaVersions' - An array of schema version summaries.
--
-- 'httpStatus', 'listSchemaVersionsResponse_httpStatus' - The response's http status code.
newListSchemaVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemaVersionsResponse
newListSchemaVersionsResponse pHttpStatus_ =
  ListSchemaVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      schemaVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
listSchemaVersionsResponse_nextToken :: Lens.Lens' ListSchemaVersionsResponse (Prelude.Maybe Prelude.Text)
listSchemaVersionsResponse_nextToken = Lens.lens (\ListSchemaVersionsResponse' {nextToken} -> nextToken) (\s@ListSchemaVersionsResponse' {} a -> s {nextToken = a} :: ListSchemaVersionsResponse)

-- | An array of schema version summaries.
listSchemaVersionsResponse_schemaVersions :: Lens.Lens' ListSchemaVersionsResponse (Prelude.Maybe [SchemaVersionSummary])
listSchemaVersionsResponse_schemaVersions = Lens.lens (\ListSchemaVersionsResponse' {schemaVersions} -> schemaVersions) (\s@ListSchemaVersionsResponse' {} a -> s {schemaVersions = a} :: ListSchemaVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSchemaVersionsResponse_httpStatus :: Lens.Lens' ListSchemaVersionsResponse Prelude.Int
listSchemaVersionsResponse_httpStatus = Lens.lens (\ListSchemaVersionsResponse' {httpStatus} -> httpStatus) (\s@ListSchemaVersionsResponse' {} a -> s {httpStatus = a} :: ListSchemaVersionsResponse)

instance Prelude.NFData ListSchemaVersionsResponse where
  rnf ListSchemaVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaVersions
      `Prelude.seq` Prelude.rnf httpStatus
