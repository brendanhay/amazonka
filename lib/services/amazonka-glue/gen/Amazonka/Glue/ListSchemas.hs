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
-- Module      : Amazonka.Glue.ListSchemas
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schemas with minimal details. Schemas in Deleting
-- status will not be included in the results. Empty results will be
-- returned if there are no schemas available.
--
-- When the @RegistryId@ is not provided, all the schemas across registries
-- will be part of the API response.
--
-- This operation returns paginated results.
module Amazonka.Glue.ListSchemas
  ( -- * Creating a Request
    ListSchemas (..),
    newListSchemas,

    -- * Request Lenses
    listSchemas_nextToken,
    listSchemas_maxResults,
    listSchemas_registryId,

    -- * Destructuring the Response
    ListSchemasResponse (..),
    newListSchemasResponse,

    -- * Response Lenses
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A wrapper structure that may contain the registry name and Amazon
    -- Resource Name (ARN).
    registryId :: Prelude.Maybe RegistryId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchemas_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'listSchemas_maxResults' - Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
--
-- 'registryId', 'listSchemas_registryId' - A wrapper structure that may contain the registry name and Amazon
-- Resource Name (ARN).
newListSchemas ::
  ListSchemas
newListSchemas =
  ListSchemas'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      registryId = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation call.
listSchemas_nextToken :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Text)
listSchemas_nextToken = Lens.lens (\ListSchemas' {nextToken} -> nextToken) (\s@ListSchemas' {} a -> s {nextToken = a} :: ListSchemas)

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
listSchemas_maxResults :: Lens.Lens' ListSchemas (Prelude.Maybe Prelude.Natural)
listSchemas_maxResults = Lens.lens (\ListSchemas' {maxResults} -> maxResults) (\s@ListSchemas' {} a -> s {maxResults = a} :: ListSchemas)

-- | A wrapper structure that may contain the registry name and Amazon
-- Resource Name (ARN).
listSchemas_registryId :: Lens.Lens' ListSchemas (Prelude.Maybe RegistryId)
listSchemas_registryId = Lens.lens (\ListSchemas' {registryId} -> registryId) (\s@ListSchemas' {} a -> s {registryId = a} :: ListSchemas)

instance Core.AWSPager ListSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_schemas Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSchemas_nextToken
          Lens..~ rs
          Lens.^? listSchemasResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListSchemas where
  type AWSResponse ListSchemas = ListSchemasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemas where
  hashWithSalt _salt ListSchemas' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` registryId

instance Prelude.NFData ListSchemas where
  rnf ListSchemas' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf registryId

instance Core.ToHeaders ListSchemas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListSchemas" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("RegistryId" Core..=) Prelude.<$> registryId
          ]
      )

instance Core.ToPath ListSchemas where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSchemas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @SchemaListItem@ objects containing details of each schema.
    schemas :: Prelude.Maybe [SchemaListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchemasResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'schemas', 'listSchemasResponse_schemas' - An array of @SchemaListItem@ objects containing details of each schema.
--
-- 'httpStatus', 'listSchemasResponse_httpStatus' - The response's http status code.
newListSchemasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemasResponse
newListSchemasResponse pHttpStatus_ =
  ListSchemasResponse'
    { nextToken = Prelude.Nothing,
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
listSchemasResponse_nextToken :: Lens.Lens' ListSchemasResponse (Prelude.Maybe Prelude.Text)
listSchemasResponse_nextToken = Lens.lens (\ListSchemasResponse' {nextToken} -> nextToken) (\s@ListSchemasResponse' {} a -> s {nextToken = a} :: ListSchemasResponse)

-- | An array of @SchemaListItem@ objects containing details of each schema.
listSchemasResponse_schemas :: Lens.Lens' ListSchemasResponse (Prelude.Maybe [SchemaListItem])
listSchemasResponse_schemas = Lens.lens (\ListSchemasResponse' {schemas} -> schemas) (\s@ListSchemasResponse' {} a -> s {schemas = a} :: ListSchemasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSchemasResponse_httpStatus :: Lens.Lens' ListSchemasResponse Prelude.Int
listSchemasResponse_httpStatus = Lens.lens (\ListSchemasResponse' {httpStatus} -> httpStatus) (\s@ListSchemasResponse' {} a -> s {httpStatus = a} :: ListSchemasResponse)

instance Prelude.NFData ListSchemasResponse where
  rnf ListSchemasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf httpStatus
