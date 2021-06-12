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
-- Module      : Network.AWS.Glue.ListSchemas
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glue.ListSchemas
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSchemas' smart constructor.
data ListSchemas = ListSchemas'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | A wrapper structure that may contain the registry name and Amazon
    -- Resource Name (ARN).
    registryId :: Core.Maybe RegistryId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      registryId = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
listSchemas_nextToken :: Lens.Lens' ListSchemas (Core.Maybe Core.Text)
listSchemas_nextToken = Lens.lens (\ListSchemas' {nextToken} -> nextToken) (\s@ListSchemas' {} a -> s {nextToken = a} :: ListSchemas)

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
listSchemas_maxResults :: Lens.Lens' ListSchemas (Core.Maybe Core.Natural)
listSchemas_maxResults = Lens.lens (\ListSchemas' {maxResults} -> maxResults) (\s@ListSchemas' {} a -> s {maxResults = a} :: ListSchemas)

-- | A wrapper structure that may contain the registry name and Amazon
-- Resource Name (ARN).
listSchemas_registryId :: Lens.Lens' ListSchemas (Core.Maybe RegistryId)
listSchemas_registryId = Lens.lens (\ListSchemas' {registryId} -> registryId) (\s@ListSchemas' {} a -> s {registryId = a} :: ListSchemas)

instance Core.AWSPager ListSchemas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemasResponse_schemas Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSchemas_nextToken
          Lens..~ rs
          Lens.^? listSchemasResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListSchemas where
  type AWSResponse ListSchemas = ListSchemasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemasResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Schemas" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSchemas

instance Core.NFData ListSchemas

instance Core.ToHeaders ListSchemas where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListSchemas" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSchemas where
  toJSON ListSchemas' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("RegistryId" Core..=) Core.<$> registryId
          ]
      )

instance Core.ToPath ListSchemas where
  toPath = Core.const "/"

instance Core.ToQuery ListSchemas where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSchemasResponse' smart constructor.
data ListSchemasResponse = ListSchemasResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @SchemaListItem@ objects containing details of each schema.
    schemas :: Core.Maybe [SchemaListItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSchemasResponse
newListSchemasResponse pHttpStatus_ =
  ListSchemasResponse'
    { nextToken = Core.Nothing,
      schemas = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
listSchemasResponse_nextToken :: Lens.Lens' ListSchemasResponse (Core.Maybe Core.Text)
listSchemasResponse_nextToken = Lens.lens (\ListSchemasResponse' {nextToken} -> nextToken) (\s@ListSchemasResponse' {} a -> s {nextToken = a} :: ListSchemasResponse)

-- | An array of @SchemaListItem@ objects containing details of each schema.
listSchemasResponse_schemas :: Lens.Lens' ListSchemasResponse (Core.Maybe [SchemaListItem])
listSchemasResponse_schemas = Lens.lens (\ListSchemasResponse' {schemas} -> schemas) (\s@ListSchemasResponse' {} a -> s {schemas = a} :: ListSchemasResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSchemasResponse_httpStatus :: Lens.Lens' ListSchemasResponse Core.Int
listSchemasResponse_httpStatus = Lens.lens (\ListSchemasResponse' {httpStatus} -> httpStatus) (\s@ListSchemasResponse' {} a -> s {httpStatus = a} :: ListSchemasResponse)

instance Core.NFData ListSchemasResponse
