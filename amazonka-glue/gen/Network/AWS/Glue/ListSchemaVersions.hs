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
-- Module      : Network.AWS.Glue.ListSchemaVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of schema versions that you have created, with minimal
-- information. Schema versions in Deleted status will not be included in
-- the results. Empty results will be returned if there are no schema
-- versions available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListSchemaVersions
  ( -- * Creating a Request
    ListSchemaVersions (..),
    newListSchemaVersions,

    -- * Request Lenses
    listSchemaVersions_nextToken,
    listSchemaVersions_maxResults,
    listSchemaVersions_schemaId,

    -- * Destructuring the Response
    ListSchemaVersionsResponse (..),
    newListSchemaVersionsResponse,

    -- * Response Lenses
    listSchemaVersionsResponse_nextToken,
    listSchemaVersionsResponse_schemas,
    listSchemaVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSchemaVersions' smart constructor.
data ListSchemaVersions = ListSchemaVersions'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
    --     provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
    --     @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: SchemaId
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
-- 'nextToken', 'listSchemaVersions_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'listSchemaVersions_maxResults' - Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
--
-- 'schemaId', 'listSchemaVersions_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
newListSchemaVersions ::
  -- | 'schemaId'
  SchemaId ->
  ListSchemaVersions
newListSchemaVersions pSchemaId_ =
  ListSchemaVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      schemaId = pSchemaId_
    }

-- | A continuation token, if this is a continuation call.
listSchemaVersions_nextToken :: Lens.Lens' ListSchemaVersions (Prelude.Maybe Prelude.Text)
listSchemaVersions_nextToken = Lens.lens (\ListSchemaVersions' {nextToken} -> nextToken) (\s@ListSchemaVersions' {} a -> s {nextToken = a} :: ListSchemaVersions)

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
listSchemaVersions_maxResults :: Lens.Lens' ListSchemaVersions (Prelude.Maybe Prelude.Natural)
listSchemaVersions_maxResults = Lens.lens (\ListSchemaVersions' {maxResults} -> maxResults) (\s@ListSchemaVersions' {} a -> s {maxResults = a} :: ListSchemaVersions)

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
listSchemaVersions_schemaId :: Lens.Lens' ListSchemaVersions SchemaId
listSchemaVersions_schemaId = Lens.lens (\ListSchemaVersions' {schemaId} -> schemaId) (\s@ListSchemaVersions' {} a -> s {schemaId = a} :: ListSchemaVersions)

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
            Lens.^? listSchemaVersionsResponse_schemas
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSchemaVersions_nextToken
          Lens..~ rs
          Lens.^? listSchemaVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSchemaVersions where
  type
    AWSResponse ListSchemaVersions =
      ListSchemaVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemaVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemaVersions

instance Prelude.NFData ListSchemaVersions

instance Core.ToHeaders ListSchemaVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.ListSchemaVersions" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSchemaVersions where
  toJSON ListSchemaVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("SchemaId" Core..= schemaId)
          ]
      )

instance Core.ToPath ListSchemaVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSchemaVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchemaVersionsResponse' smart constructor.
data ListSchemaVersionsResponse = ListSchemaVersionsResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @SchemaVersionList@ objects containing details of each
    -- schema version.
    schemas :: Prelude.Maybe [SchemaVersionListItem],
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
-- 'nextToken', 'listSchemaVersionsResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'schemas', 'listSchemaVersionsResponse_schemas' - An array of @SchemaVersionList@ objects containing details of each
-- schema version.
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
      schemas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
listSchemaVersionsResponse_nextToken :: Lens.Lens' ListSchemaVersionsResponse (Prelude.Maybe Prelude.Text)
listSchemaVersionsResponse_nextToken = Lens.lens (\ListSchemaVersionsResponse' {nextToken} -> nextToken) (\s@ListSchemaVersionsResponse' {} a -> s {nextToken = a} :: ListSchemaVersionsResponse)

-- | An array of @SchemaVersionList@ objects containing details of each
-- schema version.
listSchemaVersionsResponse_schemas :: Lens.Lens' ListSchemaVersionsResponse (Prelude.Maybe [SchemaVersionListItem])
listSchemaVersionsResponse_schemas = Lens.lens (\ListSchemaVersionsResponse' {schemas} -> schemas) (\s@ListSchemaVersionsResponse' {} a -> s {schemas = a} :: ListSchemaVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSchemaVersionsResponse_httpStatus :: Lens.Lens' ListSchemaVersionsResponse Prelude.Int
listSchemaVersionsResponse_httpStatus = Lens.lens (\ListSchemaVersionsResponse' {httpStatus} -> httpStatus) (\s@ListSchemaVersionsResponse' {} a -> s {httpStatus = a} :: ListSchemaVersionsResponse)

instance Prelude.NFData ListSchemaVersionsResponse
