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
-- Module      : Network.AWS.Glue.QuerySchemaVersionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for the schema version metadata information.
module Network.AWS.Glue.QuerySchemaVersionMetadata
  ( -- * Creating a Request
    QuerySchemaVersionMetadata (..),
    newQuerySchemaVersionMetadata,

    -- * Request Lenses
    querySchemaVersionMetadata_nextToken,
    querySchemaVersionMetadata_schemaVersionId,
    querySchemaVersionMetadata_maxResults,
    querySchemaVersionMetadata_schemaVersionNumber,
    querySchemaVersionMetadata_metadataList,
    querySchemaVersionMetadata_schemaId,

    -- * Destructuring the Response
    QuerySchemaVersionMetadataResponse (..),
    newQuerySchemaVersionMetadataResponse,

    -- * Response Lenses
    querySchemaVersionMetadataResponse_metadataInfoMap,
    querySchemaVersionMetadataResponse_nextToken,
    querySchemaVersionMetadataResponse_schemaVersionId,
    querySchemaVersionMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newQuerySchemaVersionMetadata' smart constructor.
data QuerySchemaVersionMetadata = QuerySchemaVersionMetadata'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe SchemaVersionNumber,
    -- | Search key-value pairs for metadata, if they are not provided all the
    -- metadata information will be fetched.
    metadataList :: Core.Maybe [MetadataKeyValuePair],
    -- | A wrapper structure that may contain the schema name and Amazon Resource
    -- Name (ARN).
    schemaId :: Core.Maybe SchemaId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QuerySchemaVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'querySchemaVersionMetadata_nextToken' - A continuation token, if this is a continuation call.
--
-- 'schemaVersionId', 'querySchemaVersionMetadata_schemaVersionId' - The unique version ID of the schema version.
--
-- 'maxResults', 'querySchemaVersionMetadata_maxResults' - Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
--
-- 'schemaVersionNumber', 'querySchemaVersionMetadata_schemaVersionNumber' - The version number of the schema.
--
-- 'metadataList', 'querySchemaVersionMetadata_metadataList' - Search key-value pairs for metadata, if they are not provided all the
-- metadata information will be fetched.
--
-- 'schemaId', 'querySchemaVersionMetadata_schemaId' - A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
newQuerySchemaVersionMetadata ::
  QuerySchemaVersionMetadata
newQuerySchemaVersionMetadata =
  QuerySchemaVersionMetadata'
    { nextToken =
        Core.Nothing,
      schemaVersionId = Core.Nothing,
      maxResults = Core.Nothing,
      schemaVersionNumber = Core.Nothing,
      metadataList = Core.Nothing,
      schemaId = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
querySchemaVersionMetadata_nextToken :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Core.Text)
querySchemaVersionMetadata_nextToken = Lens.lens (\QuerySchemaVersionMetadata' {nextToken} -> nextToken) (\s@QuerySchemaVersionMetadata' {} a -> s {nextToken = a} :: QuerySchemaVersionMetadata)

-- | The unique version ID of the schema version.
querySchemaVersionMetadata_schemaVersionId :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Core.Text)
querySchemaVersionMetadata_schemaVersionId = Lens.lens (\QuerySchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@QuerySchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: QuerySchemaVersionMetadata)

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
querySchemaVersionMetadata_maxResults :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Core.Natural)
querySchemaVersionMetadata_maxResults = Lens.lens (\QuerySchemaVersionMetadata' {maxResults} -> maxResults) (\s@QuerySchemaVersionMetadata' {} a -> s {maxResults = a} :: QuerySchemaVersionMetadata)

-- | The version number of the schema.
querySchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe SchemaVersionNumber)
querySchemaVersionMetadata_schemaVersionNumber = Lens.lens (\QuerySchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@QuerySchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: QuerySchemaVersionMetadata)

-- | Search key-value pairs for metadata, if they are not provided all the
-- metadata information will be fetched.
querySchemaVersionMetadata_metadataList :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe [MetadataKeyValuePair])
querySchemaVersionMetadata_metadataList = Lens.lens (\QuerySchemaVersionMetadata' {metadataList} -> metadataList) (\s@QuerySchemaVersionMetadata' {} a -> s {metadataList = a} :: QuerySchemaVersionMetadata) Core.. Lens.mapping Lens._Coerce

-- | A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
querySchemaVersionMetadata_schemaId :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe SchemaId)
querySchemaVersionMetadata_schemaId = Lens.lens (\QuerySchemaVersionMetadata' {schemaId} -> schemaId) (\s@QuerySchemaVersionMetadata' {} a -> s {schemaId = a} :: QuerySchemaVersionMetadata)

instance Core.AWSRequest QuerySchemaVersionMetadata where
  type
    AWSResponse QuerySchemaVersionMetadata =
      QuerySchemaVersionMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          QuerySchemaVersionMetadataResponse'
            Core.<$> (x Core..?> "MetadataInfoMap" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "SchemaVersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable QuerySchemaVersionMetadata

instance Core.NFData QuerySchemaVersionMetadata

instance Core.ToHeaders QuerySchemaVersionMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.QuerySchemaVersionMetadata" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON QuerySchemaVersionMetadata where
  toJSON QuerySchemaVersionMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SchemaVersionNumber" Core..=)
              Core.<$> schemaVersionNumber,
            ("MetadataList" Core..=) Core.<$> metadataList,
            ("SchemaId" Core..=) Core.<$> schemaId
          ]
      )

instance Core.ToPath QuerySchemaVersionMetadata where
  toPath = Core.const "/"

instance Core.ToQuery QuerySchemaVersionMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newQuerySchemaVersionMetadataResponse' smart constructor.
data QuerySchemaVersionMetadataResponse = QuerySchemaVersionMetadataResponse'
  { -- | A map of a metadata key and associated values.
    metadataInfoMap :: Core.Maybe (Core.HashMap Core.Text MetadataInfo),
    -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Core.Maybe Core.Text,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QuerySchemaVersionMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataInfoMap', 'querySchemaVersionMetadataResponse_metadataInfoMap' - A map of a metadata key and associated values.
--
-- 'nextToken', 'querySchemaVersionMetadataResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'schemaVersionId', 'querySchemaVersionMetadataResponse_schemaVersionId' - The unique version ID of the schema version.
--
-- 'httpStatus', 'querySchemaVersionMetadataResponse_httpStatus' - The response's http status code.
newQuerySchemaVersionMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  QuerySchemaVersionMetadataResponse
newQuerySchemaVersionMetadataResponse pHttpStatus_ =
  QuerySchemaVersionMetadataResponse'
    { metadataInfoMap =
        Core.Nothing,
      nextToken = Core.Nothing,
      schemaVersionId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of a metadata key and associated values.
querySchemaVersionMetadataResponse_metadataInfoMap :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe (Core.HashMap Core.Text MetadataInfo))
querySchemaVersionMetadataResponse_metadataInfoMap = Lens.lens (\QuerySchemaVersionMetadataResponse' {metadataInfoMap} -> metadataInfoMap) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {metadataInfoMap = a} :: QuerySchemaVersionMetadataResponse) Core.. Lens.mapping Lens._Coerce

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
querySchemaVersionMetadataResponse_nextToken :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe Core.Text)
querySchemaVersionMetadataResponse_nextToken = Lens.lens (\QuerySchemaVersionMetadataResponse' {nextToken} -> nextToken) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {nextToken = a} :: QuerySchemaVersionMetadataResponse)

-- | The unique version ID of the schema version.
querySchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe Core.Text)
querySchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\QuerySchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: QuerySchemaVersionMetadataResponse)

-- | The response's http status code.
querySchemaVersionMetadataResponse_httpStatus :: Lens.Lens' QuerySchemaVersionMetadataResponse Core.Int
querySchemaVersionMetadataResponse_httpStatus = Lens.lens (\QuerySchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: QuerySchemaVersionMetadataResponse)

instance
  Core.NFData
    QuerySchemaVersionMetadataResponse
