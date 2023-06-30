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
-- Module      : Amazonka.Glue.QuerySchemaVersionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for the schema version metadata information.
module Amazonka.Glue.QuerySchemaVersionMetadata
  ( -- * Creating a Request
    QuerySchemaVersionMetadata (..),
    newQuerySchemaVersionMetadata,

    -- * Request Lenses
    querySchemaVersionMetadata_maxResults,
    querySchemaVersionMetadata_metadataList,
    querySchemaVersionMetadata_nextToken,
    querySchemaVersionMetadata_schemaId,
    querySchemaVersionMetadata_schemaVersionId,
    querySchemaVersionMetadata_schemaVersionNumber,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newQuerySchemaVersionMetadata' smart constructor.
data QuerySchemaVersionMetadata = QuerySchemaVersionMetadata'
  { -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Search key-value pairs for metadata, if they are not provided all the
    -- metadata information will be fetched.
    metadataList :: Prelude.Maybe [MetadataKeyValuePair],
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A wrapper structure that may contain the schema name and Amazon Resource
    -- Name (ARN).
    schemaId :: Prelude.Maybe SchemaId,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The version number of the schema.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuerySchemaVersionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'querySchemaVersionMetadata_maxResults' - Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
--
-- 'metadataList', 'querySchemaVersionMetadata_metadataList' - Search key-value pairs for metadata, if they are not provided all the
-- metadata information will be fetched.
--
-- 'nextToken', 'querySchemaVersionMetadata_nextToken' - A continuation token, if this is a continuation call.
--
-- 'schemaId', 'querySchemaVersionMetadata_schemaId' - A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
--
-- 'schemaVersionId', 'querySchemaVersionMetadata_schemaVersionId' - The unique version ID of the schema version.
--
-- 'schemaVersionNumber', 'querySchemaVersionMetadata_schemaVersionNumber' - The version number of the schema.
newQuerySchemaVersionMetadata ::
  QuerySchemaVersionMetadata
newQuerySchemaVersionMetadata =
  QuerySchemaVersionMetadata'
    { maxResults =
        Prelude.Nothing,
      metadataList = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaId = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      schemaVersionNumber = Prelude.Nothing
    }

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
querySchemaVersionMetadata_maxResults :: Lens.Lens' QuerySchemaVersionMetadata (Prelude.Maybe Prelude.Natural)
querySchemaVersionMetadata_maxResults = Lens.lens (\QuerySchemaVersionMetadata' {maxResults} -> maxResults) (\s@QuerySchemaVersionMetadata' {} a -> s {maxResults = a} :: QuerySchemaVersionMetadata)

-- | Search key-value pairs for metadata, if they are not provided all the
-- metadata information will be fetched.
querySchemaVersionMetadata_metadataList :: Lens.Lens' QuerySchemaVersionMetadata (Prelude.Maybe [MetadataKeyValuePair])
querySchemaVersionMetadata_metadataList = Lens.lens (\QuerySchemaVersionMetadata' {metadataList} -> metadataList) (\s@QuerySchemaVersionMetadata' {} a -> s {metadataList = a} :: QuerySchemaVersionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if this is a continuation call.
querySchemaVersionMetadata_nextToken :: Lens.Lens' QuerySchemaVersionMetadata (Prelude.Maybe Prelude.Text)
querySchemaVersionMetadata_nextToken = Lens.lens (\QuerySchemaVersionMetadata' {nextToken} -> nextToken) (\s@QuerySchemaVersionMetadata' {} a -> s {nextToken = a} :: QuerySchemaVersionMetadata)

-- | A wrapper structure that may contain the schema name and Amazon Resource
-- Name (ARN).
querySchemaVersionMetadata_schemaId :: Lens.Lens' QuerySchemaVersionMetadata (Prelude.Maybe SchemaId)
querySchemaVersionMetadata_schemaId = Lens.lens (\QuerySchemaVersionMetadata' {schemaId} -> schemaId) (\s@QuerySchemaVersionMetadata' {} a -> s {schemaId = a} :: QuerySchemaVersionMetadata)

-- | The unique version ID of the schema version.
querySchemaVersionMetadata_schemaVersionId :: Lens.Lens' QuerySchemaVersionMetadata (Prelude.Maybe Prelude.Text)
querySchemaVersionMetadata_schemaVersionId = Lens.lens (\QuerySchemaVersionMetadata' {schemaVersionId} -> schemaVersionId) (\s@QuerySchemaVersionMetadata' {} a -> s {schemaVersionId = a} :: QuerySchemaVersionMetadata)

-- | The version number of the schema.
querySchemaVersionMetadata_schemaVersionNumber :: Lens.Lens' QuerySchemaVersionMetadata (Prelude.Maybe SchemaVersionNumber)
querySchemaVersionMetadata_schemaVersionNumber = Lens.lens (\QuerySchemaVersionMetadata' {schemaVersionNumber} -> schemaVersionNumber) (\s@QuerySchemaVersionMetadata' {} a -> s {schemaVersionNumber = a} :: QuerySchemaVersionMetadata)

instance Core.AWSRequest QuerySchemaVersionMetadata where
  type
    AWSResponse QuerySchemaVersionMetadata =
      QuerySchemaVersionMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QuerySchemaVersionMetadataResponse'
            Prelude.<$> ( x
                            Data..?> "MetadataInfoMap"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SchemaVersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable QuerySchemaVersionMetadata where
  hashWithSalt _salt QuerySchemaVersionMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` metadataList
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` schemaId
      `Prelude.hashWithSalt` schemaVersionId
      `Prelude.hashWithSalt` schemaVersionNumber

instance Prelude.NFData QuerySchemaVersionMetadata where
  rnf QuerySchemaVersionMetadata' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf metadataList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaId
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf schemaVersionNumber

instance Data.ToHeaders QuerySchemaVersionMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.QuerySchemaVersionMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON QuerySchemaVersionMetadata where
  toJSON QuerySchemaVersionMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MetadataList" Data..=) Prelude.<$> metadataList,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SchemaId" Data..=) Prelude.<$> schemaId,
            ("SchemaVersionId" Data..=)
              Prelude.<$> schemaVersionId,
            ("SchemaVersionNumber" Data..=)
              Prelude.<$> schemaVersionNumber
          ]
      )

instance Data.ToPath QuerySchemaVersionMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery QuerySchemaVersionMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQuerySchemaVersionMetadataResponse' smart constructor.
data QuerySchemaVersionMetadataResponse = QuerySchemaVersionMetadataResponse'
  { -- | A map of a metadata key and associated values.
    metadataInfoMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataInfo),
    -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  QuerySchemaVersionMetadataResponse
newQuerySchemaVersionMetadataResponse pHttpStatus_ =
  QuerySchemaVersionMetadataResponse'
    { metadataInfoMap =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map of a metadata key and associated values.
querySchemaVersionMetadataResponse_metadataInfoMap :: Lens.Lens' QuerySchemaVersionMetadataResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataInfo))
querySchemaVersionMetadataResponse_metadataInfoMap = Lens.lens (\QuerySchemaVersionMetadataResponse' {metadataInfoMap} -> metadataInfoMap) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {metadataInfoMap = a} :: QuerySchemaVersionMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
querySchemaVersionMetadataResponse_nextToken :: Lens.Lens' QuerySchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
querySchemaVersionMetadataResponse_nextToken = Lens.lens (\QuerySchemaVersionMetadataResponse' {nextToken} -> nextToken) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {nextToken = a} :: QuerySchemaVersionMetadataResponse)

-- | The unique version ID of the schema version.
querySchemaVersionMetadataResponse_schemaVersionId :: Lens.Lens' QuerySchemaVersionMetadataResponse (Prelude.Maybe Prelude.Text)
querySchemaVersionMetadataResponse_schemaVersionId = Lens.lens (\QuerySchemaVersionMetadataResponse' {schemaVersionId} -> schemaVersionId) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {schemaVersionId = a} :: QuerySchemaVersionMetadataResponse)

-- | The response's http status code.
querySchemaVersionMetadataResponse_httpStatus :: Lens.Lens' QuerySchemaVersionMetadataResponse Prelude.Int
querySchemaVersionMetadataResponse_httpStatus = Lens.lens (\QuerySchemaVersionMetadataResponse' {httpStatus} -> httpStatus) (\s@QuerySchemaVersionMetadataResponse' {} a -> s {httpStatus = a} :: QuerySchemaVersionMetadataResponse)

instance
  Prelude.NFData
    QuerySchemaVersionMetadataResponse
  where
  rnf QuerySchemaVersionMetadataResponse' {..} =
    Prelude.rnf metadataInfoMap
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf httpStatus
