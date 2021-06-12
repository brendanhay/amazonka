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
-- Module      : Network.AWS.Glue.GetSchemaVersionsDiff
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the schema version difference in the specified difference type
-- between two stored schema versions in the Schema Registry.
--
-- This API allows you to compare two schema versions between two schema
-- definitions under the same schema.
module Network.AWS.Glue.GetSchemaVersionsDiff
  ( -- * Creating a Request
    GetSchemaVersionsDiff (..),
    newGetSchemaVersionsDiff,

    -- * Request Lenses
    getSchemaVersionsDiff_schemaId,
    getSchemaVersionsDiff_firstSchemaVersionNumber,
    getSchemaVersionsDiff_secondSchemaVersionNumber,
    getSchemaVersionsDiff_schemaDiffType,

    -- * Destructuring the Response
    GetSchemaVersionsDiffResponse (..),
    newGetSchemaVersionsDiffResponse,

    -- * Response Lenses
    getSchemaVersionsDiffResponse_diff,
    getSchemaVersionsDiffResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSchemaVersionsDiff' smart constructor.
data GetSchemaVersionsDiff = GetSchemaVersionsDiff'
  { -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
    --     @SchemaName@ has to be provided.
    schemaId :: SchemaId,
    -- | The first of the two schema versions to be compared.
    firstSchemaVersionNumber :: SchemaVersionNumber,
    -- | The second of the two schema versions to be compared.
    secondSchemaVersionNumber :: SchemaVersionNumber,
    -- | Refers to @SYNTAX_DIFF@, which is the currently supported diff type.
    schemaDiffType :: SchemaDiffType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaVersionsDiff' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaId', 'getSchemaVersionsDiff_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
--     @SchemaName@ has to be provided.
--
-- 'firstSchemaVersionNumber', 'getSchemaVersionsDiff_firstSchemaVersionNumber' - The first of the two schema versions to be compared.
--
-- 'secondSchemaVersionNumber', 'getSchemaVersionsDiff_secondSchemaVersionNumber' - The second of the two schema versions to be compared.
--
-- 'schemaDiffType', 'getSchemaVersionsDiff_schemaDiffType' - Refers to @SYNTAX_DIFF@, which is the currently supported diff type.
newGetSchemaVersionsDiff ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'firstSchemaVersionNumber'
  SchemaVersionNumber ->
  -- | 'secondSchemaVersionNumber'
  SchemaVersionNumber ->
  -- | 'schemaDiffType'
  SchemaDiffType ->
  GetSchemaVersionsDiff
newGetSchemaVersionsDiff
  pSchemaId_
  pFirstSchemaVersionNumber_
  pSecondSchemaVersionNumber_
  pSchemaDiffType_ =
    GetSchemaVersionsDiff'
      { schemaId = pSchemaId_,
        firstSchemaVersionNumber =
          pFirstSchemaVersionNumber_,
        secondSchemaVersionNumber =
          pSecondSchemaVersionNumber_,
        schemaDiffType = pSchemaDiffType_
      }

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
--     @SchemaName@ has to be provided.
getSchemaVersionsDiff_schemaId :: Lens.Lens' GetSchemaVersionsDiff SchemaId
getSchemaVersionsDiff_schemaId = Lens.lens (\GetSchemaVersionsDiff' {schemaId} -> schemaId) (\s@GetSchemaVersionsDiff' {} a -> s {schemaId = a} :: GetSchemaVersionsDiff)

-- | The first of the two schema versions to be compared.
getSchemaVersionsDiff_firstSchemaVersionNumber :: Lens.Lens' GetSchemaVersionsDiff SchemaVersionNumber
getSchemaVersionsDiff_firstSchemaVersionNumber = Lens.lens (\GetSchemaVersionsDiff' {firstSchemaVersionNumber} -> firstSchemaVersionNumber) (\s@GetSchemaVersionsDiff' {} a -> s {firstSchemaVersionNumber = a} :: GetSchemaVersionsDiff)

-- | The second of the two schema versions to be compared.
getSchemaVersionsDiff_secondSchemaVersionNumber :: Lens.Lens' GetSchemaVersionsDiff SchemaVersionNumber
getSchemaVersionsDiff_secondSchemaVersionNumber = Lens.lens (\GetSchemaVersionsDiff' {secondSchemaVersionNumber} -> secondSchemaVersionNumber) (\s@GetSchemaVersionsDiff' {} a -> s {secondSchemaVersionNumber = a} :: GetSchemaVersionsDiff)

-- | Refers to @SYNTAX_DIFF@, which is the currently supported diff type.
getSchemaVersionsDiff_schemaDiffType :: Lens.Lens' GetSchemaVersionsDiff SchemaDiffType
getSchemaVersionsDiff_schemaDiffType = Lens.lens (\GetSchemaVersionsDiff' {schemaDiffType} -> schemaDiffType) (\s@GetSchemaVersionsDiff' {} a -> s {schemaDiffType = a} :: GetSchemaVersionsDiff)

instance Core.AWSRequest GetSchemaVersionsDiff where
  type
    AWSResponse GetSchemaVersionsDiff =
      GetSchemaVersionsDiffResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaVersionsDiffResponse'
            Core.<$> (x Core..?> "Diff")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSchemaVersionsDiff

instance Core.NFData GetSchemaVersionsDiff

instance Core.ToHeaders GetSchemaVersionsDiff where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetSchemaVersionsDiff" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSchemaVersionsDiff where
  toJSON GetSchemaVersionsDiff' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaId" Core..= schemaId),
            Core.Just
              ( "FirstSchemaVersionNumber"
                  Core..= firstSchemaVersionNumber
              ),
            Core.Just
              ( "SecondSchemaVersionNumber"
                  Core..= secondSchemaVersionNumber
              ),
            Core.Just ("SchemaDiffType" Core..= schemaDiffType)
          ]
      )

instance Core.ToPath GetSchemaVersionsDiff where
  toPath = Core.const "/"

instance Core.ToQuery GetSchemaVersionsDiff where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSchemaVersionsDiffResponse' smart constructor.
data GetSchemaVersionsDiffResponse = GetSchemaVersionsDiffResponse'
  { -- | The difference between schemas as a string in JsonPatch format.
    diff :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaVersionsDiffResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diff', 'getSchemaVersionsDiffResponse_diff' - The difference between schemas as a string in JsonPatch format.
--
-- 'httpStatus', 'getSchemaVersionsDiffResponse_httpStatus' - The response's http status code.
newGetSchemaVersionsDiffResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSchemaVersionsDiffResponse
newGetSchemaVersionsDiffResponse pHttpStatus_ =
  GetSchemaVersionsDiffResponse'
    { diff = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The difference between schemas as a string in JsonPatch format.
getSchemaVersionsDiffResponse_diff :: Lens.Lens' GetSchemaVersionsDiffResponse (Core.Maybe Core.Text)
getSchemaVersionsDiffResponse_diff = Lens.lens (\GetSchemaVersionsDiffResponse' {diff} -> diff) (\s@GetSchemaVersionsDiffResponse' {} a -> s {diff = a} :: GetSchemaVersionsDiffResponse)

-- | The response's http status code.
getSchemaVersionsDiffResponse_httpStatus :: Lens.Lens' GetSchemaVersionsDiffResponse Core.Int
getSchemaVersionsDiffResponse_httpStatus = Lens.lens (\GetSchemaVersionsDiffResponse' {httpStatus} -> httpStatus) (\s@GetSchemaVersionsDiffResponse' {} a -> s {httpStatus = a} :: GetSchemaVersionsDiffResponse)

instance Core.NFData GetSchemaVersionsDiffResponse
