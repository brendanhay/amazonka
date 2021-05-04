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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetSchemaVersionsDiff where
  type
    Rs GetSchemaVersionsDiff =
      GetSchemaVersionsDiffResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaVersionsDiffResponse'
            Prelude.<$> (x Prelude..?> "Diff")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSchemaVersionsDiff

instance Prelude.NFData GetSchemaVersionsDiff

instance Prelude.ToHeaders GetSchemaVersionsDiff where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.GetSchemaVersionsDiff" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetSchemaVersionsDiff where
  toJSON GetSchemaVersionsDiff' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaId" Prelude..= schemaId),
            Prelude.Just
              ( "FirstSchemaVersionNumber"
                  Prelude..= firstSchemaVersionNumber
              ),
            Prelude.Just
              ( "SecondSchemaVersionNumber"
                  Prelude..= secondSchemaVersionNumber
              ),
            Prelude.Just
              ("SchemaDiffType" Prelude..= schemaDiffType)
          ]
      )

instance Prelude.ToPath GetSchemaVersionsDiff where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetSchemaVersionsDiff where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaVersionsDiffResponse' smart constructor.
data GetSchemaVersionsDiffResponse = GetSchemaVersionsDiffResponse'
  { -- | The difference between schemas as a string in JsonPatch format.
    diff :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetSchemaVersionsDiffResponse
newGetSchemaVersionsDiffResponse pHttpStatus_ =
  GetSchemaVersionsDiffResponse'
    { diff =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The difference between schemas as a string in JsonPatch format.
getSchemaVersionsDiffResponse_diff :: Lens.Lens' GetSchemaVersionsDiffResponse (Prelude.Maybe Prelude.Text)
getSchemaVersionsDiffResponse_diff = Lens.lens (\GetSchemaVersionsDiffResponse' {diff} -> diff) (\s@GetSchemaVersionsDiffResponse' {} a -> s {diff = a} :: GetSchemaVersionsDiffResponse)

-- | The response's http status code.
getSchemaVersionsDiffResponse_httpStatus :: Lens.Lens' GetSchemaVersionsDiffResponse Prelude.Int
getSchemaVersionsDiffResponse_httpStatus = Lens.lens (\GetSchemaVersionsDiffResponse' {httpStatus} -> httpStatus) (\s@GetSchemaVersionsDiffResponse' {} a -> s {httpStatus = a} :: GetSchemaVersionsDiffResponse)

instance Prelude.NFData GetSchemaVersionsDiffResponse
