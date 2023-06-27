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
-- Module      : Amazonka.CleanRooms.BatchGetSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves multiple schemas by their identifiers.
module Amazonka.CleanRooms.BatchGetSchema
  ( -- * Creating a Request
    BatchGetSchema (..),
    newBatchGetSchema,

    -- * Request Lenses
    batchGetSchema_collaborationIdentifier,
    batchGetSchema_names,

    -- * Destructuring the Response
    BatchGetSchemaResponse (..),
    newBatchGetSchemaResponse,

    -- * Response Lenses
    batchGetSchemaResponse_httpStatus,
    batchGetSchemaResponse_schemas,
    batchGetSchemaResponse_errors,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetSchema' smart constructor.
data BatchGetSchema = BatchGetSchema'
  { -- | A unique identifier for the collaboration that the schemas belong to.
    -- Currently accepts collaboration ID.
    collaborationIdentifier :: Prelude.Text,
    -- | The names for the schema objects to retrieve.>
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationIdentifier', 'batchGetSchema_collaborationIdentifier' - A unique identifier for the collaboration that the schemas belong to.
-- Currently accepts collaboration ID.
--
-- 'names', 'batchGetSchema_names' - The names for the schema objects to retrieve.>
newBatchGetSchema ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetSchema
newBatchGetSchema pCollaborationIdentifier_ pNames_ =
  BatchGetSchema'
    { collaborationIdentifier =
        pCollaborationIdentifier_,
      names = Lens.coerced Lens.# pNames_
    }

-- | A unique identifier for the collaboration that the schemas belong to.
-- Currently accepts collaboration ID.
batchGetSchema_collaborationIdentifier :: Lens.Lens' BatchGetSchema Prelude.Text
batchGetSchema_collaborationIdentifier = Lens.lens (\BatchGetSchema' {collaborationIdentifier} -> collaborationIdentifier) (\s@BatchGetSchema' {} a -> s {collaborationIdentifier = a} :: BatchGetSchema)

-- | The names for the schema objects to retrieve.>
batchGetSchema_names :: Lens.Lens' BatchGetSchema (Prelude.NonEmpty Prelude.Text)
batchGetSchema_names = Lens.lens (\BatchGetSchema' {names} -> names) (\s@BatchGetSchema' {} a -> s {names = a} :: BatchGetSchema) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetSchema where
  type
    AWSResponse BatchGetSchema =
      BatchGetSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetSchemaResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "schemas" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchGetSchema where
  hashWithSalt _salt BatchGetSchema' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationIdentifier
      `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetSchema where
  rnf BatchGetSchema' {..} =
    Prelude.rnf collaborationIdentifier
      `Prelude.seq` Prelude.rnf names

instance Data.ToHeaders BatchGetSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetSchema where
  toJSON BatchGetSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("names" Data..= names)]
      )

instance Data.ToPath BatchGetSchema where
  toPath BatchGetSchema' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier,
        "/batch-schema"
      ]

instance Data.ToQuery BatchGetSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetSchemaResponse' smart constructor.
data BatchGetSchemaResponse = BatchGetSchemaResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The retrieved list of schemas.
    schemas :: [Schema],
    -- | Error reasons for schemas that could not be retrieved. One error is
    -- returned for every schema that could not be retrieved.
    errors :: [BatchGetSchemaError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'batchGetSchemaResponse_httpStatus' - The response's http status code.
--
-- 'schemas', 'batchGetSchemaResponse_schemas' - The retrieved list of schemas.
--
-- 'errors', 'batchGetSchemaResponse_errors' - Error reasons for schemas that could not be retrieved. One error is
-- returned for every schema that could not be retrieved.
newBatchGetSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetSchemaResponse
newBatchGetSchemaResponse pHttpStatus_ =
  BatchGetSchemaResponse'
    { httpStatus = pHttpStatus_,
      schemas = Prelude.mempty,
      errors = Prelude.mempty
    }

-- | The response's http status code.
batchGetSchemaResponse_httpStatus :: Lens.Lens' BatchGetSchemaResponse Prelude.Int
batchGetSchemaResponse_httpStatus = Lens.lens (\BatchGetSchemaResponse' {httpStatus} -> httpStatus) (\s@BatchGetSchemaResponse' {} a -> s {httpStatus = a} :: BatchGetSchemaResponse)

-- | The retrieved list of schemas.
batchGetSchemaResponse_schemas :: Lens.Lens' BatchGetSchemaResponse [Schema]
batchGetSchemaResponse_schemas = Lens.lens (\BatchGetSchemaResponse' {schemas} -> schemas) (\s@BatchGetSchemaResponse' {} a -> s {schemas = a} :: BatchGetSchemaResponse) Prelude.. Lens.coerced

-- | Error reasons for schemas that could not be retrieved. One error is
-- returned for every schema that could not be retrieved.
batchGetSchemaResponse_errors :: Lens.Lens' BatchGetSchemaResponse [BatchGetSchemaError]
batchGetSchemaResponse_errors = Lens.lens (\BatchGetSchemaResponse' {errors} -> errors) (\s@BatchGetSchemaResponse' {} a -> s {errors = a} :: BatchGetSchemaResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchGetSchemaResponse where
  rnf BatchGetSchemaResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf schemas
      `Prelude.seq` Prelude.rnf errors
