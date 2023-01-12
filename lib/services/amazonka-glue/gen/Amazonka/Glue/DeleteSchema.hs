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
-- Module      : Amazonka.Glue.DeleteSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the entire schema set, including the schema set and all of its
-- versions. To get the status of the delete operation, you can call
-- @GetSchema@ API after the asynchronous call. Deleting a registry will
-- deactivate all online operations for the schema, such as the
-- @GetSchemaByDefinition@, and @RegisterSchemaVersion@ APIs.
module Amazonka.Glue.DeleteSchema
  ( -- * Creating a Request
    DeleteSchema (..),
    newDeleteSchema,

    -- * Request Lenses
    deleteSchema_schemaId,

    -- * Destructuring the Response
    DeleteSchemaResponse (..),
    newDeleteSchemaResponse,

    -- * Response Lenses
    deleteSchemaResponse_schemaArn,
    deleteSchemaResponse_schemaName,
    deleteSchemaResponse_status,
    deleteSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSchema' smart constructor.
data DeleteSchema = DeleteSchema'
  { -- | This is a wrapper structure that may contain the schema name and Amazon
    -- Resource Name (ARN).
    schemaId :: SchemaId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaId', 'deleteSchema_schemaId' - This is a wrapper structure that may contain the schema name and Amazon
-- Resource Name (ARN).
newDeleteSchema ::
  -- | 'schemaId'
  SchemaId ->
  DeleteSchema
newDeleteSchema pSchemaId_ =
  DeleteSchema' {schemaId = pSchemaId_}

-- | This is a wrapper structure that may contain the schema name and Amazon
-- Resource Name (ARN).
deleteSchema_schemaId :: Lens.Lens' DeleteSchema SchemaId
deleteSchema_schemaId = Lens.lens (\DeleteSchema' {schemaId} -> schemaId) (\s@DeleteSchema' {} a -> s {schemaId = a} :: DeleteSchema)

instance Core.AWSRequest DeleteSchema where
  type AWSResponse DeleteSchema = DeleteSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSchemaResponse'
            Prelude.<$> (x Data..?> "SchemaArn")
            Prelude.<*> (x Data..?> "SchemaName")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSchema where
  hashWithSalt _salt DeleteSchema' {..} =
    _salt `Prelude.hashWithSalt` schemaId

instance Prelude.NFData DeleteSchema where
  rnf DeleteSchema' {..} = Prelude.rnf schemaId

instance Data.ToHeaders DeleteSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteSchema" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSchema where
  toJSON DeleteSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SchemaId" Data..= schemaId)]
      )

instance Data.ToPath DeleteSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { -- | The Amazon Resource Name (ARN) of the schema being deleted.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema being deleted.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The status of the schema.
    status :: Prelude.Maybe SchemaStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'deleteSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema being deleted.
--
-- 'schemaName', 'deleteSchemaResponse_schemaName' - The name of the schema being deleted.
--
-- 'status', 'deleteSchemaResponse_status' - The status of the schema.
--
-- 'httpStatus', 'deleteSchemaResponse_httpStatus' - The response's http status code.
newDeleteSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSchemaResponse
newDeleteSchemaResponse pHttpStatus_ =
  DeleteSchemaResponse'
    { schemaArn = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema being deleted.
deleteSchemaResponse_schemaArn :: Lens.Lens' DeleteSchemaResponse (Prelude.Maybe Prelude.Text)
deleteSchemaResponse_schemaArn = Lens.lens (\DeleteSchemaResponse' {schemaArn} -> schemaArn) (\s@DeleteSchemaResponse' {} a -> s {schemaArn = a} :: DeleteSchemaResponse)

-- | The name of the schema being deleted.
deleteSchemaResponse_schemaName :: Lens.Lens' DeleteSchemaResponse (Prelude.Maybe Prelude.Text)
deleteSchemaResponse_schemaName = Lens.lens (\DeleteSchemaResponse' {schemaName} -> schemaName) (\s@DeleteSchemaResponse' {} a -> s {schemaName = a} :: DeleteSchemaResponse)

-- | The status of the schema.
deleteSchemaResponse_status :: Lens.Lens' DeleteSchemaResponse (Prelude.Maybe SchemaStatus)
deleteSchemaResponse_status = Lens.lens (\DeleteSchemaResponse' {status} -> status) (\s@DeleteSchemaResponse' {} a -> s {status = a} :: DeleteSchemaResponse)

-- | The response's http status code.
deleteSchemaResponse_httpStatus :: Lens.Lens' DeleteSchemaResponse Prelude.Int
deleteSchemaResponse_httpStatus = Lens.lens (\DeleteSchemaResponse' {httpStatus} -> httpStatus) (\s@DeleteSchemaResponse' {} a -> s {httpStatus = a} :: DeleteSchemaResponse)

instance Prelude.NFData DeleteSchemaResponse where
  rnf DeleteSchemaResponse' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
