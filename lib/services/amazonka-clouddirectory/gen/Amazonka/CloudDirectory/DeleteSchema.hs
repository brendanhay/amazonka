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
-- Module      : Amazonka.CloudDirectory.DeleteSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given schema. Schemas in a development and published state can
-- only be deleted.
module Amazonka.CloudDirectory.DeleteSchema
  ( -- * Creating a Request
    DeleteSchema (..),
    newDeleteSchema,

    -- * Request Lenses
    deleteSchema_schemaArn,

    -- * Destructuring the Response
    DeleteSchemaResponse (..),
    newDeleteSchemaResponse,

    -- * Response Lenses
    deleteSchemaResponse_schemaArn,
    deleteSchemaResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSchema' smart constructor.
data DeleteSchema = DeleteSchema'
  { -- | The Amazon Resource Name (ARN) of the development schema. For more
    -- information, see arns.
    schemaArn :: Prelude.Text
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
-- 'schemaArn', 'deleteSchema_schemaArn' - The Amazon Resource Name (ARN) of the development schema. For more
-- information, see arns.
newDeleteSchema ::
  -- | 'schemaArn'
  Prelude.Text ->
  DeleteSchema
newDeleteSchema pSchemaArn_ =
  DeleteSchema' {schemaArn = pSchemaArn_}

-- | The Amazon Resource Name (ARN) of the development schema. For more
-- information, see arns.
deleteSchema_schemaArn :: Lens.Lens' DeleteSchema Prelude.Text
deleteSchema_schemaArn = Lens.lens (\DeleteSchema' {schemaArn} -> schemaArn) (\s@DeleteSchema' {} a -> s {schemaArn = a} :: DeleteSchema)

instance Core.AWSRequest DeleteSchema where
  type AWSResponse DeleteSchema = DeleteSchemaResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSchemaResponse'
            Prelude.<$> (x Core..?> "SchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSchema where
  hashWithSalt _salt DeleteSchema' {..} =
    _salt `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData DeleteSchema where
  rnf DeleteSchema' {..} = Prelude.rnf schemaArn

instance Core.ToHeaders DeleteSchema where
  toHeaders DeleteSchema' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON DeleteSchema where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DeleteSchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema"

instance Core.ToQuery DeleteSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { -- | The input ARN that is returned as part of the response. For more
    -- information, see arns.
    schemaArn :: Prelude.Maybe Prelude.Text,
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
-- 'schemaArn', 'deleteSchemaResponse_schemaArn' - The input ARN that is returned as part of the response. For more
-- information, see arns.
--
-- 'httpStatus', 'deleteSchemaResponse_httpStatus' - The response's http status code.
newDeleteSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSchemaResponse
newDeleteSchemaResponse pHttpStatus_ =
  DeleteSchemaResponse'
    { schemaArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The input ARN that is returned as part of the response. For more
-- information, see arns.
deleteSchemaResponse_schemaArn :: Lens.Lens' DeleteSchemaResponse (Prelude.Maybe Prelude.Text)
deleteSchemaResponse_schemaArn = Lens.lens (\DeleteSchemaResponse' {schemaArn} -> schemaArn) (\s@DeleteSchemaResponse' {} a -> s {schemaArn = a} :: DeleteSchemaResponse)

-- | The response's http status code.
deleteSchemaResponse_httpStatus :: Lens.Lens' DeleteSchemaResponse Prelude.Int
deleteSchemaResponse_httpStatus = Lens.lens (\DeleteSchemaResponse' {httpStatus} -> httpStatus) (\s@DeleteSchemaResponse' {} a -> s {httpStatus = a} :: DeleteSchemaResponse)

instance Prelude.NFData DeleteSchemaResponse where
  rnf DeleteSchemaResponse' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf httpStatus
