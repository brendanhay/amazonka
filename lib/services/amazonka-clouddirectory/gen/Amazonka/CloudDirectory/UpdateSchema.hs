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
-- Module      : Amazonka.CloudDirectory.UpdateSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schema name with a new name. Only development schema names
-- can be updated.
module Amazonka.CloudDirectory.UpdateSchema
  ( -- * Creating a Request
    UpdateSchema (..),
    newUpdateSchema,

    -- * Request Lenses
    updateSchema_schemaArn,
    updateSchema_name,

    -- * Destructuring the Response
    UpdateSchemaResponse (..),
    newUpdateSchemaResponse,

    -- * Response Lenses
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | The Amazon Resource Name (ARN) of the development schema. For more
    -- information, see arns.
    schemaArn :: Prelude.Text,
    -- | The name of the schema.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'updateSchema_schemaArn' - The Amazon Resource Name (ARN) of the development schema. For more
-- information, see arns.
--
-- 'name', 'updateSchema_name' - The name of the schema.
newUpdateSchema ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateSchema
newUpdateSchema pSchemaArn_ pName_ =
  UpdateSchema'
    { schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the development schema. For more
-- information, see arns.
updateSchema_schemaArn :: Lens.Lens' UpdateSchema Prelude.Text
updateSchema_schemaArn = Lens.lens (\UpdateSchema' {schemaArn} -> schemaArn) (\s@UpdateSchema' {} a -> s {schemaArn = a} :: UpdateSchema)

-- | The name of the schema.
updateSchema_name :: Lens.Lens' UpdateSchema Prelude.Text
updateSchema_name = Lens.lens (\UpdateSchema' {name} -> name) (\s@UpdateSchema' {} a -> s {name = a} :: UpdateSchema)

instance Core.AWSRequest UpdateSchema where
  type AWSResponse UpdateSchema = UpdateSchemaResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Prelude.<$> (x Core..?> "SchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSchema where
  hashWithSalt _salt UpdateSchema' {..} =
    _salt `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateSchema where
  rnf UpdateSchema' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateSchema where
  toHeaders UpdateSchema' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath UpdateSchema where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/schema/update"

instance Core.ToQuery UpdateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The ARN that is associated with the updated schema. For more
    -- information, see arns.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'updateSchemaResponse_schemaArn' - The ARN that is associated with the updated schema. For more
-- information, see arns.
--
-- 'httpStatus', 'updateSchemaResponse_httpStatus' - The response's http status code.
newUpdateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSchemaResponse
newUpdateSchemaResponse pHttpStatus_ =
  UpdateSchemaResponse'
    { schemaArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that is associated with the updated schema. For more
-- information, see arns.
updateSchemaResponse_schemaArn :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaArn = Lens.lens (\UpdateSchemaResponse' {schemaArn} -> schemaArn) (\s@UpdateSchemaResponse' {} a -> s {schemaArn = a} :: UpdateSchemaResponse)

-- | The response's http status code.
updateSchemaResponse_httpStatus :: Lens.Lens' UpdateSchemaResponse Prelude.Int
updateSchemaResponse_httpStatus = Lens.lens (\UpdateSchemaResponse' {httpStatus} -> httpStatus) (\s@UpdateSchemaResponse' {} a -> s {httpStatus = a} :: UpdateSchemaResponse)

instance Prelude.NFData UpdateSchemaResponse where
  rnf UpdateSchemaResponse' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf httpStatus
