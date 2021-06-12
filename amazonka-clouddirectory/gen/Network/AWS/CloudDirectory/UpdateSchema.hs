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
-- Module      : Network.AWS.CloudDirectory.UpdateSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schema name with a new name. Only development schema names
-- can be updated.
module Network.AWS.CloudDirectory.UpdateSchema
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | The Amazon Resource Name (ARN) of the development schema. For more
    -- information, see arns.
    schemaArn :: Core.Text,
    -- | The name of the schema.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'name'
  Core.Text ->
  UpdateSchema
newUpdateSchema pSchemaArn_ pName_ =
  UpdateSchema'
    { schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the development schema. For more
-- information, see arns.
updateSchema_schemaArn :: Lens.Lens' UpdateSchema Core.Text
updateSchema_schemaArn = Lens.lens (\UpdateSchema' {schemaArn} -> schemaArn) (\s@UpdateSchema' {} a -> s {schemaArn = a} :: UpdateSchema)

-- | The name of the schema.
updateSchema_name :: Lens.Lens' UpdateSchema Core.Text
updateSchema_name = Lens.lens (\UpdateSchema' {name} -> name) (\s@UpdateSchema' {} a -> s {name = a} :: UpdateSchema)

instance Core.AWSRequest UpdateSchema where
  type AWSResponse UpdateSchema = UpdateSchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Core.<$> (x Core..?> "SchemaArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSchema

instance Core.NFData UpdateSchema

instance Core.ToHeaders UpdateSchema where
  toHeaders UpdateSchema' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath UpdateSchema where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/schema/update"

instance Core.ToQuery UpdateSchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The ARN that is associated with the updated schema. For more
    -- information, see arns.
    schemaArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateSchemaResponse
newUpdateSchemaResponse pHttpStatus_ =
  UpdateSchemaResponse'
    { schemaArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN that is associated with the updated schema. For more
-- information, see arns.
updateSchemaResponse_schemaArn :: Lens.Lens' UpdateSchemaResponse (Core.Maybe Core.Text)
updateSchemaResponse_schemaArn = Lens.lens (\UpdateSchemaResponse' {schemaArn} -> schemaArn) (\s@UpdateSchemaResponse' {} a -> s {schemaArn = a} :: UpdateSchemaResponse)

-- | The response's http status code.
updateSchemaResponse_httpStatus :: Lens.Lens' UpdateSchemaResponse Core.Int
updateSchemaResponse_httpStatus = Lens.lens (\UpdateSchemaResponse' {httpStatus} -> httpStatus) (\s@UpdateSchemaResponse' {} a -> s {httpStatus = a} :: UpdateSchemaResponse)

instance Core.NFData UpdateSchemaResponse
