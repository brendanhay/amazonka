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
-- Module      : Amazonka.Glue.UpdateSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description, compatibility setting, or version checkpoint
-- for a schema set.
--
-- For updating the compatibility setting, the call will not validate
-- compatibility for the entire set of schema versions with the new
-- compatibility setting. If the value for @Compatibility@ is provided, the
-- @VersionNumber@ (a checkpoint) is also required. The API will validate
-- the checkpoint version number for consistency.
--
-- If the value for the @VersionNumber@ (checkpoint) is provided,
-- @Compatibility@ is optional and this can be used to set\/reset a
-- checkpoint for the schema.
--
-- This update will happen only if the schema is in the AVAILABLE state.
module Amazonka.Glue.UpdateSchema
  ( -- * Creating a Request
    UpdateSchema (..),
    newUpdateSchema,

    -- * Request Lenses
    updateSchema_compatibility,
    updateSchema_description,
    updateSchema_schemaVersionNumber,
    updateSchema_schemaId,

    -- * Destructuring the Response
    UpdateSchemaResponse (..),
    newUpdateSchemaResponse,

    -- * Response Lenses
    updateSchemaResponse_registryName,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | The new compatibility setting for the schema.
    compatibility :: Prelude.Maybe Compatibility,
    -- | The new description for the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | Version number required for check pointing. One of @VersionNumber@ or
    -- @Compatibility@ has to be provided.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber,
    -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
    --     @SchemaName@ has to be provided.
    schemaId :: SchemaId
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
-- 'compatibility', 'updateSchema_compatibility' - The new compatibility setting for the schema.
--
-- 'description', 'updateSchema_description' - The new description for the schema.
--
-- 'schemaVersionNumber', 'updateSchema_schemaVersionNumber' - Version number required for check pointing. One of @VersionNumber@ or
-- @Compatibility@ has to be provided.
--
-- 'schemaId', 'updateSchema_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
--     @SchemaName@ has to be provided.
newUpdateSchema ::
  -- | 'schemaId'
  SchemaId ->
  UpdateSchema
newUpdateSchema pSchemaId_ =
  UpdateSchema'
    { compatibility = Prelude.Nothing,
      description = Prelude.Nothing,
      schemaVersionNumber = Prelude.Nothing,
      schemaId = pSchemaId_
    }

-- | The new compatibility setting for the schema.
updateSchema_compatibility :: Lens.Lens' UpdateSchema (Prelude.Maybe Compatibility)
updateSchema_compatibility = Lens.lens (\UpdateSchema' {compatibility} -> compatibility) (\s@UpdateSchema' {} a -> s {compatibility = a} :: UpdateSchema)

-- | The new description for the schema.
updateSchema_description :: Lens.Lens' UpdateSchema (Prelude.Maybe Prelude.Text)
updateSchema_description = Lens.lens (\UpdateSchema' {description} -> description) (\s@UpdateSchema' {} a -> s {description = a} :: UpdateSchema)

-- | Version number required for check pointing. One of @VersionNumber@ or
-- @Compatibility@ has to be provided.
updateSchema_schemaVersionNumber :: Lens.Lens' UpdateSchema (Prelude.Maybe SchemaVersionNumber)
updateSchema_schemaVersionNumber = Lens.lens (\UpdateSchema' {schemaVersionNumber} -> schemaVersionNumber) (\s@UpdateSchema' {} a -> s {schemaVersionNumber = a} :: UpdateSchema)

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     One of @SchemaArn@ or @SchemaName@ has to be provided.
--
-- -   SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or
--     @SchemaName@ has to be provided.
updateSchema_schemaId :: Lens.Lens' UpdateSchema SchemaId
updateSchema_schemaId = Lens.lens (\UpdateSchema' {schemaId} -> schemaId) (\s@UpdateSchema' {} a -> s {schemaId = a} :: UpdateSchema)

instance Core.AWSRequest UpdateSchema where
  type AWSResponse UpdateSchema = UpdateSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Prelude.<$> (x Data..?> "RegistryName")
            Prelude.<*> (x Data..?> "SchemaName")
            Prelude.<*> (x Data..?> "SchemaArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSchema where
  hashWithSalt _salt UpdateSchema' {..} =
    _salt `Prelude.hashWithSalt` compatibility
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` schemaVersionNumber
      `Prelude.hashWithSalt` schemaId

instance Prelude.NFData UpdateSchema where
  rnf UpdateSchema' {..} =
    Prelude.rnf compatibility
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf schemaVersionNumber
      `Prelude.seq` Prelude.rnf schemaId

instance Data.ToHeaders UpdateSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.UpdateSchema" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Compatibility" Data..=) Prelude.<$> compatibility,
            ("Description" Data..=) Prelude.<$> description,
            ("SchemaVersionNumber" Data..=)
              Prelude.<$> schemaVersionNumber,
            Prelude.Just ("SchemaId" Data..= schemaId)
          ]
      )

instance Data.ToPath UpdateSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The name of the registry that contains the schema.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
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
-- 'registryName', 'updateSchemaResponse_registryName' - The name of the registry that contains the schema.
--
-- 'schemaName', 'updateSchemaResponse_schemaName' - The name of the schema.
--
-- 'schemaArn', 'updateSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'httpStatus', 'updateSchemaResponse_httpStatus' - The response's http status code.
newUpdateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSchemaResponse
newUpdateSchemaResponse pHttpStatus_ =
  UpdateSchemaResponse'
    { registryName =
        Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the registry that contains the schema.
updateSchemaResponse_registryName :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_registryName = Lens.lens (\UpdateSchemaResponse' {registryName} -> registryName) (\s@UpdateSchemaResponse' {} a -> s {registryName = a} :: UpdateSchemaResponse)

-- | The name of the schema.
updateSchemaResponse_schemaName :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaName = Lens.lens (\UpdateSchemaResponse' {schemaName} -> schemaName) (\s@UpdateSchemaResponse' {} a -> s {schemaName = a} :: UpdateSchemaResponse)

-- | The Amazon Resource Name (ARN) of the schema.
updateSchemaResponse_schemaArn :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaArn = Lens.lens (\UpdateSchemaResponse' {schemaArn} -> schemaArn) (\s@UpdateSchemaResponse' {} a -> s {schemaArn = a} :: UpdateSchemaResponse)

-- | The response's http status code.
updateSchemaResponse_httpStatus :: Lens.Lens' UpdateSchemaResponse Prelude.Int
updateSchemaResponse_httpStatus = Lens.lens (\UpdateSchemaResponse' {httpStatus} -> httpStatus) (\s@UpdateSchemaResponse' {} a -> s {httpStatus = a} :: UpdateSchemaResponse)

instance Prelude.NFData UpdateSchemaResponse where
  rnf UpdateSchemaResponse' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf httpStatus
