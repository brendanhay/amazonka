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
-- Module      : Network.AWS.Glue.UpdateSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glue.UpdateSchema
  ( -- * Creating a Request
    UpdateSchema (..),
    newUpdateSchema,

    -- * Request Lenses
    updateSchema_schemaVersionNumber,
    updateSchema_description,
    updateSchema_compatibility,
    updateSchema_schemaId,

    -- * Destructuring the Response
    UpdateSchemaResponse (..),
    newUpdateSchemaResponse,

    -- * Response Lenses
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_registryName,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | Version number required for check pointing. One of @VersionNumber@ or
    -- @Compatibility@ has to be provided.
    schemaVersionNumber :: Prelude.Maybe SchemaVersionNumber,
    -- | The new description for the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new compatibility setting for the schema.
    compatibility :: Prelude.Maybe Compatibility,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionNumber', 'updateSchema_schemaVersionNumber' - Version number required for check pointing. One of @VersionNumber@ or
-- @Compatibility@ has to be provided.
--
-- 'description', 'updateSchema_description' - The new description for the schema.
--
-- 'compatibility', 'updateSchema_compatibility' - The new compatibility setting for the schema.
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
    { schemaVersionNumber =
        Prelude.Nothing,
      description = Prelude.Nothing,
      compatibility = Prelude.Nothing,
      schemaId = pSchemaId_
    }

-- | Version number required for check pointing. One of @VersionNumber@ or
-- @Compatibility@ has to be provided.
updateSchema_schemaVersionNumber :: Lens.Lens' UpdateSchema (Prelude.Maybe SchemaVersionNumber)
updateSchema_schemaVersionNumber = Lens.lens (\UpdateSchema' {schemaVersionNumber} -> schemaVersionNumber) (\s@UpdateSchema' {} a -> s {schemaVersionNumber = a} :: UpdateSchema)

-- | The new description for the schema.
updateSchema_description :: Lens.Lens' UpdateSchema (Prelude.Maybe Prelude.Text)
updateSchema_description = Lens.lens (\UpdateSchema' {description} -> description) (\s@UpdateSchema' {} a -> s {description = a} :: UpdateSchema)

-- | The new compatibility setting for the schema.
updateSchema_compatibility :: Lens.Lens' UpdateSchema (Prelude.Maybe Compatibility)
updateSchema_compatibility = Lens.lens (\UpdateSchema' {compatibility} -> compatibility) (\s@UpdateSchema' {} a -> s {compatibility = a} :: UpdateSchema)

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

instance Prelude.AWSRequest UpdateSchema where
  type Rs UpdateSchema = UpdateSchemaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Prelude.<$> (x Prelude..?> "SchemaArn")
            Prelude.<*> (x Prelude..?> "RegistryName")
            Prelude.<*> (x Prelude..?> "SchemaName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSchema

instance Prelude.NFData UpdateSchema

instance Prelude.ToHeaders UpdateSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.UpdateSchema" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SchemaVersionNumber" Prelude..=)
              Prelude.<$> schemaVersionNumber,
            ("Description" Prelude..=) Prelude.<$> description,
            ("Compatibility" Prelude..=)
              Prelude.<$> compatibility,
            Prelude.Just ("SchemaId" Prelude..= schemaId)
          ]
      )

instance Prelude.ToPath UpdateSchema where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry that contains the schema.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'updateSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'registryName', 'updateSchemaResponse_registryName' - The name of the registry that contains the schema.
--
-- 'schemaName', 'updateSchemaResponse_schemaName' - The name of the schema.
--
-- 'httpStatus', 'updateSchemaResponse_httpStatus' - The response's http status code.
newUpdateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSchemaResponse
newUpdateSchemaResponse pHttpStatus_ =
  UpdateSchemaResponse'
    { schemaArn = Prelude.Nothing,
      registryName = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema.
updateSchemaResponse_schemaArn :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaArn = Lens.lens (\UpdateSchemaResponse' {schemaArn} -> schemaArn) (\s@UpdateSchemaResponse' {} a -> s {schemaArn = a} :: UpdateSchemaResponse)

-- | The name of the registry that contains the schema.
updateSchemaResponse_registryName :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_registryName = Lens.lens (\UpdateSchemaResponse' {registryName} -> registryName) (\s@UpdateSchemaResponse' {} a -> s {registryName = a} :: UpdateSchemaResponse)

-- | The name of the schema.
updateSchemaResponse_schemaName :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaName = Lens.lens (\UpdateSchemaResponse' {schemaName} -> schemaName) (\s@UpdateSchemaResponse' {} a -> s {schemaName = a} :: UpdateSchemaResponse)

-- | The response's http status code.
updateSchemaResponse_httpStatus :: Lens.Lens' UpdateSchemaResponse Prelude.Int
updateSchemaResponse_httpStatus = Lens.lens (\UpdateSchemaResponse' {httpStatus} -> httpStatus) (\s@UpdateSchemaResponse' {} a -> s {httpStatus = a} :: UpdateSchemaResponse)

instance Prelude.NFData UpdateSchemaResponse
