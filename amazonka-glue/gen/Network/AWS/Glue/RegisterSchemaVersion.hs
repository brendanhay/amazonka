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
-- Module      : Network.AWS.Glue.RegisterSchemaVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new version to the existing schema. Returns an error if new
-- version of schema does not meet the compatibility requirements of the
-- schema set. This API will not create a new schema set and will return a
-- 404 error if the schema set is not already present in the Schema
-- Registry.
--
-- If this is the first schema definition to be registered in the Schema
-- Registry, this API will store the schema version and return immediately.
-- Otherwise, this call has the potential to run longer than other
-- operations due to compatibility modes. You can call the
-- @GetSchemaVersion@ API with the @SchemaVersionId@ to check compatibility
-- modes.
--
-- If the same schema definition is already stored in Schema Registry as a
-- version, the schema ID of the existing schema is returned to the caller.
module Network.AWS.Glue.RegisterSchemaVersion
  ( -- * Creating a Request
    RegisterSchemaVersion (..),
    newRegisterSchemaVersion,

    -- * Request Lenses
    registerSchemaVersion_schemaId,
    registerSchemaVersion_schemaDefinition,

    -- * Destructuring the Response
    RegisterSchemaVersionResponse (..),
    newRegisterSchemaVersionResponse,

    -- * Response Lenses
    registerSchemaVersionResponse_status,
    registerSchemaVersionResponse_schemaVersionId,
    registerSchemaVersionResponse_versionNumber,
    registerSchemaVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterSchemaVersion' smart constructor.
data RegisterSchemaVersion = RegisterSchemaVersion'
  { -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
    --     provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
    --     @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: SchemaId,
    -- | The schema definition using the @DataFormat@ setting for the
    -- @SchemaName@.
    schemaDefinition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterSchemaVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaId', 'registerSchemaVersion_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
--
-- 'schemaDefinition', 'registerSchemaVersion_schemaDefinition' - The schema definition using the @DataFormat@ setting for the
-- @SchemaName@.
newRegisterSchemaVersion ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'schemaDefinition'
  Prelude.Text ->
  RegisterSchemaVersion
newRegisterSchemaVersion
  pSchemaId_
  pSchemaDefinition_ =
    RegisterSchemaVersion'
      { schemaId = pSchemaId_,
        schemaDefinition = pSchemaDefinition_
      }

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
registerSchemaVersion_schemaId :: Lens.Lens' RegisterSchemaVersion SchemaId
registerSchemaVersion_schemaId = Lens.lens (\RegisterSchemaVersion' {schemaId} -> schemaId) (\s@RegisterSchemaVersion' {} a -> s {schemaId = a} :: RegisterSchemaVersion)

-- | The schema definition using the @DataFormat@ setting for the
-- @SchemaName@.
registerSchemaVersion_schemaDefinition :: Lens.Lens' RegisterSchemaVersion Prelude.Text
registerSchemaVersion_schemaDefinition = Lens.lens (\RegisterSchemaVersion' {schemaDefinition} -> schemaDefinition) (\s@RegisterSchemaVersion' {} a -> s {schemaDefinition = a} :: RegisterSchemaVersion)

instance Core.AWSRequest RegisterSchemaVersion where
  type
    AWSResponse RegisterSchemaVersion =
      RegisterSchemaVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterSchemaVersionResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "SchemaVersionId")
            Prelude.<*> (x Core..?> "VersionNumber")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterSchemaVersion

instance Prelude.NFData RegisterSchemaVersion

instance Core.ToHeaders RegisterSchemaVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.RegisterSchemaVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterSchemaVersion where
  toJSON RegisterSchemaVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaId" Core..= schemaId),
            Prelude.Just
              ("SchemaDefinition" Core..= schemaDefinition)
          ]
      )

instance Core.ToPath RegisterSchemaVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterSchemaVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterSchemaVersionResponse' smart constructor.
data RegisterSchemaVersionResponse = RegisterSchemaVersionResponse'
  { -- | The status of the schema version.
    status :: Prelude.Maybe SchemaVersionStatus,
    -- | The unique ID that represents the version of this schema.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The version of this schema (for sync flow only, in case this is the
    -- first version).
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterSchemaVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'registerSchemaVersionResponse_status' - The status of the schema version.
--
-- 'schemaVersionId', 'registerSchemaVersionResponse_schemaVersionId' - The unique ID that represents the version of this schema.
--
-- 'versionNumber', 'registerSchemaVersionResponse_versionNumber' - The version of this schema (for sync flow only, in case this is the
-- first version).
--
-- 'httpStatus', 'registerSchemaVersionResponse_httpStatus' - The response's http status code.
newRegisterSchemaVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterSchemaVersionResponse
newRegisterSchemaVersionResponse pHttpStatus_ =
  RegisterSchemaVersionResponse'
    { status =
        Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the schema version.
registerSchemaVersionResponse_status :: Lens.Lens' RegisterSchemaVersionResponse (Prelude.Maybe SchemaVersionStatus)
registerSchemaVersionResponse_status = Lens.lens (\RegisterSchemaVersionResponse' {status} -> status) (\s@RegisterSchemaVersionResponse' {} a -> s {status = a} :: RegisterSchemaVersionResponse)

-- | The unique ID that represents the version of this schema.
registerSchemaVersionResponse_schemaVersionId :: Lens.Lens' RegisterSchemaVersionResponse (Prelude.Maybe Prelude.Text)
registerSchemaVersionResponse_schemaVersionId = Lens.lens (\RegisterSchemaVersionResponse' {schemaVersionId} -> schemaVersionId) (\s@RegisterSchemaVersionResponse' {} a -> s {schemaVersionId = a} :: RegisterSchemaVersionResponse)

-- | The version of this schema (for sync flow only, in case this is the
-- first version).
registerSchemaVersionResponse_versionNumber :: Lens.Lens' RegisterSchemaVersionResponse (Prelude.Maybe Prelude.Natural)
registerSchemaVersionResponse_versionNumber = Lens.lens (\RegisterSchemaVersionResponse' {versionNumber} -> versionNumber) (\s@RegisterSchemaVersionResponse' {} a -> s {versionNumber = a} :: RegisterSchemaVersionResponse)

-- | The response's http status code.
registerSchemaVersionResponse_httpStatus :: Lens.Lens' RegisterSchemaVersionResponse Prelude.Int
registerSchemaVersionResponse_httpStatus = Lens.lens (\RegisterSchemaVersionResponse' {httpStatus} -> httpStatus) (\s@RegisterSchemaVersionResponse' {} a -> s {httpStatus = a} :: RegisterSchemaVersionResponse)

instance Prelude.NFData RegisterSchemaVersionResponse
