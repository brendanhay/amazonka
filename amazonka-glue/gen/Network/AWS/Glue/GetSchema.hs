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
-- Module      : Network.AWS.Glue.GetSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified schema in detail.
module Network.AWS.Glue.GetSchema
  ( -- * Creating a Request
    GetSchema (..),
    newGetSchema,

    -- * Request Lenses
    getSchema_schemaId,

    -- * Destructuring the Response
    GetSchemaResponse (..),
    newGetSchemaResponse,

    -- * Response Lenses
    getSchemaResponse_schemaArn,
    getSchemaResponse_nextSchemaVersion,
    getSchemaResponse_schemaCheckpoint,
    getSchemaResponse_dataFormat,
    getSchemaResponse_updatedTime,
    getSchemaResponse_createdTime,
    getSchemaResponse_registryName,
    getSchemaResponse_schemaName,
    getSchemaResponse_description,
    getSchemaResponse_compatibility,
    getSchemaResponse_registryArn,
    getSchemaResponse_latestSchemaVersion,
    getSchemaResponse_schemaStatus,
    getSchemaResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSchema' smart constructor.
data GetSchema = GetSchema'
  { -- | This is a wrapper structure to contain schema identity fields. The
    -- structure contains:
    --
    -- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
    --     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
    --     provided.
    --
    -- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
    --     @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: SchemaId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaId', 'getSchema_schemaId' - This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
newGetSchema ::
  -- | 'schemaId'
  SchemaId ->
  GetSchema
newGetSchema pSchemaId_ =
  GetSchema' {schemaId = pSchemaId_}

-- | This is a wrapper structure to contain schema identity fields. The
-- structure contains:
--
-- -   SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema.
--     Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be
--     provided.
--
-- -   SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or
--     @SchemaName@ and @RegistryName@ has to be provided.
getSchema_schemaId :: Lens.Lens' GetSchema SchemaId
getSchema_schemaId = Lens.lens (\GetSchema' {schemaId} -> schemaId) (\s@GetSchema' {} a -> s {schemaId = a} :: GetSchema)

instance Core.AWSRequest GetSchema where
  type AWSResponse GetSchema = GetSchemaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaResponse'
            Core.<$> (x Core..?> "SchemaArn")
            Core.<*> (x Core..?> "NextSchemaVersion")
            Core.<*> (x Core..?> "SchemaCheckpoint")
            Core.<*> (x Core..?> "DataFormat")
            Core.<*> (x Core..?> "UpdatedTime")
            Core.<*> (x Core..?> "CreatedTime")
            Core.<*> (x Core..?> "RegistryName")
            Core.<*> (x Core..?> "SchemaName")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "Compatibility")
            Core.<*> (x Core..?> "RegistryArn")
            Core.<*> (x Core..?> "LatestSchemaVersion")
            Core.<*> (x Core..?> "SchemaStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSchema

instance Core.NFData GetSchema

instance Core.ToHeaders GetSchema where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetSchema" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSchema where
  toJSON GetSchema' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SchemaId" Core..= schemaId)]
      )

instance Core.ToPath GetSchema where
  toPath = Core.const "/"

instance Core.ToQuery GetSchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The next version of the schema associated with the returned schema
    -- definition.
    nextSchemaVersion :: Core.Maybe Core.Natural,
    -- | The version number of the checkpoint (the last time the compatibility
    -- mode was changed).
    schemaCheckpoint :: Core.Maybe Core.Natural,
    -- | The data format of the schema definition. Currently only @AVRO@ is
    -- supported.
    dataFormat :: Core.Maybe DataFormat,
    -- | The date and time the schema was updated.
    updatedTime :: Core.Maybe Core.Text,
    -- | The date and time the schema was created.
    createdTime :: Core.Maybe Core.Text,
    -- | The name of the registry.
    registryName :: Core.Maybe Core.Text,
    -- | The name of the schema.
    schemaName :: Core.Maybe Core.Text,
    -- | A description of schema if specified when created
    description :: Core.Maybe Core.Text,
    -- | The compatibility mode of the schema.
    compatibility :: Core.Maybe Compatibility,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Core.Maybe Core.Text,
    -- | The latest version of the schema associated with the returned schema
    -- definition.
    latestSchemaVersion :: Core.Maybe Core.Natural,
    -- | The status of the schema.
    schemaStatus :: Core.Maybe SchemaStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'getSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'nextSchemaVersion', 'getSchemaResponse_nextSchemaVersion' - The next version of the schema associated with the returned schema
-- definition.
--
-- 'schemaCheckpoint', 'getSchemaResponse_schemaCheckpoint' - The version number of the checkpoint (the last time the compatibility
-- mode was changed).
--
-- 'dataFormat', 'getSchemaResponse_dataFormat' - The data format of the schema definition. Currently only @AVRO@ is
-- supported.
--
-- 'updatedTime', 'getSchemaResponse_updatedTime' - The date and time the schema was updated.
--
-- 'createdTime', 'getSchemaResponse_createdTime' - The date and time the schema was created.
--
-- 'registryName', 'getSchemaResponse_registryName' - The name of the registry.
--
-- 'schemaName', 'getSchemaResponse_schemaName' - The name of the schema.
--
-- 'description', 'getSchemaResponse_description' - A description of schema if specified when created
--
-- 'compatibility', 'getSchemaResponse_compatibility' - The compatibility mode of the schema.
--
-- 'registryArn', 'getSchemaResponse_registryArn' - The Amazon Resource Name (ARN) of the registry.
--
-- 'latestSchemaVersion', 'getSchemaResponse_latestSchemaVersion' - The latest version of the schema associated with the returned schema
-- definition.
--
-- 'schemaStatus', 'getSchemaResponse_schemaStatus' - The status of the schema.
--
-- 'httpStatus', 'getSchemaResponse_httpStatus' - The response's http status code.
newGetSchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSchemaResponse
newGetSchemaResponse pHttpStatus_ =
  GetSchemaResponse'
    { schemaArn = Core.Nothing,
      nextSchemaVersion = Core.Nothing,
      schemaCheckpoint = Core.Nothing,
      dataFormat = Core.Nothing,
      updatedTime = Core.Nothing,
      createdTime = Core.Nothing,
      registryName = Core.Nothing,
      schemaName = Core.Nothing,
      description = Core.Nothing,
      compatibility = Core.Nothing,
      registryArn = Core.Nothing,
      latestSchemaVersion = Core.Nothing,
      schemaStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema.
getSchemaResponse_schemaArn :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_schemaArn = Lens.lens (\GetSchemaResponse' {schemaArn} -> schemaArn) (\s@GetSchemaResponse' {} a -> s {schemaArn = a} :: GetSchemaResponse)

-- | The next version of the schema associated with the returned schema
-- definition.
getSchemaResponse_nextSchemaVersion :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
getSchemaResponse_nextSchemaVersion = Lens.lens (\GetSchemaResponse' {nextSchemaVersion} -> nextSchemaVersion) (\s@GetSchemaResponse' {} a -> s {nextSchemaVersion = a} :: GetSchemaResponse)

-- | The version number of the checkpoint (the last time the compatibility
-- mode was changed).
getSchemaResponse_schemaCheckpoint :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
getSchemaResponse_schemaCheckpoint = Lens.lens (\GetSchemaResponse' {schemaCheckpoint} -> schemaCheckpoint) (\s@GetSchemaResponse' {} a -> s {schemaCheckpoint = a} :: GetSchemaResponse)

-- | The data format of the schema definition. Currently only @AVRO@ is
-- supported.
getSchemaResponse_dataFormat :: Lens.Lens' GetSchemaResponse (Core.Maybe DataFormat)
getSchemaResponse_dataFormat = Lens.lens (\GetSchemaResponse' {dataFormat} -> dataFormat) (\s@GetSchemaResponse' {} a -> s {dataFormat = a} :: GetSchemaResponse)

-- | The date and time the schema was updated.
getSchemaResponse_updatedTime :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_updatedTime = Lens.lens (\GetSchemaResponse' {updatedTime} -> updatedTime) (\s@GetSchemaResponse' {} a -> s {updatedTime = a} :: GetSchemaResponse)

-- | The date and time the schema was created.
getSchemaResponse_createdTime :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_createdTime = Lens.lens (\GetSchemaResponse' {createdTime} -> createdTime) (\s@GetSchemaResponse' {} a -> s {createdTime = a} :: GetSchemaResponse)

-- | The name of the registry.
getSchemaResponse_registryName :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_registryName = Lens.lens (\GetSchemaResponse' {registryName} -> registryName) (\s@GetSchemaResponse' {} a -> s {registryName = a} :: GetSchemaResponse)

-- | The name of the schema.
getSchemaResponse_schemaName :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_schemaName = Lens.lens (\GetSchemaResponse' {schemaName} -> schemaName) (\s@GetSchemaResponse' {} a -> s {schemaName = a} :: GetSchemaResponse)

-- | A description of schema if specified when created
getSchemaResponse_description :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_description = Lens.lens (\GetSchemaResponse' {description} -> description) (\s@GetSchemaResponse' {} a -> s {description = a} :: GetSchemaResponse)

-- | The compatibility mode of the schema.
getSchemaResponse_compatibility :: Lens.Lens' GetSchemaResponse (Core.Maybe Compatibility)
getSchemaResponse_compatibility = Lens.lens (\GetSchemaResponse' {compatibility} -> compatibility) (\s@GetSchemaResponse' {} a -> s {compatibility = a} :: GetSchemaResponse)

-- | The Amazon Resource Name (ARN) of the registry.
getSchemaResponse_registryArn :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Text)
getSchemaResponse_registryArn = Lens.lens (\GetSchemaResponse' {registryArn} -> registryArn) (\s@GetSchemaResponse' {} a -> s {registryArn = a} :: GetSchemaResponse)

-- | The latest version of the schema associated with the returned schema
-- definition.
getSchemaResponse_latestSchemaVersion :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
getSchemaResponse_latestSchemaVersion = Lens.lens (\GetSchemaResponse' {latestSchemaVersion} -> latestSchemaVersion) (\s@GetSchemaResponse' {} a -> s {latestSchemaVersion = a} :: GetSchemaResponse)

-- | The status of the schema.
getSchemaResponse_schemaStatus :: Lens.Lens' GetSchemaResponse (Core.Maybe SchemaStatus)
getSchemaResponse_schemaStatus = Lens.lens (\GetSchemaResponse' {schemaStatus} -> schemaStatus) (\s@GetSchemaResponse' {} a -> s {schemaStatus = a} :: GetSchemaResponse)

-- | The response's http status code.
getSchemaResponse_httpStatus :: Lens.Lens' GetSchemaResponse Core.Int
getSchemaResponse_httpStatus = Lens.lens (\GetSchemaResponse' {httpStatus} -> httpStatus) (\s@GetSchemaResponse' {} a -> s {httpStatus = a} :: GetSchemaResponse)

instance Core.NFData GetSchemaResponse
