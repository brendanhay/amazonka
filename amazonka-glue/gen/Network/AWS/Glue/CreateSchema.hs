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
-- Module      : Network.AWS.Glue.CreateSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new schema set and registers the schema definition. Returns an
-- error if the schema set already exists without actually registering the
-- version.
--
-- When the schema set is created, a version checkpoint will be set to the
-- first version. Compatibility mode \"DISABLED\" restricts any additional
-- schema versions from being added after the first schema version. For all
-- other compatibility modes, validation of compatibility settings will be
-- applied only from the second version onwards when the
-- @RegisterSchemaVersion@ API is used.
--
-- When this API is called without a @RegistryId@, this will create an
-- entry for a \"default-registry\" in the registry database tables, if it
-- is not already present.
module Network.AWS.Glue.CreateSchema
  ( -- * Creating a Request
    CreateSchema (..),
    newCreateSchema,

    -- * Request Lenses
    createSchema_schemaDefinition,
    createSchema_registryId,
    createSchema_tags,
    createSchema_description,
    createSchema_compatibility,
    createSchema_schemaName,
    createSchema_dataFormat,

    -- * Destructuring the Response
    CreateSchemaResponse (..),
    newCreateSchemaResponse,

    -- * Response Lenses
    createSchemaResponse_schemaArn,
    createSchemaResponse_nextSchemaVersion,
    createSchemaResponse_schemaVersionId,
    createSchemaResponse_schemaCheckpoint,
    createSchemaResponse_dataFormat,
    createSchemaResponse_registryName,
    createSchemaResponse_schemaVersionStatus,
    createSchemaResponse_tags,
    createSchemaResponse_schemaName,
    createSchemaResponse_description,
    createSchemaResponse_compatibility,
    createSchemaResponse_registryArn,
    createSchemaResponse_latestSchemaVersion,
    createSchemaResponse_schemaStatus,
    createSchemaResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { -- | The schema definition using the @DataFormat@ setting for @SchemaName@.
    schemaDefinition :: Core.Maybe Core.Text,
    -- | This is a wrapper shape to contain the registry identity fields. If this
    -- is not provided, the default registry will be used. The ARN format for
    -- the same will be:
    -- @arn:aws:glue:us-east-2:\<customer id>:registry\/default-registry:random-5-letter-id@.
    registryId :: Core.Maybe RegistryId,
    -- | AWS tags that contain a key value pair and may be searched by console,
    -- command line, or API. If specified, follows the AWS tags-on-create
    -- pattern.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | An optional description of the schema. If description is not provided,
    -- there will not be any automatic default value for this.
    description :: Core.Maybe Core.Text,
    -- | The compatibility mode of the schema. The possible values are:
    --
    -- -   /NONE/: No compatibility mode applies. You can use this choice in
    --     development scenarios or if you do not know the compatibility mode
    --     that you want to apply to schemas. Any new version added will be
    --     accepted without undergoing a compatibility check.
    --
    -- -   /DISABLED/: This compatibility choice prevents versioning for a
    --     particular schema. You can use this choice to prevent future
    --     versioning of a schema.
    --
    -- -   /BACKWARD/: This compatibility choice is recommended as it allows
    --     data receivers to read both the current and one previous schema
    --     version. This means that for instance, a new schema version cannot
    --     drop data fields or change the type of these fields, so they can\'t
    --     be read by readers using the previous version.
    --
    -- -   /BACKWARD_ALL/: This compatibility choice allows data receivers to
    --     read both the current and all previous schema versions. You can use
    --     this choice when you need to delete fields or add optional fields,
    --     and check compatibility against all previous schema versions.
    --
    -- -   /FORWARD/: This compatibility choice allows data receivers to read
    --     both the current and one next schema version, but not necessarily
    --     later versions. You can use this choice when you need to add fields
    --     or delete optional fields, but only check compatibility against the
    --     last schema version.
    --
    -- -   /FORWARD_ALL/: This compatibility choice allows data receivers to
    --     read written by producers of any new registered schema. You can use
    --     this choice when you need to add fields or delete optional fields,
    --     and check compatibility against all previous schema versions.
    --
    -- -   /FULL/: This compatibility choice allows data receivers to read data
    --     written by producers using the previous or next version of the
    --     schema, but not necessarily earlier or later versions. You can use
    --     this choice when you need to add or remove optional fields, but only
    --     check compatibility against the last schema version.
    --
    -- -   /FULL_ALL/: This compatibility choice allows data receivers to read
    --     data written by producers using all previous schema versions. You
    --     can use this choice when you need to add or remove optional fields,
    --     and check compatibility against all previous schema versions.
    compatibility :: Core.Maybe Compatibility,
    -- | Name of the schema to be created of max length of 255, and may only
    -- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
    -- No whitespace.
    schemaName :: Core.Text,
    -- | The data format of the schema definition. Currently only @AVRO@ is
    -- supported.
    dataFormat :: DataFormat
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaDefinition', 'createSchema_schemaDefinition' - The schema definition using the @DataFormat@ setting for @SchemaName@.
--
-- 'registryId', 'createSchema_registryId' - This is a wrapper shape to contain the registry identity fields. If this
-- is not provided, the default registry will be used. The ARN format for
-- the same will be:
-- @arn:aws:glue:us-east-2:\<customer id>:registry\/default-registry:random-5-letter-id@.
--
-- 'tags', 'createSchema_tags' - AWS tags that contain a key value pair and may be searched by console,
-- command line, or API. If specified, follows the AWS tags-on-create
-- pattern.
--
-- 'description', 'createSchema_description' - An optional description of the schema. If description is not provided,
-- there will not be any automatic default value for this.
--
-- 'compatibility', 'createSchema_compatibility' - The compatibility mode of the schema. The possible values are:
--
-- -   /NONE/: No compatibility mode applies. You can use this choice in
--     development scenarios or if you do not know the compatibility mode
--     that you want to apply to schemas. Any new version added will be
--     accepted without undergoing a compatibility check.
--
-- -   /DISABLED/: This compatibility choice prevents versioning for a
--     particular schema. You can use this choice to prevent future
--     versioning of a schema.
--
-- -   /BACKWARD/: This compatibility choice is recommended as it allows
--     data receivers to read both the current and one previous schema
--     version. This means that for instance, a new schema version cannot
--     drop data fields or change the type of these fields, so they can\'t
--     be read by readers using the previous version.
--
-- -   /BACKWARD_ALL/: This compatibility choice allows data receivers to
--     read both the current and all previous schema versions. You can use
--     this choice when you need to delete fields or add optional fields,
--     and check compatibility against all previous schema versions.
--
-- -   /FORWARD/: This compatibility choice allows data receivers to read
--     both the current and one next schema version, but not necessarily
--     later versions. You can use this choice when you need to add fields
--     or delete optional fields, but only check compatibility against the
--     last schema version.
--
-- -   /FORWARD_ALL/: This compatibility choice allows data receivers to
--     read written by producers of any new registered schema. You can use
--     this choice when you need to add fields or delete optional fields,
--     and check compatibility against all previous schema versions.
--
-- -   /FULL/: This compatibility choice allows data receivers to read data
--     written by producers using the previous or next version of the
--     schema, but not necessarily earlier or later versions. You can use
--     this choice when you need to add or remove optional fields, but only
--     check compatibility against the last schema version.
--
-- -   /FULL_ALL/: This compatibility choice allows data receivers to read
--     data written by producers using all previous schema versions. You
--     can use this choice when you need to add or remove optional fields,
--     and check compatibility against all previous schema versions.
--
-- 'schemaName', 'createSchema_schemaName' - Name of the schema to be created of max length of 255, and may only
-- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
-- No whitespace.
--
-- 'dataFormat', 'createSchema_dataFormat' - The data format of the schema definition. Currently only @AVRO@ is
-- supported.
newCreateSchema ::
  -- | 'schemaName'
  Core.Text ->
  -- | 'dataFormat'
  DataFormat ->
  CreateSchema
newCreateSchema pSchemaName_ pDataFormat_ =
  CreateSchema'
    { schemaDefinition = Core.Nothing,
      registryId = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      compatibility = Core.Nothing,
      schemaName = pSchemaName_,
      dataFormat = pDataFormat_
    }

-- | The schema definition using the @DataFormat@ setting for @SchemaName@.
createSchema_schemaDefinition :: Lens.Lens' CreateSchema (Core.Maybe Core.Text)
createSchema_schemaDefinition = Lens.lens (\CreateSchema' {schemaDefinition} -> schemaDefinition) (\s@CreateSchema' {} a -> s {schemaDefinition = a} :: CreateSchema)

-- | This is a wrapper shape to contain the registry identity fields. If this
-- is not provided, the default registry will be used. The ARN format for
-- the same will be:
-- @arn:aws:glue:us-east-2:\<customer id>:registry\/default-registry:random-5-letter-id@.
createSchema_registryId :: Lens.Lens' CreateSchema (Core.Maybe RegistryId)
createSchema_registryId = Lens.lens (\CreateSchema' {registryId} -> registryId) (\s@CreateSchema' {} a -> s {registryId = a} :: CreateSchema)

-- | AWS tags that contain a key value pair and may be searched by console,
-- command line, or API. If specified, follows the AWS tags-on-create
-- pattern.
createSchema_tags :: Lens.Lens' CreateSchema (Core.Maybe (Core.HashMap Core.Text Core.Text))
createSchema_tags = Lens.lens (\CreateSchema' {tags} -> tags) (\s@CreateSchema' {} a -> s {tags = a} :: CreateSchema) Core.. Lens.mapping Lens._Coerce

-- | An optional description of the schema. If description is not provided,
-- there will not be any automatic default value for this.
createSchema_description :: Lens.Lens' CreateSchema (Core.Maybe Core.Text)
createSchema_description = Lens.lens (\CreateSchema' {description} -> description) (\s@CreateSchema' {} a -> s {description = a} :: CreateSchema)

-- | The compatibility mode of the schema. The possible values are:
--
-- -   /NONE/: No compatibility mode applies. You can use this choice in
--     development scenarios or if you do not know the compatibility mode
--     that you want to apply to schemas. Any new version added will be
--     accepted without undergoing a compatibility check.
--
-- -   /DISABLED/: This compatibility choice prevents versioning for a
--     particular schema. You can use this choice to prevent future
--     versioning of a schema.
--
-- -   /BACKWARD/: This compatibility choice is recommended as it allows
--     data receivers to read both the current and one previous schema
--     version. This means that for instance, a new schema version cannot
--     drop data fields or change the type of these fields, so they can\'t
--     be read by readers using the previous version.
--
-- -   /BACKWARD_ALL/: This compatibility choice allows data receivers to
--     read both the current and all previous schema versions. You can use
--     this choice when you need to delete fields or add optional fields,
--     and check compatibility against all previous schema versions.
--
-- -   /FORWARD/: This compatibility choice allows data receivers to read
--     both the current and one next schema version, but not necessarily
--     later versions. You can use this choice when you need to add fields
--     or delete optional fields, but only check compatibility against the
--     last schema version.
--
-- -   /FORWARD_ALL/: This compatibility choice allows data receivers to
--     read written by producers of any new registered schema. You can use
--     this choice when you need to add fields or delete optional fields,
--     and check compatibility against all previous schema versions.
--
-- -   /FULL/: This compatibility choice allows data receivers to read data
--     written by producers using the previous or next version of the
--     schema, but not necessarily earlier or later versions. You can use
--     this choice when you need to add or remove optional fields, but only
--     check compatibility against the last schema version.
--
-- -   /FULL_ALL/: This compatibility choice allows data receivers to read
--     data written by producers using all previous schema versions. You
--     can use this choice when you need to add or remove optional fields,
--     and check compatibility against all previous schema versions.
createSchema_compatibility :: Lens.Lens' CreateSchema (Core.Maybe Compatibility)
createSchema_compatibility = Lens.lens (\CreateSchema' {compatibility} -> compatibility) (\s@CreateSchema' {} a -> s {compatibility = a} :: CreateSchema)

-- | Name of the schema to be created of max length of 255, and may only
-- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
-- No whitespace.
createSchema_schemaName :: Lens.Lens' CreateSchema Core.Text
createSchema_schemaName = Lens.lens (\CreateSchema' {schemaName} -> schemaName) (\s@CreateSchema' {} a -> s {schemaName = a} :: CreateSchema)

-- | The data format of the schema definition. Currently only @AVRO@ is
-- supported.
createSchema_dataFormat :: Lens.Lens' CreateSchema DataFormat
createSchema_dataFormat = Lens.lens (\CreateSchema' {dataFormat} -> dataFormat) (\s@CreateSchema' {} a -> s {dataFormat = a} :: CreateSchema)

instance Core.AWSRequest CreateSchema where
  type AWSResponse CreateSchema = CreateSchemaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Core.<$> (x Core..?> "SchemaArn")
            Core.<*> (x Core..?> "NextSchemaVersion")
            Core.<*> (x Core..?> "SchemaVersionId")
            Core.<*> (x Core..?> "SchemaCheckpoint")
            Core.<*> (x Core..?> "DataFormat")
            Core.<*> (x Core..?> "RegistryName")
            Core.<*> (x Core..?> "SchemaVersionStatus")
            Core.<*> (x Core..?> "Tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "SchemaName")
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "Compatibility")
            Core.<*> (x Core..?> "RegistryArn")
            Core.<*> (x Core..?> "LatestSchemaVersion")
            Core.<*> (x Core..?> "SchemaStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateSchema

instance Core.NFData CreateSchema

instance Core.ToHeaders CreateSchema where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateSchema" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaDefinition" Core..=)
              Core.<$> schemaDefinition,
            ("RegistryId" Core..=) Core.<$> registryId,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("Compatibility" Core..=) Core.<$> compatibility,
            Core.Just ("SchemaName" Core..= schemaName),
            Core.Just ("DataFormat" Core..= dataFormat)
          ]
      )

instance Core.ToPath CreateSchema where
  toPath = Core.const "/"

instance Core.ToQuery CreateSchema where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Core.Text,
    -- | The next version of the schema associated with the returned schema
    -- definition.
    nextSchemaVersion :: Core.Maybe Core.Natural,
    -- | The unique identifier of the first schema version.
    schemaVersionId :: Core.Maybe Core.Text,
    -- | The version number of the checkpoint (the last time the compatibility
    -- mode was changed).
    schemaCheckpoint :: Core.Maybe Core.Natural,
    -- | The data format of the schema definition. Currently only @AVRO@ is
    -- supported.
    dataFormat :: Core.Maybe DataFormat,
    -- | The name of the registry.
    registryName :: Core.Maybe Core.Text,
    -- | The status of the first schema version created.
    schemaVersionStatus :: Core.Maybe SchemaVersionStatus,
    -- | The tags for the schema.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the schema.
    schemaName :: Core.Maybe Core.Text,
    -- | A description of the schema if specified when created.
    description :: Core.Maybe Core.Text,
    -- | The schema compatibility mode.
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
-- Create a value of 'CreateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'createSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'nextSchemaVersion', 'createSchemaResponse_nextSchemaVersion' - The next version of the schema associated with the returned schema
-- definition.
--
-- 'schemaVersionId', 'createSchemaResponse_schemaVersionId' - The unique identifier of the first schema version.
--
-- 'schemaCheckpoint', 'createSchemaResponse_schemaCheckpoint' - The version number of the checkpoint (the last time the compatibility
-- mode was changed).
--
-- 'dataFormat', 'createSchemaResponse_dataFormat' - The data format of the schema definition. Currently only @AVRO@ is
-- supported.
--
-- 'registryName', 'createSchemaResponse_registryName' - The name of the registry.
--
-- 'schemaVersionStatus', 'createSchemaResponse_schemaVersionStatus' - The status of the first schema version created.
--
-- 'tags', 'createSchemaResponse_tags' - The tags for the schema.
--
-- 'schemaName', 'createSchemaResponse_schemaName' - The name of the schema.
--
-- 'description', 'createSchemaResponse_description' - A description of the schema if specified when created.
--
-- 'compatibility', 'createSchemaResponse_compatibility' - The schema compatibility mode.
--
-- 'registryArn', 'createSchemaResponse_registryArn' - The Amazon Resource Name (ARN) of the registry.
--
-- 'latestSchemaVersion', 'createSchemaResponse_latestSchemaVersion' - The latest version of the schema associated with the returned schema
-- definition.
--
-- 'schemaStatus', 'createSchemaResponse_schemaStatus' - The status of the schema.
--
-- 'httpStatus', 'createSchemaResponse_httpStatus' - The response's http status code.
newCreateSchemaResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateSchemaResponse
newCreateSchemaResponse pHttpStatus_ =
  CreateSchemaResponse'
    { schemaArn = Core.Nothing,
      nextSchemaVersion = Core.Nothing,
      schemaVersionId = Core.Nothing,
      schemaCheckpoint = Core.Nothing,
      dataFormat = Core.Nothing,
      registryName = Core.Nothing,
      schemaVersionStatus = Core.Nothing,
      tags = Core.Nothing,
      schemaName = Core.Nothing,
      description = Core.Nothing,
      compatibility = Core.Nothing,
      registryArn = Core.Nothing,
      latestSchemaVersion = Core.Nothing,
      schemaStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the schema.
createSchemaResponse_schemaArn :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Text)
createSchemaResponse_schemaArn = Lens.lens (\CreateSchemaResponse' {schemaArn} -> schemaArn) (\s@CreateSchemaResponse' {} a -> s {schemaArn = a} :: CreateSchemaResponse)

-- | The next version of the schema associated with the returned schema
-- definition.
createSchemaResponse_nextSchemaVersion :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Natural)
createSchemaResponse_nextSchemaVersion = Lens.lens (\CreateSchemaResponse' {nextSchemaVersion} -> nextSchemaVersion) (\s@CreateSchemaResponse' {} a -> s {nextSchemaVersion = a} :: CreateSchemaResponse)

-- | The unique identifier of the first schema version.
createSchemaResponse_schemaVersionId :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Text)
createSchemaResponse_schemaVersionId = Lens.lens (\CreateSchemaResponse' {schemaVersionId} -> schemaVersionId) (\s@CreateSchemaResponse' {} a -> s {schemaVersionId = a} :: CreateSchemaResponse)

-- | The version number of the checkpoint (the last time the compatibility
-- mode was changed).
createSchemaResponse_schemaCheckpoint :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Natural)
createSchemaResponse_schemaCheckpoint = Lens.lens (\CreateSchemaResponse' {schemaCheckpoint} -> schemaCheckpoint) (\s@CreateSchemaResponse' {} a -> s {schemaCheckpoint = a} :: CreateSchemaResponse)

-- | The data format of the schema definition. Currently only @AVRO@ is
-- supported.
createSchemaResponse_dataFormat :: Lens.Lens' CreateSchemaResponse (Core.Maybe DataFormat)
createSchemaResponse_dataFormat = Lens.lens (\CreateSchemaResponse' {dataFormat} -> dataFormat) (\s@CreateSchemaResponse' {} a -> s {dataFormat = a} :: CreateSchemaResponse)

-- | The name of the registry.
createSchemaResponse_registryName :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Text)
createSchemaResponse_registryName = Lens.lens (\CreateSchemaResponse' {registryName} -> registryName) (\s@CreateSchemaResponse' {} a -> s {registryName = a} :: CreateSchemaResponse)

-- | The status of the first schema version created.
createSchemaResponse_schemaVersionStatus :: Lens.Lens' CreateSchemaResponse (Core.Maybe SchemaVersionStatus)
createSchemaResponse_schemaVersionStatus = Lens.lens (\CreateSchemaResponse' {schemaVersionStatus} -> schemaVersionStatus) (\s@CreateSchemaResponse' {} a -> s {schemaVersionStatus = a} :: CreateSchemaResponse)

-- | The tags for the schema.
createSchemaResponse_tags :: Lens.Lens' CreateSchemaResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
createSchemaResponse_tags = Lens.lens (\CreateSchemaResponse' {tags} -> tags) (\s@CreateSchemaResponse' {} a -> s {tags = a} :: CreateSchemaResponse) Core.. Lens.mapping Lens._Coerce

-- | The name of the schema.
createSchemaResponse_schemaName :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Text)
createSchemaResponse_schemaName = Lens.lens (\CreateSchemaResponse' {schemaName} -> schemaName) (\s@CreateSchemaResponse' {} a -> s {schemaName = a} :: CreateSchemaResponse)

-- | A description of the schema if specified when created.
createSchemaResponse_description :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Text)
createSchemaResponse_description = Lens.lens (\CreateSchemaResponse' {description} -> description) (\s@CreateSchemaResponse' {} a -> s {description = a} :: CreateSchemaResponse)

-- | The schema compatibility mode.
createSchemaResponse_compatibility :: Lens.Lens' CreateSchemaResponse (Core.Maybe Compatibility)
createSchemaResponse_compatibility = Lens.lens (\CreateSchemaResponse' {compatibility} -> compatibility) (\s@CreateSchemaResponse' {} a -> s {compatibility = a} :: CreateSchemaResponse)

-- | The Amazon Resource Name (ARN) of the registry.
createSchemaResponse_registryArn :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Text)
createSchemaResponse_registryArn = Lens.lens (\CreateSchemaResponse' {registryArn} -> registryArn) (\s@CreateSchemaResponse' {} a -> s {registryArn = a} :: CreateSchemaResponse)

-- | The latest version of the schema associated with the returned schema
-- definition.
createSchemaResponse_latestSchemaVersion :: Lens.Lens' CreateSchemaResponse (Core.Maybe Core.Natural)
createSchemaResponse_latestSchemaVersion = Lens.lens (\CreateSchemaResponse' {latestSchemaVersion} -> latestSchemaVersion) (\s@CreateSchemaResponse' {} a -> s {latestSchemaVersion = a} :: CreateSchemaResponse)

-- | The status of the schema.
createSchemaResponse_schemaStatus :: Lens.Lens' CreateSchemaResponse (Core.Maybe SchemaStatus)
createSchemaResponse_schemaStatus = Lens.lens (\CreateSchemaResponse' {schemaStatus} -> schemaStatus) (\s@CreateSchemaResponse' {} a -> s {schemaStatus = a} :: CreateSchemaResponse)

-- | The response's http status code.
createSchemaResponse_httpStatus :: Lens.Lens' CreateSchemaResponse Core.Int
createSchemaResponse_httpStatus = Lens.lens (\CreateSchemaResponse' {httpStatus} -> httpStatus) (\s@CreateSchemaResponse' {} a -> s {httpStatus = a} :: CreateSchemaResponse)

instance Core.NFData CreateSchemaResponse
