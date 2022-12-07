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
-- Module      : Amazonka.Glue.CreateSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Glue.CreateSchema
  ( -- * Creating a Request
    CreateSchema (..),
    newCreateSchema,

    -- * Request Lenses
    createSchema_compatibility,
    createSchema_tags,
    createSchema_description,
    createSchema_registryId,
    createSchema_schemaDefinition,
    createSchema_schemaName,
    createSchema_dataFormat,

    -- * Destructuring the Response
    CreateSchemaResponse (..),
    newCreateSchemaResponse,

    -- * Response Lenses
    createSchemaResponse_compatibility,
    createSchemaResponse_tags,
    createSchemaResponse_registryName,
    createSchemaResponse_schemaStatus,
    createSchemaResponse_dataFormat,
    createSchemaResponse_schemaVersionStatus,
    createSchemaResponse_schemaName,
    createSchemaResponse_description,
    createSchemaResponse_schemaArn,
    createSchemaResponse_registryArn,
    createSchemaResponse_nextSchemaVersion,
    createSchemaResponse_schemaCheckpoint,
    createSchemaResponse_schemaVersionId,
    createSchemaResponse_latestSchemaVersion,
    createSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { -- | The compatibility mode of the schema. The possible values are:
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
    compatibility :: Prelude.Maybe Compatibility,
    -- | Amazon Web Services tags that contain a key value pair and may be
    -- searched by console, command line, or API. If specified, follows the
    -- Amazon Web Services tags-on-create pattern.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An optional description of the schema. If description is not provided,
    -- there will not be any automatic default value for this.
    description :: Prelude.Maybe Prelude.Text,
    -- | This is a wrapper shape to contain the registry identity fields. If this
    -- is not provided, the default registry will be used. The ARN format for
    -- the same will be:
    -- @arn:aws:glue:us-east-2:\<customer id>:registry\/default-registry:random-5-letter-id@.
    registryId :: Prelude.Maybe RegistryId,
    -- | The schema definition using the @DataFormat@ setting for @SchemaName@.
    schemaDefinition :: Prelude.Maybe Prelude.Text,
    -- | Name of the schema to be created of max length of 255, and may only
    -- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
    -- No whitespace.
    schemaName :: Prelude.Text,
    -- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
    -- @PROTOBUF@ are supported.
    dataFormat :: DataFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'tags', 'createSchema_tags' - Amazon Web Services tags that contain a key value pair and may be
-- searched by console, command line, or API. If specified, follows the
-- Amazon Web Services tags-on-create pattern.
--
-- 'description', 'createSchema_description' - An optional description of the schema. If description is not provided,
-- there will not be any automatic default value for this.
--
-- 'registryId', 'createSchema_registryId' - This is a wrapper shape to contain the registry identity fields. If this
-- is not provided, the default registry will be used. The ARN format for
-- the same will be:
-- @arn:aws:glue:us-east-2:\<customer id>:registry\/default-registry:random-5-letter-id@.
--
-- 'schemaDefinition', 'createSchema_schemaDefinition' - The schema definition using the @DataFormat@ setting for @SchemaName@.
--
-- 'schemaName', 'createSchema_schemaName' - Name of the schema to be created of max length of 255, and may only
-- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
-- No whitespace.
--
-- 'dataFormat', 'createSchema_dataFormat' - The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
newCreateSchema ::
  -- | 'schemaName'
  Prelude.Text ->
  -- | 'dataFormat'
  DataFormat ->
  CreateSchema
newCreateSchema pSchemaName_ pDataFormat_ =
  CreateSchema'
    { compatibility = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      registryId = Prelude.Nothing,
      schemaDefinition = Prelude.Nothing,
      schemaName = pSchemaName_,
      dataFormat = pDataFormat_
    }

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
createSchema_compatibility :: Lens.Lens' CreateSchema (Prelude.Maybe Compatibility)
createSchema_compatibility = Lens.lens (\CreateSchema' {compatibility} -> compatibility) (\s@CreateSchema' {} a -> s {compatibility = a} :: CreateSchema)

-- | Amazon Web Services tags that contain a key value pair and may be
-- searched by console, command line, or API. If specified, follows the
-- Amazon Web Services tags-on-create pattern.
createSchema_tags :: Lens.Lens' CreateSchema (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSchema_tags = Lens.lens (\CreateSchema' {tags} -> tags) (\s@CreateSchema' {} a -> s {tags = a} :: CreateSchema) Prelude.. Lens.mapping Lens.coerced

-- | An optional description of the schema. If description is not provided,
-- there will not be any automatic default value for this.
createSchema_description :: Lens.Lens' CreateSchema (Prelude.Maybe Prelude.Text)
createSchema_description = Lens.lens (\CreateSchema' {description} -> description) (\s@CreateSchema' {} a -> s {description = a} :: CreateSchema)

-- | This is a wrapper shape to contain the registry identity fields. If this
-- is not provided, the default registry will be used. The ARN format for
-- the same will be:
-- @arn:aws:glue:us-east-2:\<customer id>:registry\/default-registry:random-5-letter-id@.
createSchema_registryId :: Lens.Lens' CreateSchema (Prelude.Maybe RegistryId)
createSchema_registryId = Lens.lens (\CreateSchema' {registryId} -> registryId) (\s@CreateSchema' {} a -> s {registryId = a} :: CreateSchema)

-- | The schema definition using the @DataFormat@ setting for @SchemaName@.
createSchema_schemaDefinition :: Lens.Lens' CreateSchema (Prelude.Maybe Prelude.Text)
createSchema_schemaDefinition = Lens.lens (\CreateSchema' {schemaDefinition} -> schemaDefinition) (\s@CreateSchema' {} a -> s {schemaDefinition = a} :: CreateSchema)

-- | Name of the schema to be created of max length of 255, and may only
-- contain letters, numbers, hyphen, underscore, dollar sign, or hash mark.
-- No whitespace.
createSchema_schemaName :: Lens.Lens' CreateSchema Prelude.Text
createSchema_schemaName = Lens.lens (\CreateSchema' {schemaName} -> schemaName) (\s@CreateSchema' {} a -> s {schemaName = a} :: CreateSchema)

-- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
createSchema_dataFormat :: Lens.Lens' CreateSchema DataFormat
createSchema_dataFormat = Lens.lens (\CreateSchema' {dataFormat} -> dataFormat) (\s@CreateSchema' {} a -> s {dataFormat = a} :: CreateSchema)

instance Core.AWSRequest CreateSchema where
  type AWSResponse CreateSchema = CreateSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Prelude.<$> (x Data..?> "Compatibility")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "RegistryName")
            Prelude.<*> (x Data..?> "SchemaStatus")
            Prelude.<*> (x Data..?> "DataFormat")
            Prelude.<*> (x Data..?> "SchemaVersionStatus")
            Prelude.<*> (x Data..?> "SchemaName")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "SchemaArn")
            Prelude.<*> (x Data..?> "RegistryArn")
            Prelude.<*> (x Data..?> "NextSchemaVersion")
            Prelude.<*> (x Data..?> "SchemaCheckpoint")
            Prelude.<*> (x Data..?> "SchemaVersionId")
            Prelude.<*> (x Data..?> "LatestSchemaVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSchema where
  hashWithSalt _salt CreateSchema' {..} =
    _salt `Prelude.hashWithSalt` compatibility
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` schemaDefinition
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` dataFormat

instance Prelude.NFData CreateSchema where
  rnf CreateSchema' {..} =
    Prelude.rnf compatibility
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf schemaDefinition
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf dataFormat

instance Data.ToHeaders CreateSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateSchema" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Compatibility" Data..=) Prelude.<$> compatibility,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Description" Data..=) Prelude.<$> description,
            ("RegistryId" Data..=) Prelude.<$> registryId,
            ("SchemaDefinition" Data..=)
              Prelude.<$> schemaDefinition,
            Prelude.Just ("SchemaName" Data..= schemaName),
            Prelude.Just ("DataFormat" Data..= dataFormat)
          ]
      )

instance Data.ToPath CreateSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The schema compatibility mode.
    compatibility :: Prelude.Maybe Compatibility,
    -- | The tags for the schema.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the registry.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The status of the schema.
    schemaStatus :: Prelude.Maybe SchemaStatus,
    -- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
    -- @PROTOBUF@ are supported.
    dataFormat :: Prelude.Maybe DataFormat,
    -- | The status of the first schema version created.
    schemaVersionStatus :: Prelude.Maybe SchemaVersionStatus,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | A description of the schema if specified when created.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Prelude.Maybe Prelude.Text,
    -- | The next version of the schema associated with the returned schema
    -- definition.
    nextSchemaVersion :: Prelude.Maybe Prelude.Natural,
    -- | The version number of the checkpoint (the last time the compatibility
    -- mode was changed).
    schemaCheckpoint :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the first schema version.
    schemaVersionId :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the schema associated with the returned schema
    -- definition.
    latestSchemaVersion :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibility', 'createSchemaResponse_compatibility' - The schema compatibility mode.
--
-- 'tags', 'createSchemaResponse_tags' - The tags for the schema.
--
-- 'registryName', 'createSchemaResponse_registryName' - The name of the registry.
--
-- 'schemaStatus', 'createSchemaResponse_schemaStatus' - The status of the schema.
--
-- 'dataFormat', 'createSchemaResponse_dataFormat' - The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
--
-- 'schemaVersionStatus', 'createSchemaResponse_schemaVersionStatus' - The status of the first schema version created.
--
-- 'schemaName', 'createSchemaResponse_schemaName' - The name of the schema.
--
-- 'description', 'createSchemaResponse_description' - A description of the schema if specified when created.
--
-- 'schemaArn', 'createSchemaResponse_schemaArn' - The Amazon Resource Name (ARN) of the schema.
--
-- 'registryArn', 'createSchemaResponse_registryArn' - The Amazon Resource Name (ARN) of the registry.
--
-- 'nextSchemaVersion', 'createSchemaResponse_nextSchemaVersion' - The next version of the schema associated with the returned schema
-- definition.
--
-- 'schemaCheckpoint', 'createSchemaResponse_schemaCheckpoint' - The version number of the checkpoint (the last time the compatibility
-- mode was changed).
--
-- 'schemaVersionId', 'createSchemaResponse_schemaVersionId' - The unique identifier of the first schema version.
--
-- 'latestSchemaVersion', 'createSchemaResponse_latestSchemaVersion' - The latest version of the schema associated with the returned schema
-- definition.
--
-- 'httpStatus', 'createSchemaResponse_httpStatus' - The response's http status code.
newCreateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSchemaResponse
newCreateSchemaResponse pHttpStatus_ =
  CreateSchemaResponse'
    { compatibility =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      registryName = Prelude.Nothing,
      schemaStatus = Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      schemaVersionStatus = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      description = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      registryArn = Prelude.Nothing,
      nextSchemaVersion = Prelude.Nothing,
      schemaCheckpoint = Prelude.Nothing,
      schemaVersionId = Prelude.Nothing,
      latestSchemaVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The schema compatibility mode.
createSchemaResponse_compatibility :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Compatibility)
createSchemaResponse_compatibility = Lens.lens (\CreateSchemaResponse' {compatibility} -> compatibility) (\s@CreateSchemaResponse' {} a -> s {compatibility = a} :: CreateSchemaResponse)

-- | The tags for the schema.
createSchemaResponse_tags :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSchemaResponse_tags = Lens.lens (\CreateSchemaResponse' {tags} -> tags) (\s@CreateSchemaResponse' {} a -> s {tags = a} :: CreateSchemaResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the registry.
createSchemaResponse_registryName :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_registryName = Lens.lens (\CreateSchemaResponse' {registryName} -> registryName) (\s@CreateSchemaResponse' {} a -> s {registryName = a} :: CreateSchemaResponse)

-- | The status of the schema.
createSchemaResponse_schemaStatus :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe SchemaStatus)
createSchemaResponse_schemaStatus = Lens.lens (\CreateSchemaResponse' {schemaStatus} -> schemaStatus) (\s@CreateSchemaResponse' {} a -> s {schemaStatus = a} :: CreateSchemaResponse)

-- | The data format of the schema definition. Currently @AVRO@, @JSON@ and
-- @PROTOBUF@ are supported.
createSchemaResponse_dataFormat :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe DataFormat)
createSchemaResponse_dataFormat = Lens.lens (\CreateSchemaResponse' {dataFormat} -> dataFormat) (\s@CreateSchemaResponse' {} a -> s {dataFormat = a} :: CreateSchemaResponse)

-- | The status of the first schema version created.
createSchemaResponse_schemaVersionStatus :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe SchemaVersionStatus)
createSchemaResponse_schemaVersionStatus = Lens.lens (\CreateSchemaResponse' {schemaVersionStatus} -> schemaVersionStatus) (\s@CreateSchemaResponse' {} a -> s {schemaVersionStatus = a} :: CreateSchemaResponse)

-- | The name of the schema.
createSchemaResponse_schemaName :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaName = Lens.lens (\CreateSchemaResponse' {schemaName} -> schemaName) (\s@CreateSchemaResponse' {} a -> s {schemaName = a} :: CreateSchemaResponse)

-- | A description of the schema if specified when created.
createSchemaResponse_description :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_description = Lens.lens (\CreateSchemaResponse' {description} -> description) (\s@CreateSchemaResponse' {} a -> s {description = a} :: CreateSchemaResponse)

-- | The Amazon Resource Name (ARN) of the schema.
createSchemaResponse_schemaArn :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaArn = Lens.lens (\CreateSchemaResponse' {schemaArn} -> schemaArn) (\s@CreateSchemaResponse' {} a -> s {schemaArn = a} :: CreateSchemaResponse)

-- | The Amazon Resource Name (ARN) of the registry.
createSchemaResponse_registryArn :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_registryArn = Lens.lens (\CreateSchemaResponse' {registryArn} -> registryArn) (\s@CreateSchemaResponse' {} a -> s {registryArn = a} :: CreateSchemaResponse)

-- | The next version of the schema associated with the returned schema
-- definition.
createSchemaResponse_nextSchemaVersion :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Natural)
createSchemaResponse_nextSchemaVersion = Lens.lens (\CreateSchemaResponse' {nextSchemaVersion} -> nextSchemaVersion) (\s@CreateSchemaResponse' {} a -> s {nextSchemaVersion = a} :: CreateSchemaResponse)

-- | The version number of the checkpoint (the last time the compatibility
-- mode was changed).
createSchemaResponse_schemaCheckpoint :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Natural)
createSchemaResponse_schemaCheckpoint = Lens.lens (\CreateSchemaResponse' {schemaCheckpoint} -> schemaCheckpoint) (\s@CreateSchemaResponse' {} a -> s {schemaCheckpoint = a} :: CreateSchemaResponse)

-- | The unique identifier of the first schema version.
createSchemaResponse_schemaVersionId :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaVersionId = Lens.lens (\CreateSchemaResponse' {schemaVersionId} -> schemaVersionId) (\s@CreateSchemaResponse' {} a -> s {schemaVersionId = a} :: CreateSchemaResponse)

-- | The latest version of the schema associated with the returned schema
-- definition.
createSchemaResponse_latestSchemaVersion :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Natural)
createSchemaResponse_latestSchemaVersion = Lens.lens (\CreateSchemaResponse' {latestSchemaVersion} -> latestSchemaVersion) (\s@CreateSchemaResponse' {} a -> s {latestSchemaVersion = a} :: CreateSchemaResponse)

-- | The response's http status code.
createSchemaResponse_httpStatus :: Lens.Lens' CreateSchemaResponse Prelude.Int
createSchemaResponse_httpStatus = Lens.lens (\CreateSchemaResponse' {httpStatus} -> httpStatus) (\s@CreateSchemaResponse' {} a -> s {httpStatus = a} :: CreateSchemaResponse)

instance Prelude.NFData CreateSchemaResponse where
  rnf CreateSchemaResponse' {..} =
    Prelude.rnf compatibility
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaStatus
      `Prelude.seq` Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf schemaVersionStatus
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf registryArn
      `Prelude.seq` Prelude.rnf nextSchemaVersion
      `Prelude.seq` Prelude.rnf schemaCheckpoint
      `Prelude.seq` Prelude.rnf schemaVersionId
      `Prelude.seq` Prelude.rnf latestSchemaVersion
      `Prelude.seq` Prelude.rnf httpStatus
