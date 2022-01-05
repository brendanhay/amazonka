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
-- Module      : Amazonka.Schemas.CreateSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a schema definition.
--
-- Inactive schemas will be deleted after two years.
module Amazonka.Schemas.CreateSchema
  ( -- * Creating a Request
    CreateSchema (..),
    newCreateSchema,

    -- * Request Lenses
    createSchema_description,
    createSchema_tags,
    createSchema_registryName,
    createSchema_schemaName,
    createSchema_type,
    createSchema_content,

    -- * Destructuring the Response
    CreateSchemaResponse (..),
    newCreateSchemaResponse,

    -- * Response Lenses
    createSchemaResponse_schemaVersion,
    createSchemaResponse_schemaName,
    createSchemaResponse_schemaArn,
    createSchemaResponse_type,
    createSchemaResponse_lastModified,
    createSchemaResponse_description,
    createSchemaResponse_versionCreatedDate,
    createSchemaResponse_tags,
    createSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newCreateSchema' smart constructor.
data CreateSchema = CreateSchema'
  { -- | A description of the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the schema.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Text,
    -- | The type of schema.
    type' :: Type,
    -- | The source of the schema definition.
    content :: Prelude.Text
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
-- 'description', 'createSchema_description' - A description of the schema.
--
-- 'tags', 'createSchema_tags' - Tags associated with the schema.
--
-- 'registryName', 'createSchema_registryName' - The name of the registry.
--
-- 'schemaName', 'createSchema_schemaName' - The name of the schema.
--
-- 'type'', 'createSchema_type' - The type of schema.
--
-- 'content', 'createSchema_content' - The source of the schema definition.
newCreateSchema ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  -- | 'type''
  Type ->
  -- | 'content'
  Prelude.Text ->
  CreateSchema
newCreateSchema
  pRegistryName_
  pSchemaName_
  pType_
  pContent_ =
    CreateSchema'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        registryName = pRegistryName_,
        schemaName = pSchemaName_,
        type' = pType_,
        content = pContent_
      }

-- | A description of the schema.
createSchema_description :: Lens.Lens' CreateSchema (Prelude.Maybe Prelude.Text)
createSchema_description = Lens.lens (\CreateSchema' {description} -> description) (\s@CreateSchema' {} a -> s {description = a} :: CreateSchema)

-- | Tags associated with the schema.
createSchema_tags :: Lens.Lens' CreateSchema (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSchema_tags = Lens.lens (\CreateSchema' {tags} -> tags) (\s@CreateSchema' {} a -> s {tags = a} :: CreateSchema) Prelude.. Lens.mapping Lens.coerced

-- | The name of the registry.
createSchema_registryName :: Lens.Lens' CreateSchema Prelude.Text
createSchema_registryName = Lens.lens (\CreateSchema' {registryName} -> registryName) (\s@CreateSchema' {} a -> s {registryName = a} :: CreateSchema)

-- | The name of the schema.
createSchema_schemaName :: Lens.Lens' CreateSchema Prelude.Text
createSchema_schemaName = Lens.lens (\CreateSchema' {schemaName} -> schemaName) (\s@CreateSchema' {} a -> s {schemaName = a} :: CreateSchema)

-- | The type of schema.
createSchema_type :: Lens.Lens' CreateSchema Type
createSchema_type = Lens.lens (\CreateSchema' {type'} -> type') (\s@CreateSchema' {} a -> s {type' = a} :: CreateSchema)

-- | The source of the schema definition.
createSchema_content :: Lens.Lens' CreateSchema Prelude.Text
createSchema_content = Lens.lens (\CreateSchema' {content} -> content) (\s@CreateSchema' {} a -> s {content = a} :: CreateSchema)

instance Core.AWSRequest CreateSchema where
  type AWSResponse CreateSchema = CreateSchemaResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSchemaResponse'
            Prelude.<$> (x Core..?> "SchemaVersion")
            Prelude.<*> (x Core..?> "SchemaName")
            Prelude.<*> (x Core..?> "SchemaArn")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "LastModified")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "VersionCreatedDate")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSchema where
  hashWithSalt _salt CreateSchema' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` content

instance Prelude.NFData CreateSchema where
  rnf CreateSchema' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf content

instance Core.ToHeaders CreateSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSchema where
  toJSON CreateSchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Content" Core..= content)
          ]
      )

instance Core.ToPath CreateSchema where
  toPath CreateSchema' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Core.toBS registryName,
        "/schemas/name/",
        Core.toBS schemaName
      ]

instance Core.ToQuery CreateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSchemaResponse' smart constructor.
data CreateSchemaResponse = CreateSchemaResponse'
  { -- | The version number of the schema
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the schema.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The date and time that schema was modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | The description of the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date the schema version was created.
    versionCreatedDate :: Prelude.Maybe Core.POSIX,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'schemaVersion', 'createSchemaResponse_schemaVersion' - The version number of the schema
--
-- 'schemaName', 'createSchemaResponse_schemaName' - The name of the schema.
--
-- 'schemaArn', 'createSchemaResponse_schemaArn' - The ARN of the schema.
--
-- 'type'', 'createSchemaResponse_type' - The type of the schema.
--
-- 'lastModified', 'createSchemaResponse_lastModified' - The date and time that schema was modified.
--
-- 'description', 'createSchemaResponse_description' - The description of the schema.
--
-- 'versionCreatedDate', 'createSchemaResponse_versionCreatedDate' - The date the schema version was created.
--
-- 'tags', 'createSchemaResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createSchemaResponse_httpStatus' - The response's http status code.
newCreateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSchemaResponse
newCreateSchemaResponse pHttpStatus_ =
  CreateSchemaResponse'
    { schemaVersion =
        Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      description = Prelude.Nothing,
      versionCreatedDate = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version number of the schema
createSchemaResponse_schemaVersion :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaVersion = Lens.lens (\CreateSchemaResponse' {schemaVersion} -> schemaVersion) (\s@CreateSchemaResponse' {} a -> s {schemaVersion = a} :: CreateSchemaResponse)

-- | The name of the schema.
createSchemaResponse_schemaName :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaName = Lens.lens (\CreateSchemaResponse' {schemaName} -> schemaName) (\s@CreateSchemaResponse' {} a -> s {schemaName = a} :: CreateSchemaResponse)

-- | The ARN of the schema.
createSchemaResponse_schemaArn :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_schemaArn = Lens.lens (\CreateSchemaResponse' {schemaArn} -> schemaArn) (\s@CreateSchemaResponse' {} a -> s {schemaArn = a} :: CreateSchemaResponse)

-- | The type of the schema.
createSchemaResponse_type :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_type = Lens.lens (\CreateSchemaResponse' {type'} -> type') (\s@CreateSchemaResponse' {} a -> s {type' = a} :: CreateSchemaResponse)

-- | The date and time that schema was modified.
createSchemaResponse_lastModified :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.UTCTime)
createSchemaResponse_lastModified = Lens.lens (\CreateSchemaResponse' {lastModified} -> lastModified) (\s@CreateSchemaResponse' {} a -> s {lastModified = a} :: CreateSchemaResponse) Prelude.. Lens.mapping Core._Time

-- | The description of the schema.
createSchemaResponse_description :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.Text)
createSchemaResponse_description = Lens.lens (\CreateSchemaResponse' {description} -> description) (\s@CreateSchemaResponse' {} a -> s {description = a} :: CreateSchemaResponse)

-- | The date the schema version was created.
createSchemaResponse_versionCreatedDate :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe Prelude.UTCTime)
createSchemaResponse_versionCreatedDate = Lens.lens (\CreateSchemaResponse' {versionCreatedDate} -> versionCreatedDate) (\s@CreateSchemaResponse' {} a -> s {versionCreatedDate = a} :: CreateSchemaResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
createSchemaResponse_tags :: Lens.Lens' CreateSchemaResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSchemaResponse_tags = Lens.lens (\CreateSchemaResponse' {tags} -> tags) (\s@CreateSchemaResponse' {} a -> s {tags = a} :: CreateSchemaResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSchemaResponse_httpStatus :: Lens.Lens' CreateSchemaResponse Prelude.Int
createSchemaResponse_httpStatus = Lens.lens (\CreateSchemaResponse' {httpStatus} -> httpStatus) (\s@CreateSchemaResponse' {} a -> s {httpStatus = a} :: CreateSchemaResponse)

instance Prelude.NFData CreateSchemaResponse where
  rnf CreateSchemaResponse' {..} =
    Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionCreatedDate
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
