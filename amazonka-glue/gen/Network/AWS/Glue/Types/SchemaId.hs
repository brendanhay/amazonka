{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaId where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The unique ID of the schema in the AWS Glue schema registry.
--
-- /See:/ 'newSchemaId' smart constructor.
data SchemaId = SchemaId'
  { -- | The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
    -- @SchemaName@ has to be provided.
    schemaArn :: Core.Maybe Core.Text,
    -- | The name of the schema registry that contains the schema.
    registryName :: Core.Maybe Core.Text,
    -- | The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
    -- provided.
    schemaName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SchemaId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'schemaId_schemaArn' - The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
-- @SchemaName@ has to be provided.
--
-- 'registryName', 'schemaId_registryName' - The name of the schema registry that contains the schema.
--
-- 'schemaName', 'schemaId_schemaName' - The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
-- provided.
newSchemaId ::
  SchemaId
newSchemaId =
  SchemaId'
    { schemaArn = Core.Nothing,
      registryName = Core.Nothing,
      schemaName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
-- @SchemaName@ has to be provided.
schemaId_schemaArn :: Lens.Lens' SchemaId (Core.Maybe Core.Text)
schemaId_schemaArn = Lens.lens (\SchemaId' {schemaArn} -> schemaArn) (\s@SchemaId' {} a -> s {schemaArn = a} :: SchemaId)

-- | The name of the schema registry that contains the schema.
schemaId_registryName :: Lens.Lens' SchemaId (Core.Maybe Core.Text)
schemaId_registryName = Lens.lens (\SchemaId' {registryName} -> registryName) (\s@SchemaId' {} a -> s {registryName = a} :: SchemaId)

-- | The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
-- provided.
schemaId_schemaName :: Lens.Lens' SchemaId (Core.Maybe Core.Text)
schemaId_schemaName = Lens.lens (\SchemaId' {schemaName} -> schemaName) (\s@SchemaId' {} a -> s {schemaName = a} :: SchemaId)

instance Core.FromJSON SchemaId where
  parseJSON =
    Core.withObject
      "SchemaId"
      ( \x ->
          SchemaId'
            Core.<$> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "RegistryName")
            Core.<*> (x Core..:? "SchemaName")
      )

instance Core.Hashable SchemaId

instance Core.NFData SchemaId

instance Core.ToJSON SchemaId where
  toJSON SchemaId' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaArn" Core..=) Core.<$> schemaArn,
            ("RegistryName" Core..=) Core.<$> registryName,
            ("SchemaName" Core..=) Core.<$> schemaName
          ]
      )
