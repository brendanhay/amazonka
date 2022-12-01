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
-- Module      : Amazonka.Glue.Types.SchemaId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The unique ID of the schema in the Glue schema registry.
--
-- /See:/ 'newSchemaId' smart constructor.
data SchemaId = SchemaId'
  { -- | The name of the schema registry that contains the schema.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
    -- provided.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
    -- @SchemaName@ has to be provided.
    schemaArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryName', 'schemaId_registryName' - The name of the schema registry that contains the schema.
--
-- 'schemaName', 'schemaId_schemaName' - The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
-- provided.
--
-- 'schemaArn', 'schemaId_schemaArn' - The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
-- @SchemaName@ has to be provided.
newSchemaId ::
  SchemaId
newSchemaId =
  SchemaId'
    { registryName = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaArn = Prelude.Nothing
    }

-- | The name of the schema registry that contains the schema.
schemaId_registryName :: Lens.Lens' SchemaId (Prelude.Maybe Prelude.Text)
schemaId_registryName = Lens.lens (\SchemaId' {registryName} -> registryName) (\s@SchemaId' {} a -> s {registryName = a} :: SchemaId)

-- | The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
-- provided.
schemaId_schemaName :: Lens.Lens' SchemaId (Prelude.Maybe Prelude.Text)
schemaId_schemaName = Lens.lens (\SchemaId' {schemaName} -> schemaName) (\s@SchemaId' {} a -> s {schemaName = a} :: SchemaId)

-- | The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
-- @SchemaName@ has to be provided.
schemaId_schemaArn :: Lens.Lens' SchemaId (Prelude.Maybe Prelude.Text)
schemaId_schemaArn = Lens.lens (\SchemaId' {schemaArn} -> schemaArn) (\s@SchemaId' {} a -> s {schemaArn = a} :: SchemaId)

instance Core.FromJSON SchemaId where
  parseJSON =
    Core.withObject
      "SchemaId"
      ( \x ->
          SchemaId'
            Prelude.<$> (x Core..:? "RegistryName")
            Prelude.<*> (x Core..:? "SchemaName")
            Prelude.<*> (x Core..:? "SchemaArn")
      )

instance Prelude.Hashable SchemaId where
  hashWithSalt _salt SchemaId' {..} =
    _salt `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData SchemaId where
  rnf SchemaId' {..} =
    Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf schemaArn

instance Core.ToJSON SchemaId where
  toJSON SchemaId' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RegistryName" Core..=) Prelude.<$> registryName,
            ("SchemaName" Core..=) Prelude.<$> schemaName,
            ("SchemaArn" Core..=) Prelude.<$> schemaArn
          ]
      )
