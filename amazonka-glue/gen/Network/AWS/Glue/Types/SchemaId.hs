{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The unique ID of the schema in the AWS Glue schema registry.
--
-- /See:/ 'newSchemaId' smart constructor.
data SchemaId = SchemaId'
  { -- | The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
    -- @SchemaName@ has to be provided.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema registry that contains the schema.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
    -- provided.
    schemaName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { schemaArn = Prelude.Nothing,
      registryName = Prelude.Nothing,
      schemaName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or
-- @SchemaName@ has to be provided.
schemaId_schemaArn :: Lens.Lens' SchemaId (Prelude.Maybe Prelude.Text)
schemaId_schemaArn = Lens.lens (\SchemaId' {schemaArn} -> schemaArn) (\s@SchemaId' {} a -> s {schemaArn = a} :: SchemaId)

-- | The name of the schema registry that contains the schema.
schemaId_registryName :: Lens.Lens' SchemaId (Prelude.Maybe Prelude.Text)
schemaId_registryName = Lens.lens (\SchemaId' {registryName} -> registryName) (\s@SchemaId' {} a -> s {registryName = a} :: SchemaId)

-- | The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be
-- provided.
schemaId_schemaName :: Lens.Lens' SchemaId (Prelude.Maybe Prelude.Text)
schemaId_schemaName = Lens.lens (\SchemaId' {schemaName} -> schemaName) (\s@SchemaId' {} a -> s {schemaName = a} :: SchemaId)

instance Prelude.FromJSON SchemaId where
  parseJSON =
    Prelude.withObject
      "SchemaId"
      ( \x ->
          SchemaId'
            Prelude.<$> (x Prelude..:? "SchemaArn")
            Prelude.<*> (x Prelude..:? "RegistryName")
            Prelude.<*> (x Prelude..:? "SchemaName")
      )

instance Prelude.Hashable SchemaId

instance Prelude.NFData SchemaId

instance Prelude.ToJSON SchemaId where
  toJSON SchemaId' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SchemaArn" Prelude..=) Prelude.<$> schemaArn,
            ("RegistryName" Prelude..=) Prelude.<$> registryName,
            ("SchemaName" Prelude..=) Prelude.<$> schemaName
          ]
      )
