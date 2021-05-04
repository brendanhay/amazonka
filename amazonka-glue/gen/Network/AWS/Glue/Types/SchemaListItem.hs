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
-- Module      : Network.AWS.Glue.Types.SchemaListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaListItem where

import Network.AWS.Glue.Types.SchemaStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains minimal details for a schema.
--
-- /See:/ 'newSchemaListItem' smart constructor.
data SchemaListItem = SchemaListItem'
  { -- | The Amazon Resource Name (ARN) for the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a schema was updated.
    updatedTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a schema was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | the name of the registry where the schema resides.
    registryName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | A description for the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | The status of the schema.
    schemaStatus :: Prelude.Maybe SchemaStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SchemaListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'schemaListItem_schemaArn' - The Amazon Resource Name (ARN) for the schema.
--
-- 'updatedTime', 'schemaListItem_updatedTime' - The date and time that a schema was updated.
--
-- 'createdTime', 'schemaListItem_createdTime' - The date and time that a schema was created.
--
-- 'registryName', 'schemaListItem_registryName' - the name of the registry where the schema resides.
--
-- 'schemaName', 'schemaListItem_schemaName' - The name of the schema.
--
-- 'description', 'schemaListItem_description' - A description for the schema.
--
-- 'schemaStatus', 'schemaListItem_schemaStatus' - The status of the schema.
newSchemaListItem ::
  SchemaListItem
newSchemaListItem =
  SchemaListItem'
    { schemaArn = Prelude.Nothing,
      updatedTime = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      registryName = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      description = Prelude.Nothing,
      schemaStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the schema.
schemaListItem_schemaArn :: Lens.Lens' SchemaListItem (Prelude.Maybe Prelude.Text)
schemaListItem_schemaArn = Lens.lens (\SchemaListItem' {schemaArn} -> schemaArn) (\s@SchemaListItem' {} a -> s {schemaArn = a} :: SchemaListItem)

-- | The date and time that a schema was updated.
schemaListItem_updatedTime :: Lens.Lens' SchemaListItem (Prelude.Maybe Prelude.Text)
schemaListItem_updatedTime = Lens.lens (\SchemaListItem' {updatedTime} -> updatedTime) (\s@SchemaListItem' {} a -> s {updatedTime = a} :: SchemaListItem)

-- | The date and time that a schema was created.
schemaListItem_createdTime :: Lens.Lens' SchemaListItem (Prelude.Maybe Prelude.Text)
schemaListItem_createdTime = Lens.lens (\SchemaListItem' {createdTime} -> createdTime) (\s@SchemaListItem' {} a -> s {createdTime = a} :: SchemaListItem)

-- | the name of the registry where the schema resides.
schemaListItem_registryName :: Lens.Lens' SchemaListItem (Prelude.Maybe Prelude.Text)
schemaListItem_registryName = Lens.lens (\SchemaListItem' {registryName} -> registryName) (\s@SchemaListItem' {} a -> s {registryName = a} :: SchemaListItem)

-- | The name of the schema.
schemaListItem_schemaName :: Lens.Lens' SchemaListItem (Prelude.Maybe Prelude.Text)
schemaListItem_schemaName = Lens.lens (\SchemaListItem' {schemaName} -> schemaName) (\s@SchemaListItem' {} a -> s {schemaName = a} :: SchemaListItem)

-- | A description for the schema.
schemaListItem_description :: Lens.Lens' SchemaListItem (Prelude.Maybe Prelude.Text)
schemaListItem_description = Lens.lens (\SchemaListItem' {description} -> description) (\s@SchemaListItem' {} a -> s {description = a} :: SchemaListItem)

-- | The status of the schema.
schemaListItem_schemaStatus :: Lens.Lens' SchemaListItem (Prelude.Maybe SchemaStatus)
schemaListItem_schemaStatus = Lens.lens (\SchemaListItem' {schemaStatus} -> schemaStatus) (\s@SchemaListItem' {} a -> s {schemaStatus = a} :: SchemaListItem)

instance Prelude.FromJSON SchemaListItem where
  parseJSON =
    Prelude.withObject
      "SchemaListItem"
      ( \x ->
          SchemaListItem'
            Prelude.<$> (x Prelude..:? "SchemaArn")
            Prelude.<*> (x Prelude..:? "UpdatedTime")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "RegistryName")
            Prelude.<*> (x Prelude..:? "SchemaName")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "SchemaStatus")
      )

instance Prelude.Hashable SchemaListItem

instance Prelude.NFData SchemaListItem
