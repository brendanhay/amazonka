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
-- Module      : Amazonka.Glue.Types.Mapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Mapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the mapping of data property keys.
--
-- /See:/ 'newMapping' smart constructor.
data Mapping = Mapping'
  { -- | Only applicable to nested data structures. If you want to change the
    -- parent structure, but also one of its children, you can fill out this
    -- data strucutre. It is also @Mapping@, but its @FromPath@ will be the
    -- parent\'s @FromPath@ plus the @FromPath@ from this structure.
    --
    -- For the children part, suppose you have the structure:
    --
    -- @{ \"FromPath\": \"OuterStructure\", \"ToKey\": \"OuterStructure\", \"ToType\": \"Struct\", \"Dropped\": false, \"Chidlren\": [{ \"FromPath\": \"inner\", \"ToKey\": \"inner\", \"ToType\": \"Double\", \"Dropped\": false, }] }@
    --
    -- You can specify a @Mapping@ that looks like:
    --
    -- @{ \"FromPath\": \"OuterStructure\", \"ToKey\": \"OuterStructure\", \"ToType\": \"Struct\", \"Dropped\": false, \"Chidlren\": [{ \"FromPath\": \"inner\", \"ToKey\": \"inner\", \"ToType\": \"Double\", \"Dropped\": false, }] }@
    children :: Prelude.Maybe [Mapping],
    -- | If true, then the column is removed.
    dropped :: Prelude.Maybe Prelude.Bool,
    -- | The table or column to be modified.
    fromPath :: Prelude.Maybe [Prelude.Text],
    -- | The type of the data to be modified.
    fromType :: Prelude.Maybe Prelude.Text,
    -- | After the apply mapping, what the name of the column should be. Can be
    -- the same as @FromPath@.
    toKey :: Prelude.Maybe Prelude.Text,
    -- | The data type that the data is to be modified to.
    toType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Mapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'children', 'mapping_children' - Only applicable to nested data structures. If you want to change the
-- parent structure, but also one of its children, you can fill out this
-- data strucutre. It is also @Mapping@, but its @FromPath@ will be the
-- parent\'s @FromPath@ plus the @FromPath@ from this structure.
--
-- For the children part, suppose you have the structure:
--
-- @{ \"FromPath\": \"OuterStructure\", \"ToKey\": \"OuterStructure\", \"ToType\": \"Struct\", \"Dropped\": false, \"Chidlren\": [{ \"FromPath\": \"inner\", \"ToKey\": \"inner\", \"ToType\": \"Double\", \"Dropped\": false, }] }@
--
-- You can specify a @Mapping@ that looks like:
--
-- @{ \"FromPath\": \"OuterStructure\", \"ToKey\": \"OuterStructure\", \"ToType\": \"Struct\", \"Dropped\": false, \"Chidlren\": [{ \"FromPath\": \"inner\", \"ToKey\": \"inner\", \"ToType\": \"Double\", \"Dropped\": false, }] }@
--
-- 'dropped', 'mapping_dropped' - If true, then the column is removed.
--
-- 'fromPath', 'mapping_fromPath' - The table or column to be modified.
--
-- 'fromType', 'mapping_fromType' - The type of the data to be modified.
--
-- 'toKey', 'mapping_toKey' - After the apply mapping, what the name of the column should be. Can be
-- the same as @FromPath@.
--
-- 'toType', 'mapping_toType' - The data type that the data is to be modified to.
newMapping ::
  Mapping
newMapping =
  Mapping'
    { children = Prelude.Nothing,
      dropped = Prelude.Nothing,
      fromPath = Prelude.Nothing,
      fromType = Prelude.Nothing,
      toKey = Prelude.Nothing,
      toType = Prelude.Nothing
    }

-- | Only applicable to nested data structures. If you want to change the
-- parent structure, but also one of its children, you can fill out this
-- data strucutre. It is also @Mapping@, but its @FromPath@ will be the
-- parent\'s @FromPath@ plus the @FromPath@ from this structure.
--
-- For the children part, suppose you have the structure:
--
-- @{ \"FromPath\": \"OuterStructure\", \"ToKey\": \"OuterStructure\", \"ToType\": \"Struct\", \"Dropped\": false, \"Chidlren\": [{ \"FromPath\": \"inner\", \"ToKey\": \"inner\", \"ToType\": \"Double\", \"Dropped\": false, }] }@
--
-- You can specify a @Mapping@ that looks like:
--
-- @{ \"FromPath\": \"OuterStructure\", \"ToKey\": \"OuterStructure\", \"ToType\": \"Struct\", \"Dropped\": false, \"Chidlren\": [{ \"FromPath\": \"inner\", \"ToKey\": \"inner\", \"ToType\": \"Double\", \"Dropped\": false, }] }@
mapping_children :: Lens.Lens' Mapping (Prelude.Maybe [Mapping])
mapping_children = Lens.lens (\Mapping' {children} -> children) (\s@Mapping' {} a -> s {children = a} :: Mapping) Prelude.. Lens.mapping Lens.coerced

-- | If true, then the column is removed.
mapping_dropped :: Lens.Lens' Mapping (Prelude.Maybe Prelude.Bool)
mapping_dropped = Lens.lens (\Mapping' {dropped} -> dropped) (\s@Mapping' {} a -> s {dropped = a} :: Mapping)

-- | The table or column to be modified.
mapping_fromPath :: Lens.Lens' Mapping (Prelude.Maybe [Prelude.Text])
mapping_fromPath = Lens.lens (\Mapping' {fromPath} -> fromPath) (\s@Mapping' {} a -> s {fromPath = a} :: Mapping) Prelude.. Lens.mapping Lens.coerced

-- | The type of the data to be modified.
mapping_fromType :: Lens.Lens' Mapping (Prelude.Maybe Prelude.Text)
mapping_fromType = Lens.lens (\Mapping' {fromType} -> fromType) (\s@Mapping' {} a -> s {fromType = a} :: Mapping)

-- | After the apply mapping, what the name of the column should be. Can be
-- the same as @FromPath@.
mapping_toKey :: Lens.Lens' Mapping (Prelude.Maybe Prelude.Text)
mapping_toKey = Lens.lens (\Mapping' {toKey} -> toKey) (\s@Mapping' {} a -> s {toKey = a} :: Mapping)

-- | The data type that the data is to be modified to.
mapping_toType :: Lens.Lens' Mapping (Prelude.Maybe Prelude.Text)
mapping_toType = Lens.lens (\Mapping' {toType} -> toType) (\s@Mapping' {} a -> s {toType = a} :: Mapping)

instance Data.FromJSON Mapping where
  parseJSON =
    Data.withObject
      "Mapping"
      ( \x ->
          Mapping'
            Prelude.<$> (x Data..:? "Children" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Dropped")
            Prelude.<*> (x Data..:? "FromPath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FromType")
            Prelude.<*> (x Data..:? "ToKey")
            Prelude.<*> (x Data..:? "ToType")
      )

instance Prelude.Hashable Mapping where
  hashWithSalt _salt Mapping' {..} =
    _salt
      `Prelude.hashWithSalt` children
      `Prelude.hashWithSalt` dropped
      `Prelude.hashWithSalt` fromPath
      `Prelude.hashWithSalt` fromType
      `Prelude.hashWithSalt` toKey
      `Prelude.hashWithSalt` toType

instance Prelude.NFData Mapping where
  rnf Mapping' {..} =
    Prelude.rnf children
      `Prelude.seq` Prelude.rnf dropped
      `Prelude.seq` Prelude.rnf fromPath
      `Prelude.seq` Prelude.rnf fromType
      `Prelude.seq` Prelude.rnf toKey
      `Prelude.seq` Prelude.rnf toType

instance Data.ToJSON Mapping where
  toJSON Mapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Children" Data..=) Prelude.<$> children,
            ("Dropped" Data..=) Prelude.<$> dropped,
            ("FromPath" Data..=) Prelude.<$> fromPath,
            ("FromType" Data..=) Prelude.<$> fromType,
            ("ToKey" Data..=) Prelude.<$> toKey,
            ("ToType" Data..=) Prelude.<$> toType
          ]
      )
