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
-- Module      : Amazonka.Glue.Types.MappingEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MappingEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a mapping.
--
-- /See:/ 'newMappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { -- | The source path.
    sourcePath :: Prelude.Maybe Prelude.Text,
    -- | The name of the source table.
    sourceTable :: Prelude.Maybe Prelude.Text,
    -- | The source type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The target path.
    targetPath :: Prelude.Maybe Prelude.Text,
    -- | The target table.
    targetTable :: Prelude.Maybe Prelude.Text,
    -- | The target type.
    targetType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MappingEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourcePath', 'mappingEntry_sourcePath' - The source path.
--
-- 'sourceTable', 'mappingEntry_sourceTable' - The name of the source table.
--
-- 'sourceType', 'mappingEntry_sourceType' - The source type.
--
-- 'targetPath', 'mappingEntry_targetPath' - The target path.
--
-- 'targetTable', 'mappingEntry_targetTable' - The target table.
--
-- 'targetType', 'mappingEntry_targetType' - The target type.
newMappingEntry ::
  MappingEntry
newMappingEntry =
  MappingEntry'
    { sourcePath = Prelude.Nothing,
      sourceTable = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      targetPath = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      targetType = Prelude.Nothing
    }

-- | The source path.
mappingEntry_sourcePath :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourcePath = Lens.lens (\MappingEntry' {sourcePath} -> sourcePath) (\s@MappingEntry' {} a -> s {sourcePath = a} :: MappingEntry)

-- | The name of the source table.
mappingEntry_sourceTable :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourceTable = Lens.lens (\MappingEntry' {sourceTable} -> sourceTable) (\s@MappingEntry' {} a -> s {sourceTable = a} :: MappingEntry)

-- | The source type.
mappingEntry_sourceType :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourceType = Lens.lens (\MappingEntry' {sourceType} -> sourceType) (\s@MappingEntry' {} a -> s {sourceType = a} :: MappingEntry)

-- | The target path.
mappingEntry_targetPath :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetPath = Lens.lens (\MappingEntry' {targetPath} -> targetPath) (\s@MappingEntry' {} a -> s {targetPath = a} :: MappingEntry)

-- | The target table.
mappingEntry_targetTable :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetTable = Lens.lens (\MappingEntry' {targetTable} -> targetTable) (\s@MappingEntry' {} a -> s {targetTable = a} :: MappingEntry)

-- | The target type.
mappingEntry_targetType :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetType = Lens.lens (\MappingEntry' {targetType} -> targetType) (\s@MappingEntry' {} a -> s {targetType = a} :: MappingEntry)

instance Data.FromJSON MappingEntry where
  parseJSON =
    Data.withObject
      "MappingEntry"
      ( \x ->
          MappingEntry'
            Prelude.<$> (x Data..:? "SourcePath")
            Prelude.<*> (x Data..:? "SourceTable")
            Prelude.<*> (x Data..:? "SourceType")
            Prelude.<*> (x Data..:? "TargetPath")
            Prelude.<*> (x Data..:? "TargetTable")
            Prelude.<*> (x Data..:? "TargetType")
      )

instance Prelude.Hashable MappingEntry where
  hashWithSalt _salt MappingEntry' {..} =
    _salt `Prelude.hashWithSalt` sourcePath
      `Prelude.hashWithSalt` sourceTable
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` targetPath
      `Prelude.hashWithSalt` targetTable
      `Prelude.hashWithSalt` targetType

instance Prelude.NFData MappingEntry where
  rnf MappingEntry' {..} =
    Prelude.rnf sourcePath
      `Prelude.seq` Prelude.rnf sourceTable
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf targetPath
      `Prelude.seq` Prelude.rnf targetTable
      `Prelude.seq` Prelude.rnf targetType

instance Data.ToJSON MappingEntry where
  toJSON MappingEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SourcePath" Data..=) Prelude.<$> sourcePath,
            ("SourceTable" Data..=) Prelude.<$> sourceTable,
            ("SourceType" Data..=) Prelude.<$> sourceType,
            ("TargetPath" Data..=) Prelude.<$> targetPath,
            ("TargetTable" Data..=) Prelude.<$> targetTable,
            ("TargetType" Data..=) Prelude.<$> targetType
          ]
      )
