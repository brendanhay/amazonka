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
-- Module      : Network.AWS.Glue.Types.MappingEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MappingEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines a mapping.
--
-- /See:/ 'newMappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { -- | The target type.
    targetType :: Core.Maybe Core.Text,
    -- | The target table.
    targetTable :: Core.Maybe Core.Text,
    -- | The target path.
    targetPath :: Core.Maybe Core.Text,
    -- | The name of the source table.
    sourceTable :: Core.Maybe Core.Text,
    -- | The source path.
    sourcePath :: Core.Maybe Core.Text,
    -- | The source type.
    sourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MappingEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetType', 'mappingEntry_targetType' - The target type.
--
-- 'targetTable', 'mappingEntry_targetTable' - The target table.
--
-- 'targetPath', 'mappingEntry_targetPath' - The target path.
--
-- 'sourceTable', 'mappingEntry_sourceTable' - The name of the source table.
--
-- 'sourcePath', 'mappingEntry_sourcePath' - The source path.
--
-- 'sourceType', 'mappingEntry_sourceType' - The source type.
newMappingEntry ::
  MappingEntry
newMappingEntry =
  MappingEntry'
    { targetType = Core.Nothing,
      targetTable = Core.Nothing,
      targetPath = Core.Nothing,
      sourceTable = Core.Nothing,
      sourcePath = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | The target type.
mappingEntry_targetType :: Lens.Lens' MappingEntry (Core.Maybe Core.Text)
mappingEntry_targetType = Lens.lens (\MappingEntry' {targetType} -> targetType) (\s@MappingEntry' {} a -> s {targetType = a} :: MappingEntry)

-- | The target table.
mappingEntry_targetTable :: Lens.Lens' MappingEntry (Core.Maybe Core.Text)
mappingEntry_targetTable = Lens.lens (\MappingEntry' {targetTable} -> targetTable) (\s@MappingEntry' {} a -> s {targetTable = a} :: MappingEntry)

-- | The target path.
mappingEntry_targetPath :: Lens.Lens' MappingEntry (Core.Maybe Core.Text)
mappingEntry_targetPath = Lens.lens (\MappingEntry' {targetPath} -> targetPath) (\s@MappingEntry' {} a -> s {targetPath = a} :: MappingEntry)

-- | The name of the source table.
mappingEntry_sourceTable :: Lens.Lens' MappingEntry (Core.Maybe Core.Text)
mappingEntry_sourceTable = Lens.lens (\MappingEntry' {sourceTable} -> sourceTable) (\s@MappingEntry' {} a -> s {sourceTable = a} :: MappingEntry)

-- | The source path.
mappingEntry_sourcePath :: Lens.Lens' MappingEntry (Core.Maybe Core.Text)
mappingEntry_sourcePath = Lens.lens (\MappingEntry' {sourcePath} -> sourcePath) (\s@MappingEntry' {} a -> s {sourcePath = a} :: MappingEntry)

-- | The source type.
mappingEntry_sourceType :: Lens.Lens' MappingEntry (Core.Maybe Core.Text)
mappingEntry_sourceType = Lens.lens (\MappingEntry' {sourceType} -> sourceType) (\s@MappingEntry' {} a -> s {sourceType = a} :: MappingEntry)

instance Core.FromJSON MappingEntry where
  parseJSON =
    Core.withObject
      "MappingEntry"
      ( \x ->
          MappingEntry'
            Core.<$> (x Core..:? "TargetType")
            Core.<*> (x Core..:? "TargetTable")
            Core.<*> (x Core..:? "TargetPath")
            Core.<*> (x Core..:? "SourceTable")
            Core.<*> (x Core..:? "SourcePath")
            Core.<*> (x Core..:? "SourceType")
      )

instance Core.Hashable MappingEntry

instance Core.NFData MappingEntry

instance Core.ToJSON MappingEntry where
  toJSON MappingEntry' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TargetType" Core..=) Core.<$> targetType,
            ("TargetTable" Core..=) Core.<$> targetTable,
            ("TargetPath" Core..=) Core.<$> targetPath,
            ("SourceTable" Core..=) Core.<$> sourceTable,
            ("SourcePath" Core..=) Core.<$> sourcePath,
            ("SourceType" Core..=) Core.<$> sourceType
          ]
      )
