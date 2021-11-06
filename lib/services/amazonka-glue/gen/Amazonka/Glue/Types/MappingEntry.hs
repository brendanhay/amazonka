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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MappingEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines a mapping.
--
-- /See:/ 'newMappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { -- | The target table.
    targetTable :: Prelude.Maybe Prelude.Text,
    -- | The source type.
    sourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the source table.
    sourceTable :: Prelude.Maybe Prelude.Text,
    -- | The target type.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | The target path.
    targetPath :: Prelude.Maybe Prelude.Text,
    -- | The source path.
    sourcePath :: Prelude.Maybe Prelude.Text
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
-- 'targetTable', 'mappingEntry_targetTable' - The target table.
--
-- 'sourceType', 'mappingEntry_sourceType' - The source type.
--
-- 'sourceTable', 'mappingEntry_sourceTable' - The name of the source table.
--
-- 'targetType', 'mappingEntry_targetType' - The target type.
--
-- 'targetPath', 'mappingEntry_targetPath' - The target path.
--
-- 'sourcePath', 'mappingEntry_sourcePath' - The source path.
newMappingEntry ::
  MappingEntry
newMappingEntry =
  MappingEntry'
    { targetTable = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceTable = Prelude.Nothing,
      targetType = Prelude.Nothing,
      targetPath = Prelude.Nothing,
      sourcePath = Prelude.Nothing
    }

-- | The target table.
mappingEntry_targetTable :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetTable = Lens.lens (\MappingEntry' {targetTable} -> targetTable) (\s@MappingEntry' {} a -> s {targetTable = a} :: MappingEntry)

-- | The source type.
mappingEntry_sourceType :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourceType = Lens.lens (\MappingEntry' {sourceType} -> sourceType) (\s@MappingEntry' {} a -> s {sourceType = a} :: MappingEntry)

-- | The name of the source table.
mappingEntry_sourceTable :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourceTable = Lens.lens (\MappingEntry' {sourceTable} -> sourceTable) (\s@MappingEntry' {} a -> s {sourceTable = a} :: MappingEntry)

-- | The target type.
mappingEntry_targetType :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetType = Lens.lens (\MappingEntry' {targetType} -> targetType) (\s@MappingEntry' {} a -> s {targetType = a} :: MappingEntry)

-- | The target path.
mappingEntry_targetPath :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetPath = Lens.lens (\MappingEntry' {targetPath} -> targetPath) (\s@MappingEntry' {} a -> s {targetPath = a} :: MappingEntry)

-- | The source path.
mappingEntry_sourcePath :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourcePath = Lens.lens (\MappingEntry' {sourcePath} -> sourcePath) (\s@MappingEntry' {} a -> s {sourcePath = a} :: MappingEntry)

instance Core.FromJSON MappingEntry where
  parseJSON =
    Core.withObject
      "MappingEntry"
      ( \x ->
          MappingEntry'
            Prelude.<$> (x Core..:? "TargetTable")
            Prelude.<*> (x Core..:? "SourceType")
            Prelude.<*> (x Core..:? "SourceTable")
            Prelude.<*> (x Core..:? "TargetType")
            Prelude.<*> (x Core..:? "TargetPath")
            Prelude.<*> (x Core..:? "SourcePath")
      )

instance Prelude.Hashable MappingEntry

instance Prelude.NFData MappingEntry

instance Core.ToJSON MappingEntry where
  toJSON MappingEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetTable" Core..=) Prelude.<$> targetTable,
            ("SourceType" Core..=) Prelude.<$> sourceType,
            ("SourceTable" Core..=) Prelude.<$> sourceTable,
            ("TargetType" Core..=) Prelude.<$> targetType,
            ("TargetPath" Core..=) Prelude.<$> targetPath,
            ("SourcePath" Core..=) Prelude.<$> sourcePath
          ]
      )
