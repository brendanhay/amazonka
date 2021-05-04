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
-- Module      : Network.AWS.Glue.Types.MappingEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MappingEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a mapping.
--
-- /See:/ 'newMappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { -- | The target type.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | The target table.
    targetTable :: Prelude.Maybe Prelude.Text,
    -- | The target path.
    targetPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the source table.
    sourceTable :: Prelude.Maybe Prelude.Text,
    -- | The source path.
    sourcePath :: Prelude.Maybe Prelude.Text,
    -- | The source type.
    sourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { targetType = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      targetPath = Prelude.Nothing,
      sourceTable = Prelude.Nothing,
      sourcePath = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | The target type.
mappingEntry_targetType :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetType = Lens.lens (\MappingEntry' {targetType} -> targetType) (\s@MappingEntry' {} a -> s {targetType = a} :: MappingEntry)

-- | The target table.
mappingEntry_targetTable :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetTable = Lens.lens (\MappingEntry' {targetTable} -> targetTable) (\s@MappingEntry' {} a -> s {targetTable = a} :: MappingEntry)

-- | The target path.
mappingEntry_targetPath :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_targetPath = Lens.lens (\MappingEntry' {targetPath} -> targetPath) (\s@MappingEntry' {} a -> s {targetPath = a} :: MappingEntry)

-- | The name of the source table.
mappingEntry_sourceTable :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourceTable = Lens.lens (\MappingEntry' {sourceTable} -> sourceTable) (\s@MappingEntry' {} a -> s {sourceTable = a} :: MappingEntry)

-- | The source path.
mappingEntry_sourcePath :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourcePath = Lens.lens (\MappingEntry' {sourcePath} -> sourcePath) (\s@MappingEntry' {} a -> s {sourcePath = a} :: MappingEntry)

-- | The source type.
mappingEntry_sourceType :: Lens.Lens' MappingEntry (Prelude.Maybe Prelude.Text)
mappingEntry_sourceType = Lens.lens (\MappingEntry' {sourceType} -> sourceType) (\s@MappingEntry' {} a -> s {sourceType = a} :: MappingEntry)

instance Prelude.FromJSON MappingEntry where
  parseJSON =
    Prelude.withObject
      "MappingEntry"
      ( \x ->
          MappingEntry'
            Prelude.<$> (x Prelude..:? "TargetType")
            Prelude.<*> (x Prelude..:? "TargetTable")
            Prelude.<*> (x Prelude..:? "TargetPath")
            Prelude.<*> (x Prelude..:? "SourceTable")
            Prelude.<*> (x Prelude..:? "SourcePath")
            Prelude.<*> (x Prelude..:? "SourceType")
      )

instance Prelude.Hashable MappingEntry

instance Prelude.NFData MappingEntry

instance Prelude.ToJSON MappingEntry where
  toJSON MappingEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TargetType" Prelude..=) Prelude.<$> targetType,
            ("TargetTable" Prelude..=) Prelude.<$> targetTable,
            ("TargetPath" Prelude..=) Prelude.<$> targetPath,
            ("SourceTable" Prelude..=) Prelude.<$> sourceTable,
            ("SourcePath" Prelude..=) Prelude.<$> sourcePath,
            ("SourceType" Prelude..=) Prelude.<$> sourceType
          ]
      )
