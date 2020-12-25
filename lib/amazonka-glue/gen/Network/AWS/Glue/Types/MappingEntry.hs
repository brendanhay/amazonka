{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MappingEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MappingEntry
  ( MappingEntry (..),

    -- * Smart constructor
    mkMappingEntry,

    -- * Lenses
    meSourcePath,
    meSourceTable,
    meSourceType,
    meTargetPath,
    meTargetTable,
    meTargetType,
  )
where

import qualified Network.AWS.Glue.Types.SourcePath as Types
import qualified Network.AWS.Glue.Types.SourceTable as Types
import qualified Network.AWS.Glue.Types.SourceType as Types
import qualified Network.AWS.Glue.Types.TargetPath as Types
import qualified Network.AWS.Glue.Types.TargetTable as Types
import qualified Network.AWS.Glue.Types.TargetType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines a mapping.
--
-- /See:/ 'mkMappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { -- | The source path.
    sourcePath :: Core.Maybe Types.SourcePath,
    -- | The name of the source table.
    sourceTable :: Core.Maybe Types.SourceTable,
    -- | The source type.
    sourceType :: Core.Maybe Types.SourceType,
    -- | The target path.
    targetPath :: Core.Maybe Types.TargetPath,
    -- | The target table.
    targetTable :: Core.Maybe Types.TargetTable,
    -- | The target type.
    targetType :: Core.Maybe Types.TargetType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MappingEntry' value with any optional fields omitted.
mkMappingEntry ::
  MappingEntry
mkMappingEntry =
  MappingEntry'
    { sourcePath = Core.Nothing,
      sourceTable = Core.Nothing,
      sourceType = Core.Nothing,
      targetPath = Core.Nothing,
      targetTable = Core.Nothing,
      targetType = Core.Nothing
    }

-- | The source path.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSourcePath :: Lens.Lens' MappingEntry (Core.Maybe Types.SourcePath)
meSourcePath = Lens.field @"sourcePath"
{-# DEPRECATED meSourcePath "Use generic-lens or generic-optics with 'sourcePath' instead." #-}

-- | The name of the source table.
--
-- /Note:/ Consider using 'sourceTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSourceTable :: Lens.Lens' MappingEntry (Core.Maybe Types.SourceTable)
meSourceTable = Lens.field @"sourceTable"
{-# DEPRECATED meSourceTable "Use generic-lens or generic-optics with 'sourceTable' instead." #-}

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSourceType :: Lens.Lens' MappingEntry (Core.Maybe Types.SourceType)
meSourceType = Lens.field @"sourceType"
{-# DEPRECATED meSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The target path.
--
-- /Note:/ Consider using 'targetPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meTargetPath :: Lens.Lens' MappingEntry (Core.Maybe Types.TargetPath)
meTargetPath = Lens.field @"targetPath"
{-# DEPRECATED meTargetPath "Use generic-lens or generic-optics with 'targetPath' instead." #-}

-- | The target table.
--
-- /Note:/ Consider using 'targetTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meTargetTable :: Lens.Lens' MappingEntry (Core.Maybe Types.TargetTable)
meTargetTable = Lens.field @"targetTable"
{-# DEPRECATED meTargetTable "Use generic-lens or generic-optics with 'targetTable' instead." #-}

-- | The target type.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meTargetType :: Lens.Lens' MappingEntry (Core.Maybe Types.TargetType)
meTargetType = Lens.field @"targetType"
{-# DEPRECATED meTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

instance Core.FromJSON MappingEntry where
  toJSON MappingEntry {..} =
    Core.object
      ( Core.catMaybes
          [ ("SourcePath" Core..=) Core.<$> sourcePath,
            ("SourceTable" Core..=) Core.<$> sourceTable,
            ("SourceType" Core..=) Core.<$> sourceType,
            ("TargetPath" Core..=) Core.<$> targetPath,
            ("TargetTable" Core..=) Core.<$> targetTable,
            ("TargetType" Core..=) Core.<$> targetType
          ]
      )

instance Core.FromJSON MappingEntry where
  parseJSON =
    Core.withObject "MappingEntry" Core.$
      \x ->
        MappingEntry'
          Core.<$> (x Core..:? "SourcePath")
          Core.<*> (x Core..:? "SourceTable")
          Core.<*> (x Core..:? "SourceType")
          Core.<*> (x Core..:? "TargetPath")
          Core.<*> (x Core..:? "TargetTable")
          Core.<*> (x Core..:? "TargetType")
