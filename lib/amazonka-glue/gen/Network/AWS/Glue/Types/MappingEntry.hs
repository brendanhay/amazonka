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
    meTargetTable,
    meSourceType,
    meSourceTable,
    meTargetType,
    meTargetPath,
    meSourcePath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines a mapping.
--
-- /See:/ 'mkMappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { -- | The target table.
    targetTable :: Lude.Maybe Lude.Text,
    -- | The source type.
    sourceType :: Lude.Maybe Lude.Text,
    -- | The name of the source table.
    sourceTable :: Lude.Maybe Lude.Text,
    -- | The target type.
    targetType :: Lude.Maybe Lude.Text,
    -- | The target path.
    targetPath :: Lude.Maybe Lude.Text,
    -- | The source path.
    sourcePath :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MappingEntry' with the minimum fields required to make a request.
--
-- * 'targetTable' - The target table.
-- * 'sourceType' - The source type.
-- * 'sourceTable' - The name of the source table.
-- * 'targetType' - The target type.
-- * 'targetPath' - The target path.
-- * 'sourcePath' - The source path.
mkMappingEntry ::
  MappingEntry
mkMappingEntry =
  MappingEntry'
    { targetTable = Lude.Nothing,
      sourceType = Lude.Nothing,
      sourceTable = Lude.Nothing,
      targetType = Lude.Nothing,
      targetPath = Lude.Nothing,
      sourcePath = Lude.Nothing
    }

-- | The target table.
--
-- /Note:/ Consider using 'targetTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meTargetTable :: Lens.Lens' MappingEntry (Lude.Maybe Lude.Text)
meTargetTable = Lens.lens (targetTable :: MappingEntry -> Lude.Maybe Lude.Text) (\s a -> s {targetTable = a} :: MappingEntry)
{-# DEPRECATED meTargetTable "Use generic-lens or generic-optics with 'targetTable' instead." #-}

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSourceType :: Lens.Lens' MappingEntry (Lude.Maybe Lude.Text)
meSourceType = Lens.lens (sourceType :: MappingEntry -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: MappingEntry)
{-# DEPRECATED meSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The name of the source table.
--
-- /Note:/ Consider using 'sourceTable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSourceTable :: Lens.Lens' MappingEntry (Lude.Maybe Lude.Text)
meSourceTable = Lens.lens (sourceTable :: MappingEntry -> Lude.Maybe Lude.Text) (\s a -> s {sourceTable = a} :: MappingEntry)
{-# DEPRECATED meSourceTable "Use generic-lens or generic-optics with 'sourceTable' instead." #-}

-- | The target type.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meTargetType :: Lens.Lens' MappingEntry (Lude.Maybe Lude.Text)
meTargetType = Lens.lens (targetType :: MappingEntry -> Lude.Maybe Lude.Text) (\s a -> s {targetType = a} :: MappingEntry)
{-# DEPRECATED meTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | The target path.
--
-- /Note:/ Consider using 'targetPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meTargetPath :: Lens.Lens' MappingEntry (Lude.Maybe Lude.Text)
meTargetPath = Lens.lens (targetPath :: MappingEntry -> Lude.Maybe Lude.Text) (\s a -> s {targetPath = a} :: MappingEntry)
{-# DEPRECATED meTargetPath "Use generic-lens or generic-optics with 'targetPath' instead." #-}

-- | The source path.
--
-- /Note:/ Consider using 'sourcePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSourcePath :: Lens.Lens' MappingEntry (Lude.Maybe Lude.Text)
meSourcePath = Lens.lens (sourcePath :: MappingEntry -> Lude.Maybe Lude.Text) (\s a -> s {sourcePath = a} :: MappingEntry)
{-# DEPRECATED meSourcePath "Use generic-lens or generic-optics with 'sourcePath' instead." #-}

instance Lude.FromJSON MappingEntry where
  parseJSON =
    Lude.withObject
      "MappingEntry"
      ( \x ->
          MappingEntry'
            Lude.<$> (x Lude..:? "TargetTable")
            Lude.<*> (x Lude..:? "SourceType")
            Lude.<*> (x Lude..:? "SourceTable")
            Lude.<*> (x Lude..:? "TargetType")
            Lude.<*> (x Lude..:? "TargetPath")
            Lude.<*> (x Lude..:? "SourcePath")
      )

instance Lude.ToJSON MappingEntry where
  toJSON MappingEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TargetTable" Lude..=) Lude.<$> targetTable,
            ("SourceType" Lude..=) Lude.<$> sourceType,
            ("SourceTable" Lude..=) Lude.<$> sourceTable,
            ("TargetType" Lude..=) Lude.<$> targetType,
            ("TargetPath" Lude..=) Lude.<$> targetPath,
            ("SourcePath" Lude..=) Lude.<$> sourcePath
          ]
      )
