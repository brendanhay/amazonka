{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetEntry
  ( DatasetEntry (..),

    -- * Smart constructor
    mkDatasetEntry,

    -- * Lenses
    deDataURI,
    deEntryName,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.EntryName as Types
import qualified Network.AWS.IoTAnalytics.Types.PresignedURI as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The reference to a data set entry.
--
-- /See:/ 'mkDatasetEntry' smart constructor.
data DatasetEntry = DatasetEntry'
  { -- | The presigned URI of the data set item.
    dataURI :: Core.Maybe Types.PresignedURI,
    -- | The name of the data set item.
    entryName :: Core.Maybe Types.EntryName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetEntry' value with any optional fields omitted.
mkDatasetEntry ::
  DatasetEntry
mkDatasetEntry =
  DatasetEntry' {dataURI = Core.Nothing, entryName = Core.Nothing}

-- | The presigned URI of the data set item.
--
-- /Note:/ Consider using 'dataURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deDataURI :: Lens.Lens' DatasetEntry (Core.Maybe Types.PresignedURI)
deDataURI = Lens.field @"dataURI"
{-# DEPRECATED deDataURI "Use generic-lens or generic-optics with 'dataURI' instead." #-}

-- | The name of the data set item.
--
-- /Note:/ Consider using 'entryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEntryName :: Lens.Lens' DatasetEntry (Core.Maybe Types.EntryName)
deEntryName = Lens.field @"entryName"
{-# DEPRECATED deEntryName "Use generic-lens or generic-optics with 'entryName' instead." #-}

instance Core.FromJSON DatasetEntry where
  parseJSON =
    Core.withObject "DatasetEntry" Core.$
      \x ->
        DatasetEntry'
          Core.<$> (x Core..:? "dataURI") Core.<*> (x Core..:? "entryName")
