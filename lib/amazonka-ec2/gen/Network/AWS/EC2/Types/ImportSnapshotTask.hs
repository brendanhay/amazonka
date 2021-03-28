{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportSnapshotTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImportSnapshotTask
  ( ImportSnapshotTask (..)
  -- * Smart constructor
  , mkImportSnapshotTask
  -- * Lenses
  , istDescription
  , istImportTaskId
  , istSnapshotTaskDetail
  , istTags
  ) where

import qualified Network.AWS.EC2.Types.SnapshotTaskDetail as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an import snapshot task.
--
-- /See:/ 'mkImportSnapshotTask' smart constructor.
data ImportSnapshotTask = ImportSnapshotTask'
  { description :: Core.Maybe Core.Text
    -- ^ A description of the import snapshot task.
  , importTaskId :: Core.Maybe Core.Text
    -- ^ The ID of the import snapshot task.
  , snapshotTaskDetail :: Core.Maybe Types.SnapshotTaskDetail
    -- ^ Describes an import snapshot task.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the import snapshot task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportSnapshotTask' value with any optional fields omitted.
mkImportSnapshotTask
    :: ImportSnapshotTask
mkImportSnapshotTask
  = ImportSnapshotTask'{description = Core.Nothing,
                        importTaskId = Core.Nothing, snapshotTaskDetail = Core.Nothing,
                        tags = Core.Nothing}

-- | A description of the import snapshot task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istDescription :: Lens.Lens' ImportSnapshotTask (Core.Maybe Core.Text)
istDescription = Lens.field @"description"
{-# INLINEABLE istDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the import snapshot task.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istImportTaskId :: Lens.Lens' ImportSnapshotTask (Core.Maybe Core.Text)
istImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE istImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

-- | Describes an import snapshot task.
--
-- /Note:/ Consider using 'snapshotTaskDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istSnapshotTaskDetail :: Lens.Lens' ImportSnapshotTask (Core.Maybe Types.SnapshotTaskDetail)
istSnapshotTaskDetail = Lens.field @"snapshotTaskDetail"
{-# INLINEABLE istSnapshotTaskDetail #-}
{-# DEPRECATED snapshotTaskDetail "Use generic-lens or generic-optics with 'snapshotTaskDetail' instead"  #-}

-- | The tags for the import snapshot task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
istTags :: Lens.Lens' ImportSnapshotTask (Core.Maybe [Types.Tag])
istTags = Lens.field @"tags"
{-# INLINEABLE istTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML ImportSnapshotTask where
        parseXML x
          = ImportSnapshotTask' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "importTaskId"
                Core.<*> x Core..@? "snapshotTaskDetail"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
