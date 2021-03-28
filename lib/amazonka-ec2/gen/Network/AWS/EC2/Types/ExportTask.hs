{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ExportTask
  ( ExportTask (..)
  -- * Smart constructor
  , mkExportTask
  -- * Lenses
  , etDescription
  , etExportTaskId
  , etExportToS3Task
  , etInstanceExportDetails
  , etState
  , etStatusMessage
  , etTags
  ) where

import qualified Network.AWS.EC2.Types.ExportTaskState as Types
import qualified Network.AWS.EC2.Types.ExportToS3Task as Types
import qualified Network.AWS.EC2.Types.InstanceExportDetails as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance export task.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { description :: Core.Text
    -- ^ A description of the resource being exported.
  , exportTaskId :: Core.Text
    -- ^ The ID of the export task.
  , exportToS3Task :: Types.ExportToS3Task
    -- ^ Information about the export task.
  , instanceExportDetails :: Types.InstanceExportDetails
    -- ^ Information about the instance to export.
  , state :: Types.ExportTaskState
    -- ^ The state of the export task.
  , statusMessage :: Core.Text
    -- ^ The status message related to the export task.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags for the export task.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTask' value with any optional fields omitted.
mkExportTask
    :: Core.Text -- ^ 'description'
    -> Core.Text -- ^ 'exportTaskId'
    -> Types.ExportToS3Task -- ^ 'exportToS3Task'
    -> Types.InstanceExportDetails -- ^ 'instanceExportDetails'
    -> Types.ExportTaskState -- ^ 'state'
    -> Core.Text -- ^ 'statusMessage'
    -> ExportTask
mkExportTask description exportTaskId exportToS3Task
  instanceExportDetails state statusMessage
  = ExportTask'{description, exportTaskId, exportToS3Task,
                instanceExportDetails, state, statusMessage, tags = Core.Nothing}

-- | A description of the resource being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDescription :: Lens.Lens' ExportTask Core.Text
etDescription = Lens.field @"description"
{-# INLINEABLE etDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'exportTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportTaskId :: Lens.Lens' ExportTask Core.Text
etExportTaskId = Lens.field @"exportTaskId"
{-# INLINEABLE etExportTaskId #-}
{-# DEPRECATED exportTaskId "Use generic-lens or generic-optics with 'exportTaskId' instead"  #-}

-- | Information about the export task.
--
-- /Note:/ Consider using 'exportToS3Task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportToS3Task :: Lens.Lens' ExportTask Types.ExportToS3Task
etExportToS3Task = Lens.field @"exportToS3Task"
{-# INLINEABLE etExportToS3Task #-}
{-# DEPRECATED exportToS3Task "Use generic-lens or generic-optics with 'exportToS3Task' instead"  #-}

-- | Information about the instance to export.
--
-- /Note:/ Consider using 'instanceExportDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etInstanceExportDetails :: Lens.Lens' ExportTask Types.InstanceExportDetails
etInstanceExportDetails = Lens.field @"instanceExportDetails"
{-# INLINEABLE etInstanceExportDetails #-}
{-# DEPRECATED instanceExportDetails "Use generic-lens or generic-optics with 'instanceExportDetails' instead"  #-}

-- | The state of the export task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etState :: Lens.Lens' ExportTask Types.ExportTaskState
etState = Lens.field @"state"
{-# INLINEABLE etState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The status message related to the export task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatusMessage :: Lens.Lens' ExportTask Core.Text
etStatusMessage = Lens.field @"statusMessage"
{-# INLINEABLE etStatusMessage #-}
{-# DEPRECATED statusMessage "Use generic-lens or generic-optics with 'statusMessage' instead"  #-}

-- | The tags for the export task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTags :: Lens.Lens' ExportTask (Core.Maybe [Types.Tag])
etTags = Lens.field @"tags"
{-# INLINEABLE etTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML ExportTask where
        parseXML x
          = ExportTask' Core.<$>
              (x Core..@ "description") Core.<*> x Core..@ "exportTaskId"
                Core.<*> x Core..@ "exportToS3"
                Core.<*> x Core..@ "instanceExport"
                Core.<*> x Core..@ "state"
                Core.<*> x Core..@ "statusMessage"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
