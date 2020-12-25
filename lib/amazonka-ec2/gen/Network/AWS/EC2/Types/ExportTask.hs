{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTask
  ( ExportTask (..),

    -- * Smart constructor
    mkExportTask,

    -- * Lenses
    etDescription,
    etExportTaskId,
    etExportToS3Task,
    etInstanceExportDetails,
    etState,
    etStatusMessage,
    etTags,
  )
where

import qualified Network.AWS.EC2.Types.ExportTaskState as Types
import qualified Network.AWS.EC2.Types.ExportToS3Task as Types
import qualified Network.AWS.EC2.Types.InstanceExportDetails as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance export task.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | A description of the resource being exported.
    description :: Types.String,
    -- | The ID of the export task.
    exportTaskId :: Types.String,
    -- | Information about the export task.
    exportToS3Task :: Types.ExportToS3Task,
    -- | Information about the instance to export.
    instanceExportDetails :: Types.InstanceExportDetails,
    -- | The state of the export task.
    state :: Types.ExportTaskState,
    -- | The status message related to the export task.
    statusMessage :: Types.String,
    -- | The tags for the export task.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportTask' value with any optional fields omitted.
mkExportTask ::
  -- | 'description'
  Types.String ->
  -- | 'exportTaskId'
  Types.String ->
  -- | 'exportToS3Task'
  Types.ExportToS3Task ->
  -- | 'instanceExportDetails'
  Types.InstanceExportDetails ->
  -- | 'state'
  Types.ExportTaskState ->
  -- | 'statusMessage'
  Types.String ->
  ExportTask
mkExportTask
  description
  exportTaskId
  exportToS3Task
  instanceExportDetails
  state
  statusMessage =
    ExportTask'
      { description,
        exportTaskId,
        exportToS3Task,
        instanceExportDetails,
        state,
        statusMessage,
        tags = Core.Nothing
      }

-- | A description of the resource being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDescription :: Lens.Lens' ExportTask Types.String
etDescription = Lens.field @"description"
{-# DEPRECATED etDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'exportTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportTaskId :: Lens.Lens' ExportTask Types.String
etExportTaskId = Lens.field @"exportTaskId"
{-# DEPRECATED etExportTaskId "Use generic-lens or generic-optics with 'exportTaskId' instead." #-}

-- | Information about the export task.
--
-- /Note:/ Consider using 'exportToS3Task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportToS3Task :: Lens.Lens' ExportTask Types.ExportToS3Task
etExportToS3Task = Lens.field @"exportToS3Task"
{-# DEPRECATED etExportToS3Task "Use generic-lens or generic-optics with 'exportToS3Task' instead." #-}

-- | Information about the instance to export.
--
-- /Note:/ Consider using 'instanceExportDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etInstanceExportDetails :: Lens.Lens' ExportTask Types.InstanceExportDetails
etInstanceExportDetails = Lens.field @"instanceExportDetails"
{-# DEPRECATED etInstanceExportDetails "Use generic-lens or generic-optics with 'instanceExportDetails' instead." #-}

-- | The state of the export task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etState :: Lens.Lens' ExportTask Types.ExportTaskState
etState = Lens.field @"state"
{-# DEPRECATED etState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status message related to the export task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatusMessage :: Lens.Lens' ExportTask Types.String
etStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED etStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The tags for the export task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTags :: Lens.Lens' ExportTask (Core.Maybe [Types.Tag])
etTags = Lens.field @"tags"
{-# DEPRECATED etTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Core.<$> (x Core..@ "description")
      Core.<*> (x Core..@ "exportTaskId")
      Core.<*> (x Core..@ "exportToS3")
      Core.<*> (x Core..@ "instanceExport")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@ "statusMessage")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
