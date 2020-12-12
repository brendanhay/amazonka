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
    etTags,
    etDescription,
    etExportTaskId,
    etExportToS3Task,
    etInstanceExportDetails,
    etState,
    etStatusMessage,
  )
where

import Network.AWS.EC2.Types.ExportTaskState
import Network.AWS.EC2.Types.ExportToS3Task
import Network.AWS.EC2.Types.InstanceExportDetails
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance export task.
--
-- /See:/ 'mkExportTask' smart constructor.
data ExportTask = ExportTask'
  { tags :: Lude.Maybe [Tag],
    description :: Lude.Text,
    exportTaskId :: Lude.Text,
    exportToS3Task :: ExportToS3Task,
    instanceExportDetails :: InstanceExportDetails,
    state :: ExportTaskState,
    statusMessage :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExportTask' with the minimum fields required to make a request.
--
-- * 'description' - A description of the resource being exported.
-- * 'exportTaskId' - The ID of the export task.
-- * 'exportToS3Task' - Information about the export task.
-- * 'instanceExportDetails' - Information about the instance to export.
-- * 'state' - The state of the export task.
-- * 'statusMessage' - The status message related to the export task.
-- * 'tags' - The tags for the export task.
mkExportTask ::
  -- | 'description'
  Lude.Text ->
  -- | 'exportTaskId'
  Lude.Text ->
  -- | 'exportToS3Task'
  ExportToS3Task ->
  -- | 'instanceExportDetails'
  InstanceExportDetails ->
  -- | 'state'
  ExportTaskState ->
  -- | 'statusMessage'
  Lude.Text ->
  ExportTask
mkExportTask
  pDescription_
  pExportTaskId_
  pExportToS3Task_
  pInstanceExportDetails_
  pState_
  pStatusMessage_ =
    ExportTask'
      { tags = Lude.Nothing,
        description = pDescription_,
        exportTaskId = pExportTaskId_,
        exportToS3Task = pExportToS3Task_,
        instanceExportDetails = pInstanceExportDetails_,
        state = pState_,
        statusMessage = pStatusMessage_
      }

-- | The tags for the export task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etTags :: Lens.Lens' ExportTask (Lude.Maybe [Tag])
etTags = Lens.lens (tags :: ExportTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ExportTask)
{-# DEPRECATED etTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A description of the resource being exported.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etDescription :: Lens.Lens' ExportTask Lude.Text
etDescription = Lens.lens (description :: ExportTask -> Lude.Text) (\s a -> s {description = a} :: ExportTask)
{-# DEPRECATED etDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'exportTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportTaskId :: Lens.Lens' ExportTask Lude.Text
etExportTaskId = Lens.lens (exportTaskId :: ExportTask -> Lude.Text) (\s a -> s {exportTaskId = a} :: ExportTask)
{-# DEPRECATED etExportTaskId "Use generic-lens or generic-optics with 'exportTaskId' instead." #-}

-- | Information about the export task.
--
-- /Note:/ Consider using 'exportToS3Task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etExportToS3Task :: Lens.Lens' ExportTask ExportToS3Task
etExportToS3Task = Lens.lens (exportToS3Task :: ExportTask -> ExportToS3Task) (\s a -> s {exportToS3Task = a} :: ExportTask)
{-# DEPRECATED etExportToS3Task "Use generic-lens or generic-optics with 'exportToS3Task' instead." #-}

-- | Information about the instance to export.
--
-- /Note:/ Consider using 'instanceExportDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etInstanceExportDetails :: Lens.Lens' ExportTask InstanceExportDetails
etInstanceExportDetails = Lens.lens (instanceExportDetails :: ExportTask -> InstanceExportDetails) (\s a -> s {instanceExportDetails = a} :: ExportTask)
{-# DEPRECATED etInstanceExportDetails "Use generic-lens or generic-optics with 'instanceExportDetails' instead." #-}

-- | The state of the export task.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etState :: Lens.Lens' ExportTask ExportTaskState
etState = Lens.lens (state :: ExportTask -> ExportTaskState) (\s a -> s {state = a} :: ExportTask)
{-# DEPRECATED etState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The status message related to the export task.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etStatusMessage :: Lens.Lens' ExportTask Lude.Text
etStatusMessage = Lens.lens (statusMessage :: ExportTask -> Lude.Text) (\s a -> s {statusMessage = a} :: ExportTask)
{-# DEPRECATED etStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Lude.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Lude.<$> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "description")
      Lude.<*> (x Lude..@ "exportTaskId")
      Lude.<*> (x Lude..@ "exportToS3")
      Lude.<*> (x Lude..@ "instanceExport")
      Lude.<*> (x Lude..@ "state")
      Lude.<*> (x Lude..@ "statusMessage")
