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
-- Module      : Network.AWS.EC2.Types.ExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportTask where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ExportTaskState
import Network.AWS.EC2.Types.ExportToS3Task
import Network.AWS.EC2.Types.InstanceExportDetails
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an export instance task.
--
-- /See:/ 'newExportTask' smart constructor.
data ExportTask = ExportTask'
  { -- | The tags for the export task.
    tags :: Prelude.Maybe [Tag],
    -- | A description of the resource being exported.
    description :: Prelude.Text,
    -- | The ID of the export task.
    exportTaskId :: Prelude.Text,
    -- | Information about the export task.
    exportToS3Task :: ExportToS3Task,
    -- | Information about the instance to export.
    instanceExportDetails :: InstanceExportDetails,
    -- | The state of the export task.
    state :: ExportTaskState,
    -- | The status message related to the export task.
    statusMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'exportTask_tags' - The tags for the export task.
--
-- 'description', 'exportTask_description' - A description of the resource being exported.
--
-- 'exportTaskId', 'exportTask_exportTaskId' - The ID of the export task.
--
-- 'exportToS3Task', 'exportTask_exportToS3Task' - Information about the export task.
--
-- 'instanceExportDetails', 'exportTask_instanceExportDetails' - Information about the instance to export.
--
-- 'state', 'exportTask_state' - The state of the export task.
--
-- 'statusMessage', 'exportTask_statusMessage' - The status message related to the export task.
newExportTask ::
  -- | 'description'
  Prelude.Text ->
  -- | 'exportTaskId'
  Prelude.Text ->
  -- | 'exportToS3Task'
  ExportToS3Task ->
  -- | 'instanceExportDetails'
  InstanceExportDetails ->
  -- | 'state'
  ExportTaskState ->
  -- | 'statusMessage'
  Prelude.Text ->
  ExportTask
newExportTask
  pDescription_
  pExportTaskId_
  pExportToS3Task_
  pInstanceExportDetails_
  pState_
  pStatusMessage_ =
    ExportTask'
      { tags = Prelude.Nothing,
        description = pDescription_,
        exportTaskId = pExportTaskId_,
        exportToS3Task = pExportToS3Task_,
        instanceExportDetails = pInstanceExportDetails_,
        state = pState_,
        statusMessage = pStatusMessage_
      }

-- | The tags for the export task.
exportTask_tags :: Lens.Lens' ExportTask (Prelude.Maybe [Tag])
exportTask_tags = Lens.lens (\ExportTask' {tags} -> tags) (\s@ExportTask' {} a -> s {tags = a} :: ExportTask) Prelude.. Lens.mapping Prelude._Coerce

-- | A description of the resource being exported.
exportTask_description :: Lens.Lens' ExportTask Prelude.Text
exportTask_description = Lens.lens (\ExportTask' {description} -> description) (\s@ExportTask' {} a -> s {description = a} :: ExportTask)

-- | The ID of the export task.
exportTask_exportTaskId :: Lens.Lens' ExportTask Prelude.Text
exportTask_exportTaskId = Lens.lens (\ExportTask' {exportTaskId} -> exportTaskId) (\s@ExportTask' {} a -> s {exportTaskId = a} :: ExportTask)

-- | Information about the export task.
exportTask_exportToS3Task :: Lens.Lens' ExportTask ExportToS3Task
exportTask_exportToS3Task = Lens.lens (\ExportTask' {exportToS3Task} -> exportToS3Task) (\s@ExportTask' {} a -> s {exportToS3Task = a} :: ExportTask)

-- | Information about the instance to export.
exportTask_instanceExportDetails :: Lens.Lens' ExportTask InstanceExportDetails
exportTask_instanceExportDetails = Lens.lens (\ExportTask' {instanceExportDetails} -> instanceExportDetails) (\s@ExportTask' {} a -> s {instanceExportDetails = a} :: ExportTask)

-- | The state of the export task.
exportTask_state :: Lens.Lens' ExportTask ExportTaskState
exportTask_state = Lens.lens (\ExportTask' {state} -> state) (\s@ExportTask' {} a -> s {state = a} :: ExportTask)

-- | The status message related to the export task.
exportTask_statusMessage :: Lens.Lens' ExportTask Prelude.Text
exportTask_statusMessage = Lens.lens (\ExportTask' {statusMessage} -> statusMessage) (\s@ExportTask' {} a -> s {statusMessage = a} :: ExportTask)

instance Prelude.FromXML ExportTask where
  parseXML x =
    ExportTask'
      Prelude.<$> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@ "description")
      Prelude.<*> (x Prelude..@ "exportTaskId")
      Prelude.<*> (x Prelude..@ "exportToS3")
      Prelude.<*> (x Prelude..@ "instanceExport")
      Prelude.<*> (x Prelude..@ "state")
      Prelude.<*> (x Prelude..@ "statusMessage")

instance Prelude.Hashable ExportTask

instance Prelude.NFData ExportTask
