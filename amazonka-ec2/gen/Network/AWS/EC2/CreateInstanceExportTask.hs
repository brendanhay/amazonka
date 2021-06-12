{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an Amazon S3 bucket.
--
-- For information about the supported operating systems, image formats,
-- and known limitations for the types of instances you can export, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html Exporting an Instance as a VM Using VM Import\/Export>
-- in the /VM Import\/Export User Guide/.
module Network.AWS.EC2.CreateInstanceExportTask
  ( -- * Creating a Request
    CreateInstanceExportTask (..),
    newCreateInstanceExportTask,

    -- * Request Lenses
    createInstanceExportTask_tagSpecifications,
    createInstanceExportTask_description,
    createInstanceExportTask_exportToS3Task,
    createInstanceExportTask_instanceId,
    createInstanceExportTask_targetEnvironment,

    -- * Destructuring the Response
    CreateInstanceExportTaskResponse (..),
    newCreateInstanceExportTaskResponse,

    -- * Response Lenses
    createInstanceExportTaskResponse_exportTask,
    createInstanceExportTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { -- | The tags to apply to the export instance task during creation.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | A description for the conversion task or the resource being exported.
    -- The maximum length is 255 characters.
    description :: Core.Maybe Core.Text,
    -- | The format and location for an export instance task.
    exportToS3Task :: ExportToS3TaskSpecification,
    -- | The ID of the instance.
    instanceId :: Core.Text,
    -- | The target virtualization environment.
    targetEnvironment :: ExportEnvironment
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInstanceExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createInstanceExportTask_tagSpecifications' - The tags to apply to the export instance task during creation.
--
-- 'description', 'createInstanceExportTask_description' - A description for the conversion task or the resource being exported.
-- The maximum length is 255 characters.
--
-- 'exportToS3Task', 'createInstanceExportTask_exportToS3Task' - The format and location for an export instance task.
--
-- 'instanceId', 'createInstanceExportTask_instanceId' - The ID of the instance.
--
-- 'targetEnvironment', 'createInstanceExportTask_targetEnvironment' - The target virtualization environment.
newCreateInstanceExportTask ::
  -- | 'exportToS3Task'
  ExportToS3TaskSpecification ->
  -- | 'instanceId'
  Core.Text ->
  -- | 'targetEnvironment'
  ExportEnvironment ->
  CreateInstanceExportTask
newCreateInstanceExportTask
  pExportToS3Task_
  pInstanceId_
  pTargetEnvironment_ =
    CreateInstanceExportTask'
      { tagSpecifications =
          Core.Nothing,
        description = Core.Nothing,
        exportToS3Task = pExportToS3Task_,
        instanceId = pInstanceId_,
        targetEnvironment = pTargetEnvironment_
      }

-- | The tags to apply to the export instance task during creation.
createInstanceExportTask_tagSpecifications :: Lens.Lens' CreateInstanceExportTask (Core.Maybe [TagSpecification])
createInstanceExportTask_tagSpecifications = Lens.lens (\CreateInstanceExportTask' {tagSpecifications} -> tagSpecifications) (\s@CreateInstanceExportTask' {} a -> s {tagSpecifications = a} :: CreateInstanceExportTask) Core.. Lens.mapping Lens._Coerce

-- | A description for the conversion task or the resource being exported.
-- The maximum length is 255 characters.
createInstanceExportTask_description :: Lens.Lens' CreateInstanceExportTask (Core.Maybe Core.Text)
createInstanceExportTask_description = Lens.lens (\CreateInstanceExportTask' {description} -> description) (\s@CreateInstanceExportTask' {} a -> s {description = a} :: CreateInstanceExportTask)

-- | The format and location for an export instance task.
createInstanceExportTask_exportToS3Task :: Lens.Lens' CreateInstanceExportTask ExportToS3TaskSpecification
createInstanceExportTask_exportToS3Task = Lens.lens (\CreateInstanceExportTask' {exportToS3Task} -> exportToS3Task) (\s@CreateInstanceExportTask' {} a -> s {exportToS3Task = a} :: CreateInstanceExportTask)

-- | The ID of the instance.
createInstanceExportTask_instanceId :: Lens.Lens' CreateInstanceExportTask Core.Text
createInstanceExportTask_instanceId = Lens.lens (\CreateInstanceExportTask' {instanceId} -> instanceId) (\s@CreateInstanceExportTask' {} a -> s {instanceId = a} :: CreateInstanceExportTask)

-- | The target virtualization environment.
createInstanceExportTask_targetEnvironment :: Lens.Lens' CreateInstanceExportTask ExportEnvironment
createInstanceExportTask_targetEnvironment = Lens.lens (\CreateInstanceExportTask' {targetEnvironment} -> targetEnvironment) (\s@CreateInstanceExportTask' {} a -> s {targetEnvironment = a} :: CreateInstanceExportTask)

instance Core.AWSRequest CreateInstanceExportTask where
  type
    AWSResponse CreateInstanceExportTask =
      CreateInstanceExportTaskResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInstanceExportTaskResponse'
            Core.<$> (x Core..@? "exportTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateInstanceExportTask

instance Core.NFData CreateInstanceExportTask

instance Core.ToHeaders CreateInstanceExportTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateInstanceExportTask where
  toPath = Core.const "/"

instance Core.ToQuery CreateInstanceExportTask where
  toQuery CreateInstanceExportTask' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateInstanceExportTask" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "Description" Core.=: description,
        "ExportToS3" Core.=: exportToS3Task,
        "InstanceId" Core.=: instanceId,
        "TargetEnvironment" Core.=: targetEnvironment
      ]

-- | /See:/ 'newCreateInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { -- | Information about the export instance task.
    exportTask :: Core.Maybe ExportTask,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInstanceExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTask', 'createInstanceExportTaskResponse_exportTask' - Information about the export instance task.
--
-- 'httpStatus', 'createInstanceExportTaskResponse_httpStatus' - The response's http status code.
newCreateInstanceExportTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateInstanceExportTaskResponse
newCreateInstanceExportTaskResponse pHttpStatus_ =
  CreateInstanceExportTaskResponse'
    { exportTask =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the export instance task.
createInstanceExportTaskResponse_exportTask :: Lens.Lens' CreateInstanceExportTaskResponse (Core.Maybe ExportTask)
createInstanceExportTaskResponse_exportTask = Lens.lens (\CreateInstanceExportTaskResponse' {exportTask} -> exportTask) (\s@CreateInstanceExportTaskResponse' {} a -> s {exportTask = a} :: CreateInstanceExportTaskResponse)

-- | The response's http status code.
createInstanceExportTaskResponse_httpStatus :: Lens.Lens' CreateInstanceExportTaskResponse Core.Int
createInstanceExportTaskResponse_httpStatus = Lens.lens (\CreateInstanceExportTaskResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceExportTaskResponse' {} a -> s {httpStatus = a} :: CreateInstanceExportTaskResponse)

instance Core.NFData CreateInstanceExportTaskResponse
