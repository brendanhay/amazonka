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
-- Module      : Amazonka.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an Amazon S3 bucket.
--
-- For information about the supported operating systems, image formats,
-- and known limitations for the types of instances you can export, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html Exporting an instance as a VM Using VM Import\/Export>
-- in the /VM Import\/Export User Guide/.
module Amazonka.EC2.CreateInstanceExportTask
  ( -- * Creating a Request
    CreateInstanceExportTask (..),
    newCreateInstanceExportTask,

    -- * Request Lenses
    createInstanceExportTask_description,
    createInstanceExportTask_tagSpecifications,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { -- | A description for the conversion task or the resource being exported.
    -- The maximum length is 255 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the export instance task during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The format and location for an export instance task.
    exportToS3Task :: ExportToS3TaskSpecification,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The target virtualization environment.
    targetEnvironment :: ExportEnvironment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createInstanceExportTask_description' - A description for the conversion task or the resource being exported.
-- The maximum length is 255 characters.
--
-- 'tagSpecifications', 'createInstanceExportTask_tagSpecifications' - The tags to apply to the export instance task during creation.
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
  Prelude.Text ->
  -- | 'targetEnvironment'
  ExportEnvironment ->
  CreateInstanceExportTask
newCreateInstanceExportTask
  pExportToS3Task_
  pInstanceId_
  pTargetEnvironment_ =
    CreateInstanceExportTask'
      { description =
          Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        exportToS3Task = pExportToS3Task_,
        instanceId = pInstanceId_,
        targetEnvironment = pTargetEnvironment_
      }

-- | A description for the conversion task or the resource being exported.
-- The maximum length is 255 characters.
createInstanceExportTask_description :: Lens.Lens' CreateInstanceExportTask (Prelude.Maybe Prelude.Text)
createInstanceExportTask_description = Lens.lens (\CreateInstanceExportTask' {description} -> description) (\s@CreateInstanceExportTask' {} a -> s {description = a} :: CreateInstanceExportTask)

-- | The tags to apply to the export instance task during creation.
createInstanceExportTask_tagSpecifications :: Lens.Lens' CreateInstanceExportTask (Prelude.Maybe [TagSpecification])
createInstanceExportTask_tagSpecifications = Lens.lens (\CreateInstanceExportTask' {tagSpecifications} -> tagSpecifications) (\s@CreateInstanceExportTask' {} a -> s {tagSpecifications = a} :: CreateInstanceExportTask) Prelude.. Lens.mapping Lens.coerced

-- | The format and location for an export instance task.
createInstanceExportTask_exportToS3Task :: Lens.Lens' CreateInstanceExportTask ExportToS3TaskSpecification
createInstanceExportTask_exportToS3Task = Lens.lens (\CreateInstanceExportTask' {exportToS3Task} -> exportToS3Task) (\s@CreateInstanceExportTask' {} a -> s {exportToS3Task = a} :: CreateInstanceExportTask)

-- | The ID of the instance.
createInstanceExportTask_instanceId :: Lens.Lens' CreateInstanceExportTask Prelude.Text
createInstanceExportTask_instanceId = Lens.lens (\CreateInstanceExportTask' {instanceId} -> instanceId) (\s@CreateInstanceExportTask' {} a -> s {instanceId = a} :: CreateInstanceExportTask)

-- | The target virtualization environment.
createInstanceExportTask_targetEnvironment :: Lens.Lens' CreateInstanceExportTask ExportEnvironment
createInstanceExportTask_targetEnvironment = Lens.lens (\CreateInstanceExportTask' {targetEnvironment} -> targetEnvironment) (\s@CreateInstanceExportTask' {} a -> s {targetEnvironment = a} :: CreateInstanceExportTask)

instance Core.AWSRequest CreateInstanceExportTask where
  type
    AWSResponse CreateInstanceExportTask =
      CreateInstanceExportTaskResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInstanceExportTaskResponse'
            Prelude.<$> (x Data..@? "exportTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstanceExportTask where
  hashWithSalt _salt CreateInstanceExportTask' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` exportToS3Task
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` targetEnvironment

instance Prelude.NFData CreateInstanceExportTask where
  rnf CreateInstanceExportTask' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf exportToS3Task
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf targetEnvironment

instance Data.ToHeaders CreateInstanceExportTask where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateInstanceExportTask where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInstanceExportTask where
  toQuery CreateInstanceExportTask' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateInstanceExportTask" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "ExportToS3" Data.=: exportToS3Task,
        "InstanceId" Data.=: instanceId,
        "TargetEnvironment" Data.=: targetEnvironment
      ]

-- | /See:/ 'newCreateInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { -- | Information about the export instance task.
    exportTask :: Prelude.Maybe ExportTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateInstanceExportTaskResponse
newCreateInstanceExportTaskResponse pHttpStatus_ =
  CreateInstanceExportTaskResponse'
    { exportTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the export instance task.
createInstanceExportTaskResponse_exportTask :: Lens.Lens' CreateInstanceExportTaskResponse (Prelude.Maybe ExportTask)
createInstanceExportTaskResponse_exportTask = Lens.lens (\CreateInstanceExportTaskResponse' {exportTask} -> exportTask) (\s@CreateInstanceExportTaskResponse' {} a -> s {exportTask = a} :: CreateInstanceExportTaskResponse)

-- | The response's http status code.
createInstanceExportTaskResponse_httpStatus :: Lens.Lens' CreateInstanceExportTaskResponse Prelude.Int
createInstanceExportTaskResponse_httpStatus = Lens.lens (\CreateInstanceExportTaskResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceExportTaskResponse' {} a -> s {httpStatus = a} :: CreateInstanceExportTaskResponse)

instance
  Prelude.NFData
    CreateInstanceExportTaskResponse
  where
  rnf CreateInstanceExportTaskResponse' {..} =
    Prelude.rnf exportTask
      `Prelude.seq` Prelude.rnf httpStatus
