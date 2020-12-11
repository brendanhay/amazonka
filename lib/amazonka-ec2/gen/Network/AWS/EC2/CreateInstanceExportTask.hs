{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateInstanceExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a running or stopped instance to an Amazon S3 bucket.
--
-- For information about the supported operating systems, image formats, and known limitations for the types of instances you can export, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport.html Exporting an Instance as a VM Using VM Import/Export> in the /VM Import\/Export User Guide/ .
module Network.AWS.EC2.CreateInstanceExportTask
  ( -- * Creating a request
    CreateInstanceExportTask (..),
    mkCreateInstanceExportTask,

    -- ** Request lenses
    cietTagSpecifications,
    cietDescription,
    cietExportToS3Task,
    cietInstanceId,
    cietTargetEnvironment,

    -- * Destructuring the response
    CreateInstanceExportTaskResponse (..),
    mkCreateInstanceExportTaskResponse,

    -- ** Response lenses
    cietrsExportTask,
    cietrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { tagSpecifications ::
      Lude.Maybe [TagSpecification],
    description :: Lude.Maybe Lude.Text,
    exportToS3Task ::
      ExportToS3TaskSpecification,
    instanceId :: Lude.Text,
    targetEnvironment :: ExportEnvironment
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceExportTask' with the minimum fields required to make a request.
--
-- * 'description' - A description for the conversion task or the resource being exported. The maximum length is 255 characters.
-- * 'exportToS3Task' - The format and location for an instance export task.
-- * 'instanceId' - The ID of the instance.
-- * 'tagSpecifications' - The tags to apply to the instance export task during creation.
-- * 'targetEnvironment' - The target virtualization environment.
mkCreateInstanceExportTask ::
  -- | 'exportToS3Task'
  ExportToS3TaskSpecification ->
  -- | 'instanceId'
  Lude.Text ->
  -- | 'targetEnvironment'
  ExportEnvironment ->
  CreateInstanceExportTask
mkCreateInstanceExportTask
  pExportToS3Task_
  pInstanceId_
  pTargetEnvironment_ =
    CreateInstanceExportTask'
      { tagSpecifications = Lude.Nothing,
        description = Lude.Nothing,
        exportToS3Task = pExportToS3Task_,
        instanceId = pInstanceId_,
        targetEnvironment = pTargetEnvironment_
      }

-- | The tags to apply to the instance export task during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietTagSpecifications :: Lens.Lens' CreateInstanceExportTask (Lude.Maybe [TagSpecification])
cietTagSpecifications = Lens.lens (tagSpecifications :: CreateInstanceExportTask -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateInstanceExportTask)
{-# DEPRECATED cietTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | A description for the conversion task or the resource being exported. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietDescription :: Lens.Lens' CreateInstanceExportTask (Lude.Maybe Lude.Text)
cietDescription = Lens.lens (description :: CreateInstanceExportTask -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateInstanceExportTask)
{-# DEPRECATED cietDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The format and location for an instance export task.
--
-- /Note:/ Consider using 'exportToS3Task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietExportToS3Task :: Lens.Lens' CreateInstanceExportTask ExportToS3TaskSpecification
cietExportToS3Task = Lens.lens (exportToS3Task :: CreateInstanceExportTask -> ExportToS3TaskSpecification) (\s a -> s {exportToS3Task = a} :: CreateInstanceExportTask)
{-# DEPRECATED cietExportToS3Task "Use generic-lens or generic-optics with 'exportToS3Task' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietInstanceId :: Lens.Lens' CreateInstanceExportTask Lude.Text
cietInstanceId = Lens.lens (instanceId :: CreateInstanceExportTask -> Lude.Text) (\s a -> s {instanceId = a} :: CreateInstanceExportTask)
{-# DEPRECATED cietInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The target virtualization environment.
--
-- /Note:/ Consider using 'targetEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietTargetEnvironment :: Lens.Lens' CreateInstanceExportTask ExportEnvironment
cietTargetEnvironment = Lens.lens (targetEnvironment :: CreateInstanceExportTask -> ExportEnvironment) (\s a -> s {targetEnvironment = a} :: CreateInstanceExportTask)
{-# DEPRECATED cietTargetEnvironment "Use generic-lens or generic-optics with 'targetEnvironment' instead." #-}

instance Lude.AWSRequest CreateInstanceExportTask where
  type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateInstanceExportTaskResponse'
            Lude.<$> (x Lude..@? "exportTask") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstanceExportTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateInstanceExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstanceExportTask where
  toQuery CreateInstanceExportTask' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateInstanceExportTask" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "Description" Lude.=: description,
        "ExportToS3" Lude.=: exportToS3Task,
        "InstanceId" Lude.=: instanceId,
        "TargetEnvironment" Lude.=: targetEnvironment
      ]

-- | /See:/ 'mkCreateInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { exportTask ::
      Lude.Maybe ExportTask,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstanceExportTaskResponse' with the minimum fields required to make a request.
--
-- * 'exportTask' - Information about the instance export task.
-- * 'responseStatus' - The response status code.
mkCreateInstanceExportTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstanceExportTaskResponse
mkCreateInstanceExportTaskResponse pResponseStatus_ =
  CreateInstanceExportTaskResponse'
    { exportTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the instance export task.
--
-- /Note:/ Consider using 'exportTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietrsExportTask :: Lens.Lens' CreateInstanceExportTaskResponse (Lude.Maybe ExportTask)
cietrsExportTask = Lens.lens (exportTask :: CreateInstanceExportTaskResponse -> Lude.Maybe ExportTask) (\s a -> s {exportTask = a} :: CreateInstanceExportTaskResponse)
{-# DEPRECATED cietrsExportTask "Use generic-lens or generic-optics with 'exportTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietrsResponseStatus :: Lens.Lens' CreateInstanceExportTaskResponse Lude.Int
cietrsResponseStatus = Lens.lens (responseStatus :: CreateInstanceExportTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstanceExportTaskResponse)
{-# DEPRECATED cietrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
