{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cietExportToS3Task,
    cietInstanceId,
    cietTargetEnvironment,
    cietDescription,
    cietTagSpecifications,

    -- * Destructuring the response
    CreateInstanceExportTaskResponse (..),
    mkCreateInstanceExportTaskResponse,

    -- ** Response lenses
    cietrrsExportTask,
    cietrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { -- | The format and location for an instance export task.
    exportToS3Task :: Types.ExportToS3TaskSpecification,
    -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | The target virtualization environment.
    targetEnvironment :: Types.ExportEnvironment,
    -- | A description for the conversion task or the resource being exported. The maximum length is 255 characters.
    description :: Core.Maybe Types.Description,
    -- | The tags to apply to the instance export task during creation.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceExportTask' value with any optional fields omitted.
mkCreateInstanceExportTask ::
  -- | 'exportToS3Task'
  Types.ExportToS3TaskSpecification ->
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'targetEnvironment'
  Types.ExportEnvironment ->
  CreateInstanceExportTask
mkCreateInstanceExportTask
  exportToS3Task
  instanceId
  targetEnvironment =
    CreateInstanceExportTask'
      { exportToS3Task,
        instanceId,
        targetEnvironment,
        description = Core.Nothing,
        tagSpecifications = Core.Nothing
      }

-- | The format and location for an instance export task.
--
-- /Note:/ Consider using 'exportToS3Task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietExportToS3Task :: Lens.Lens' CreateInstanceExportTask Types.ExportToS3TaskSpecification
cietExportToS3Task = Lens.field @"exportToS3Task"
{-# DEPRECATED cietExportToS3Task "Use generic-lens or generic-optics with 'exportToS3Task' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietInstanceId :: Lens.Lens' CreateInstanceExportTask Types.InstanceId
cietInstanceId = Lens.field @"instanceId"
{-# DEPRECATED cietInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The target virtualization environment.
--
-- /Note:/ Consider using 'targetEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietTargetEnvironment :: Lens.Lens' CreateInstanceExportTask Types.ExportEnvironment
cietTargetEnvironment = Lens.field @"targetEnvironment"
{-# DEPRECATED cietTargetEnvironment "Use generic-lens or generic-optics with 'targetEnvironment' instead." #-}

-- | A description for the conversion task or the resource being exported. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietDescription :: Lens.Lens' CreateInstanceExportTask (Core.Maybe Types.Description)
cietDescription = Lens.field @"description"
{-# DEPRECATED cietDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to apply to the instance export task during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietTagSpecifications :: Lens.Lens' CreateInstanceExportTask (Core.Maybe [Types.TagSpecification])
cietTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cietTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateInstanceExportTask where
  type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateInstanceExportTask")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "ExportToS3" exportToS3Task)
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "TargetEnvironment" targetEnvironment)
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInstanceExportTaskResponse'
            Core.<$> (x Core..@? "exportTask") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { -- | Information about the instance export task.
    exportTask :: Core.Maybe Types.ExportTask,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceExportTaskResponse' value with any optional fields omitted.
mkCreateInstanceExportTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateInstanceExportTaskResponse
mkCreateInstanceExportTaskResponse responseStatus =
  CreateInstanceExportTaskResponse'
    { exportTask = Core.Nothing,
      responseStatus
    }

-- | Information about the instance export task.
--
-- /Note:/ Consider using 'exportTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietrrsExportTask :: Lens.Lens' CreateInstanceExportTaskResponse (Core.Maybe Types.ExportTask)
cietrrsExportTask = Lens.field @"exportTask"
{-# DEPRECATED cietrrsExportTask "Use generic-lens or generic-optics with 'exportTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietrrsResponseStatus :: Lens.Lens' CreateInstanceExportTaskResponse Core.Int
cietrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cietrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
