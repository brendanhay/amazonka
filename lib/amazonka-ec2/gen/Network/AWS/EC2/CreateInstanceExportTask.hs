{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateInstanceExportTask (..)
    , mkCreateInstanceExportTask
    -- ** Request lenses
    , cietExportToS3Task
    , cietInstanceId
    , cietTargetEnvironment
    , cietDescription
    , cietTagSpecifications

    -- * Destructuring the response
    , CreateInstanceExportTaskResponse (..)
    , mkCreateInstanceExportTaskResponse
    -- ** Response lenses
    , cietrrsExportTask
    , cietrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateInstanceExportTask' smart constructor.
data CreateInstanceExportTask = CreateInstanceExportTask'
  { exportToS3Task :: Types.ExportToS3TaskSpecification
    -- ^ The format and location for an instance export task.
  , instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , targetEnvironment :: Types.ExportEnvironment
    -- ^ The target virtualization environment.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the conversion task or the resource being exported. The maximum length is 255 characters.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the instance export task during creation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceExportTask' value with any optional fields omitted.
mkCreateInstanceExportTask
    :: Types.ExportToS3TaskSpecification -- ^ 'exportToS3Task'
    -> Types.InstanceId -- ^ 'instanceId'
    -> Types.ExportEnvironment -- ^ 'targetEnvironment'
    -> CreateInstanceExportTask
mkCreateInstanceExportTask exportToS3Task instanceId
  targetEnvironment
  = CreateInstanceExportTask'{exportToS3Task, instanceId,
                              targetEnvironment, description = Core.Nothing,
                              tagSpecifications = Core.Nothing}

-- | The format and location for an instance export task.
--
-- /Note:/ Consider using 'exportToS3Task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietExportToS3Task :: Lens.Lens' CreateInstanceExportTask Types.ExportToS3TaskSpecification
cietExportToS3Task = Lens.field @"exportToS3Task"
{-# INLINEABLE cietExportToS3Task #-}
{-# DEPRECATED exportToS3Task "Use generic-lens or generic-optics with 'exportToS3Task' instead"  #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietInstanceId :: Lens.Lens' CreateInstanceExportTask Types.InstanceId
cietInstanceId = Lens.field @"instanceId"
{-# INLINEABLE cietInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The target virtualization environment.
--
-- /Note:/ Consider using 'targetEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietTargetEnvironment :: Lens.Lens' CreateInstanceExportTask Types.ExportEnvironment
cietTargetEnvironment = Lens.field @"targetEnvironment"
{-# INLINEABLE cietTargetEnvironment #-}
{-# DEPRECATED targetEnvironment "Use generic-lens or generic-optics with 'targetEnvironment' instead"  #-}

-- | A description for the conversion task or the resource being exported. The maximum length is 255 characters.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietDescription :: Lens.Lens' CreateInstanceExportTask (Core.Maybe Core.Text)
cietDescription = Lens.field @"description"
{-# INLINEABLE cietDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The tags to apply to the instance export task during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietTagSpecifications :: Lens.Lens' CreateInstanceExportTask (Core.Maybe [Types.TagSpecification])
cietTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cietTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateInstanceExportTask where
        toQuery CreateInstanceExportTask{..}
          = Core.toQueryPair "Action"
              ("CreateInstanceExportTask" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "ExportToS3" exportToS3Task
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.toQueryPair "TargetEnvironment" targetEnvironment
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateInstanceExportTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateInstanceExportTask where
        type Rs CreateInstanceExportTask = CreateInstanceExportTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateInstanceExportTaskResponse' Core.<$>
                   (x Core..@? "exportTask") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateInstanceExportTaskResponse' smart constructor.
data CreateInstanceExportTaskResponse = CreateInstanceExportTaskResponse'
  { exportTask :: Core.Maybe Types.ExportTask
    -- ^ Information about the instance export task.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstanceExportTaskResponse' value with any optional fields omitted.
mkCreateInstanceExportTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateInstanceExportTaskResponse
mkCreateInstanceExportTaskResponse responseStatus
  = CreateInstanceExportTaskResponse'{exportTask = Core.Nothing,
                                      responseStatus}

-- | Information about the instance export task.
--
-- /Note:/ Consider using 'exportTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietrrsExportTask :: Lens.Lens' CreateInstanceExportTaskResponse (Core.Maybe Types.ExportTask)
cietrrsExportTask = Lens.field @"exportTask"
{-# INLINEABLE cietrrsExportTask #-}
{-# DEPRECATED exportTask "Use generic-lens or generic-optics with 'exportTask' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cietrrsResponseStatus :: Lens.Lens' CreateInstanceExportTaskResponse Core.Int
cietrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cietrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
