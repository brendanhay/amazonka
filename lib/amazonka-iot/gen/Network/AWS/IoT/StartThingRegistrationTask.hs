{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StartThingRegistrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bulk thing provisioning task.
module Network.AWS.IoT.StartThingRegistrationTask
    (
    -- * Creating a request
      StartThingRegistrationTask (..)
    , mkStartThingRegistrationTask
    -- ** Request lenses
    , strtTemplateBody
    , strtInputFileBucket
    , strtInputFileKey
    , strtRoleArn

    -- * Destructuring the response
    , StartThingRegistrationTaskResponse (..)
    , mkStartThingRegistrationTaskResponse
    -- ** Response lenses
    , strtrrsTaskId
    , strtrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartThingRegistrationTask' smart constructor.
data StartThingRegistrationTask = StartThingRegistrationTask'
  { templateBody :: Types.TemplateBody
    -- ^ The provisioning template.
  , inputFileBucket :: Types.RegistryS3BucketName
    -- ^ The S3 bucket that contains the input file.
  , inputFileKey :: Types.RegistryS3KeyName
    -- ^ The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
  , roleArn :: Types.RoleArn
    -- ^ The IAM role ARN that grants permission the input file.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartThingRegistrationTask' value with any optional fields omitted.
mkStartThingRegistrationTask
    :: Types.TemplateBody -- ^ 'templateBody'
    -> Types.RegistryS3BucketName -- ^ 'inputFileBucket'
    -> Types.RegistryS3KeyName -- ^ 'inputFileKey'
    -> Types.RoleArn -- ^ 'roleArn'
    -> StartThingRegistrationTask
mkStartThingRegistrationTask templateBody inputFileBucket
  inputFileKey roleArn
  = StartThingRegistrationTask'{templateBody, inputFileBucket,
                                inputFileKey, roleArn}

-- | The provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtTemplateBody :: Lens.Lens' StartThingRegistrationTask Types.TemplateBody
strtTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE strtTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | The S3 bucket that contains the input file.
--
-- /Note:/ Consider using 'inputFileBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtInputFileBucket :: Lens.Lens' StartThingRegistrationTask Types.RegistryS3BucketName
strtInputFileBucket = Lens.field @"inputFileBucket"
{-# INLINEABLE strtInputFileBucket #-}
{-# DEPRECATED inputFileBucket "Use generic-lens or generic-optics with 'inputFileBucket' instead"  #-}

-- | The name of input file within the S3 bucket. This file contains a newline delimited JSON file. Each line contains the parameter values to provision one device (thing).
--
-- /Note:/ Consider using 'inputFileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtInputFileKey :: Lens.Lens' StartThingRegistrationTask Types.RegistryS3KeyName
strtInputFileKey = Lens.field @"inputFileKey"
{-# INLINEABLE strtInputFileKey #-}
{-# DEPRECATED inputFileKey "Use generic-lens or generic-optics with 'inputFileKey' instead"  #-}

-- | The IAM role ARN that grants permission the input file.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtRoleArn :: Lens.Lens' StartThingRegistrationTask Types.RoleArn
strtRoleArn = Lens.field @"roleArn"
{-# INLINEABLE strtRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery StartThingRegistrationTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartThingRegistrationTask where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON StartThingRegistrationTask where
        toJSON StartThingRegistrationTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("templateBody" Core..= templateBody),
                  Core.Just ("inputFileBucket" Core..= inputFileBucket),
                  Core.Just ("inputFileKey" Core..= inputFileKey),
                  Core.Just ("roleArn" Core..= roleArn)])

instance Core.AWSRequest StartThingRegistrationTask where
        type Rs StartThingRegistrationTask =
             StartThingRegistrationTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/thing-registration-tasks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartThingRegistrationTaskResponse' Core.<$>
                   (x Core..:? "taskId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartThingRegistrationTaskResponse' smart constructor.
data StartThingRegistrationTaskResponse = StartThingRegistrationTaskResponse'
  { taskId :: Core.Maybe Types.TaskId
    -- ^ The bulk thing provisioning task ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartThingRegistrationTaskResponse' value with any optional fields omitted.
mkStartThingRegistrationTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartThingRegistrationTaskResponse
mkStartThingRegistrationTaskResponse responseStatus
  = StartThingRegistrationTaskResponse'{taskId = Core.Nothing,
                                        responseStatus}

-- | The bulk thing provisioning task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtrrsTaskId :: Lens.Lens' StartThingRegistrationTaskResponse (Core.Maybe Types.TaskId)
strtrrsTaskId = Lens.field @"taskId"
{-# INLINEABLE strtrrsTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtrrsResponseStatus :: Lens.Lens' StartThingRegistrationTaskResponse Core.Int
strtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE strtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
