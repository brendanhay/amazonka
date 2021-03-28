{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StartImportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an import task, which allows you to import details of your on-premises environment directly into AWS Migration Hub without having to use the Application Discovery Service (ADS) tools such as the Discovery Connector or Discovery Agent. This gives you the option to perform migration assessment and planning directly from your imported data, including the ability to group your devices as applications and track their migration status.
--
-- To start an import request, do this:
--
--     * Download the specially formatted comma separated value (CSV) import template, which you can find here: <https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv https://s3-us-west-2.amazonaws.com/templates-7cffcf56-bd96-4b1c-b45b-a5b42f282e46/import_template.csv> .
--
--
--     * Fill out the template with your server and application data.
--
--
--     * Upload your import file to an Amazon S3 bucket, and make a note of it's Object URL. Your import file must be in the CSV format.
--
--
--     * Use the console or the @StartImportTask@ command with the AWS CLI or one of the AWS SDKs to import the records from your file.
--
--
-- For more information, including step-by-step procedures, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-import.html Migration Hub Import> in the /AWS Application Discovery Service User Guide/ .
module Network.AWS.Discovery.StartImportTask
    (
    -- * Creating a request
      StartImportTask (..)
    , mkStartImportTask
    -- ** Request lenses
    , sitName
    , sitImportUrl
    , sitClientRequestToken

    -- * Destructuring the response
    , StartImportTaskResponse (..)
    , mkStartImportTaskResponse
    -- ** Response lenses
    , sitrrsTask
    , sitrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartImportTask' smart constructor.
data StartImportTask = StartImportTask'
  { name :: Types.ImportTaskName
    -- ^ A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
  , importUrl :: Types.ImportURL
    -- ^ The URL for your import file that you've uploaded to Amazon S3.
  , clientRequestToken :: Core.Maybe Types.ClientRequestToken
    -- ^ Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated.
--
-- Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartImportTask' value with any optional fields omitted.
mkStartImportTask
    :: Types.ImportTaskName -- ^ 'name'
    -> Types.ImportURL -- ^ 'importUrl'
    -> StartImportTask
mkStartImportTask name importUrl
  = StartImportTask'{name, importUrl,
                     clientRequestToken = Core.Nothing}

-- | A descriptive name for this request. You can use this name to filter future requests related to this import task, such as identifying applications and servers that were included in this import task. We recommend that you use a meaningful name for each import task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitName :: Lens.Lens' StartImportTask Types.ImportTaskName
sitName = Lens.field @"name"
{-# INLINEABLE sitName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The URL for your import file that you've uploaded to Amazon S3.
--
-- /Note:/ Consider using 'importUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitImportUrl :: Lens.Lens' StartImportTask Types.ImportURL
sitImportUrl = Lens.field @"importUrl"
{-# INLINEABLE sitImportUrl #-}
{-# DEPRECATED importUrl "Use generic-lens or generic-optics with 'importUrl' instead"  #-}

-- | Optional. A unique token that you can provide to prevent the same import request from occurring more than once. If you don't provide a token, a token is automatically generated.
--
-- Sending more than one @StartImportTask@ request with the same client request token will return information about the original import task with that client request token.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitClientRequestToken :: Lens.Lens' StartImportTask (Core.Maybe Types.ClientRequestToken)
sitClientRequestToken = Lens.field @"clientRequestToken"
{-# INLINEABLE sitClientRequestToken #-}
{-# DEPRECATED clientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead"  #-}

instance Core.ToQuery StartImportTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartImportTask where
        toHeaders StartImportTask{..}
          = Core.pure
              ("X-Amz-Target", "AWSPoseidonService_V2015_11_01.StartImportTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartImportTask where
        toJSON StartImportTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("importUrl" Core..= importUrl),
                  ("clientRequestToken" Core..=) Core.<$> clientRequestToken])

instance Core.AWSRequest StartImportTask where
        type Rs StartImportTask = StartImportTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartImportTaskResponse' Core.<$>
                   (x Core..:? "task") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartImportTaskResponse' smart constructor.
data StartImportTaskResponse = StartImportTaskResponse'
  { task :: Core.Maybe Types.ImportTask
    -- ^ An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartImportTaskResponse' value with any optional fields omitted.
mkStartImportTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartImportTaskResponse
mkStartImportTaskResponse responseStatus
  = StartImportTaskResponse'{task = Core.Nothing, responseStatus}

-- | An array of information related to the import task request including status information, times, IDs, the Amazon S3 Object URL for the import file, and more. 
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitrrsTask :: Lens.Lens' StartImportTaskResponse (Core.Maybe Types.ImportTask)
sitrrsTask = Lens.field @"task"
{-# INLINEABLE sitrrsTask #-}
{-# DEPRECATED task "Use generic-lens or generic-optics with 'task' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sitrrsResponseStatus :: Lens.Lens' StartImportTaskResponse Core.Int
sitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
