{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- With the UpdatePipelineNotifications operation, you can update Amazon Simple Notification Service (Amazon SNS) notifications for a pipeline.
--
-- When you update notifications for a pipeline, Elastic Transcoder returns the values that you specified in the request.
module Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
  ( -- * Creating a request
    UpdatePipelineNotifications (..),
    mkUpdatePipelineNotifications,

    -- ** Request lenses
    upnId,
    upnNotifications,

    -- * Destructuring the response
    UpdatePipelineNotificationsResponse (..),
    mkUpdatePipelineNotificationsResponse,

    -- ** Response lenses
    upnrrsPipeline,
    upnrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @UpdatePipelineNotificationsRequest@ structure.
--
-- /See:/ 'mkUpdatePipelineNotifications' smart constructor.
data UpdatePipelineNotifications = UpdatePipelineNotifications'
  { -- | The identifier of the pipeline for which you want to change notification settings.
    id :: Types.Id,
    -- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
    --
    -- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
    --
    --     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process jobs that are added to this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
    --
    --
    --     * __Complete__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job. This is the ARN that Amazon SNS returned when you created the topic.
    --
    --
    --     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition. This is the ARN that Amazon SNS returned when you created the topic.
    --
    --
    --     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition. This is the ARN that Amazon SNS returned when you created the topic.
    notifications :: Types.Notifications
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipelineNotifications' value with any optional fields omitted.
mkUpdatePipelineNotifications ::
  -- | 'id'
  Types.Id ->
  -- | 'notifications'
  Types.Notifications ->
  UpdatePipelineNotifications
mkUpdatePipelineNotifications id notifications =
  UpdatePipelineNotifications' {id, notifications}

-- | The identifier of the pipeline for which you want to change notification settings.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnId :: Lens.Lens' UpdatePipelineNotifications Types.Id
upnId = Lens.field @"id"
{-# DEPRECATED upnId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process jobs that are added to this pipeline. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Complete__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition. This is the ARN that Amazon SNS returned when you created the topic.
--
--
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnNotifications :: Lens.Lens' UpdatePipelineNotifications Types.Notifications
upnNotifications = Lens.field @"notifications"
{-# DEPRECATED upnNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

instance Core.FromJSON UpdatePipelineNotifications where
  toJSON UpdatePipelineNotifications {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Notifications" Core..= notifications)]
      )

instance Core.AWSRequest UpdatePipelineNotifications where
  type
    Rs UpdatePipelineNotifications =
      UpdatePipelineNotificationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2012-09-25/pipelines/" Core.<> (Core.toText id)
                Core.<> ("/notifications")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipelineNotificationsResponse'
            Core.<$> (x Core..:? "Pipeline") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The @UpdatePipelineNotificationsResponse@ structure.
--
-- /See:/ 'mkUpdatePipelineNotificationsResponse' smart constructor.
data UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse'
  { -- | A section of the response body that provides information about the pipeline associated with this notification.
    pipeline :: Core.Maybe Types.Pipeline,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePipelineNotificationsResponse' value with any optional fields omitted.
mkUpdatePipelineNotificationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdatePipelineNotificationsResponse
mkUpdatePipelineNotificationsResponse responseStatus =
  UpdatePipelineNotificationsResponse'
    { pipeline = Core.Nothing,
      responseStatus
    }

-- | A section of the response body that provides information about the pipeline associated with this notification.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnrrsPipeline :: Lens.Lens' UpdatePipelineNotificationsResponse (Core.Maybe Types.Pipeline)
upnrrsPipeline = Lens.field @"pipeline"
{-# DEPRECATED upnrrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnrrsResponseStatus :: Lens.Lens' UpdatePipelineNotificationsResponse Core.Int
upnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED upnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
