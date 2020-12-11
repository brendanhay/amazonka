{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    upnrsPipeline,
    upnrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @UpdatePipelineNotificationsRequest@ structure.
--
-- /See:/ 'mkUpdatePipelineNotifications' smart constructor.
data UpdatePipelineNotifications = UpdatePipelineNotifications'
  { id ::
      Lude.Text,
    notifications :: Notifications
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePipelineNotifications' with the minimum fields required to make a request.
--
-- * 'id' - The identifier of the pipeline for which you want to change notification settings.
-- * 'notifications' - The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status.
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
mkUpdatePipelineNotifications ::
  -- | 'id'
  Lude.Text ->
  -- | 'notifications'
  Notifications ->
  UpdatePipelineNotifications
mkUpdatePipelineNotifications pId_ pNotifications_ =
  UpdatePipelineNotifications'
    { id = pId_,
      notifications = pNotifications_
    }

-- | The identifier of the pipeline for which you want to change notification settings.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnId :: Lens.Lens' UpdatePipelineNotifications Lude.Text
upnId = Lens.lens (id :: UpdatePipelineNotifications -> Lude.Text) (\s a -> s {id = a} :: UpdatePipelineNotifications)
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
upnNotifications :: Lens.Lens' UpdatePipelineNotifications Notifications
upnNotifications = Lens.lens (notifications :: UpdatePipelineNotifications -> Notifications) (\s a -> s {notifications = a} :: UpdatePipelineNotifications)
{-# DEPRECATED upnNotifications "Use generic-lens or generic-optics with 'notifications' instead." #-}

instance Lude.AWSRequest UpdatePipelineNotifications where
  type
    Rs UpdatePipelineNotifications =
      UpdatePipelineNotificationsResponse
  request = Req.postJSON elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePipelineNotificationsResponse'
            Lude.<$> (x Lude..?> "Pipeline") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePipelineNotifications where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdatePipelineNotifications where
  toJSON UpdatePipelineNotifications' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Notifications" Lude..= notifications)]
      )

instance Lude.ToPath UpdatePipelineNotifications where
  toPath UpdatePipelineNotifications' {..} =
    Lude.mconcat
      ["/2012-09-25/pipelines/", Lude.toBS id, "/notifications"]

instance Lude.ToQuery UpdatePipelineNotifications where
  toQuery = Lude.const Lude.mempty

-- | The @UpdatePipelineNotificationsResponse@ structure.
--
-- /See:/ 'mkUpdatePipelineNotificationsResponse' smart constructor.
data UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse'
  { pipeline ::
      Lude.Maybe Pipeline,
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

-- | Creates a value of 'UpdatePipelineNotificationsResponse' with the minimum fields required to make a request.
--
-- * 'pipeline' - A section of the response body that provides information about the pipeline associated with this notification.
-- * 'responseStatus' - The response status code.
mkUpdatePipelineNotificationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePipelineNotificationsResponse
mkUpdatePipelineNotificationsResponse pResponseStatus_ =
  UpdatePipelineNotificationsResponse'
    { pipeline = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A section of the response body that provides information about the pipeline associated with this notification.
--
-- /Note:/ Consider using 'pipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnrsPipeline :: Lens.Lens' UpdatePipelineNotificationsResponse (Lude.Maybe Pipeline)
upnrsPipeline = Lens.lens (pipeline :: UpdatePipelineNotificationsResponse -> Lude.Maybe Pipeline) (\s a -> s {pipeline = a} :: UpdatePipelineNotificationsResponse)
{-# DEPRECATED upnrsPipeline "Use generic-lens or generic-optics with 'pipeline' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upnrsResponseStatus :: Lens.Lens' UpdatePipelineNotificationsResponse Lude.Int
upnrsResponseStatus = Lens.lens (responseStatus :: UpdatePipelineNotificationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePipelineNotificationsResponse)
{-# DEPRECATED upnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
