{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- With the UpdatePipelineNotifications operation, you can update Amazon Simple Notification Service (Amazon SNS) notifications for a pipeline.
--
--
-- When you update notifications for a pipeline, Elastic Transcoder returns the values that you specified in the request.
--
module Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
    (
    -- * Creating a Request
      updatePipelineNotifications
    , UpdatePipelineNotifications
    -- * Request Lenses
    , upnId
    , upnNotifications

    -- * Destructuring the Response
    , updatePipelineNotificationsResponse
    , UpdatePipelineNotificationsResponse
    -- * Response Lenses
    , upnrsPipeline
    , upnrsResponseStatus
    ) where

import Network.AWS.ElasticTranscoder.Types
import Network.AWS.ElasticTranscoder.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The @UpdatePipelineNotificationsRequest@ structure.
--
--
--
-- /See:/ 'updatePipelineNotifications' smart constructor.
data UpdatePipelineNotifications = UpdatePipelineNotifications'
  { _upnId            :: !Text
  , _upnNotifications :: !Notifications
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePipelineNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upnId' - The identifier of the pipeline for which you want to change notification settings.
--
-- * 'upnNotifications' - The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process jobs that are added to this pipeline. This is the ARN that Amazon SNS returned when you created the topic.     * __Completed__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job. This is the ARN that Amazon SNS returned when you created the topic.     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition. This is the ARN that Amazon SNS returned when you created the topic.     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition. This is the ARN that Amazon SNS returned when you created the topic.
updatePipelineNotifications
    :: Text -- ^ 'upnId'
    -> Notifications -- ^ 'upnNotifications'
    -> UpdatePipelineNotifications
updatePipelineNotifications pId_ pNotifications_ =
  UpdatePipelineNotifications'
    {_upnId = pId_, _upnNotifications = pNotifications_}


-- | The identifier of the pipeline for which you want to change notification settings.
upnId :: Lens' UpdatePipelineNotifications Text
upnId = lens _upnId (\ s a -> s{_upnId = a})

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ : The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process jobs that are added to this pipeline. This is the ARN that Amazon SNS returned when you created the topic.     * __Completed__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing a job. This is the ARN that Amazon SNS returned when you created the topic.     * __Warning__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition. This is the ARN that Amazon SNS returned when you created the topic.     * __Error__ : The topic ARN for the Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition. This is the ARN that Amazon SNS returned when you created the topic.
upnNotifications :: Lens' UpdatePipelineNotifications Notifications
upnNotifications = lens _upnNotifications (\ s a -> s{_upnNotifications = a})

instance AWSRequest UpdatePipelineNotifications where
        type Rs UpdatePipelineNotifications =
             UpdatePipelineNotificationsResponse
        request = postJSON elasticTranscoder
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePipelineNotificationsResponse' <$>
                   (x .?> "Pipeline") <*> (pure (fromEnum s)))

instance Hashable UpdatePipelineNotifications where

instance NFData UpdatePipelineNotifications where

instance ToHeaders UpdatePipelineNotifications where
        toHeaders = const mempty

instance ToJSON UpdatePipelineNotifications where
        toJSON UpdatePipelineNotifications'{..}
          = object
              (catMaybes
                 [Just ("Notifications" .= _upnNotifications)])

instance ToPath UpdatePipelineNotifications where
        toPath UpdatePipelineNotifications'{..}
          = mconcat
              ["/2012-09-25/pipelines/", toBS _upnId,
               "/notifications"]

instance ToQuery UpdatePipelineNotifications where
        toQuery = const mempty

-- | The @UpdatePipelineNotificationsResponse@ structure.
--
--
--
-- /See:/ 'updatePipelineNotificationsResponse' smart constructor.
data UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse'
  { _upnrsPipeline       :: !(Maybe Pipeline)
  , _upnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdatePipelineNotificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upnrsPipeline' - A section of the response body that provides information about the pipeline associated with this notification.
--
-- * 'upnrsResponseStatus' - -- | The response status code.
updatePipelineNotificationsResponse
    :: Int -- ^ 'upnrsResponseStatus'
    -> UpdatePipelineNotificationsResponse
updatePipelineNotificationsResponse pResponseStatus_ =
  UpdatePipelineNotificationsResponse'
    {_upnrsPipeline = Nothing, _upnrsResponseStatus = pResponseStatus_}


-- | A section of the response body that provides information about the pipeline associated with this notification.
upnrsPipeline :: Lens' UpdatePipelineNotificationsResponse (Maybe Pipeline)
upnrsPipeline = lens _upnrsPipeline (\ s a -> s{_upnrsPipeline = a})

-- | -- | The response status code.
upnrsResponseStatus :: Lens' UpdatePipelineNotificationsResponse Int
upnrsResponseStatus = lens _upnrsResponseStatus (\ s a -> s{_upnrsResponseStatus = a})

instance NFData UpdatePipelineNotificationsResponse
         where
