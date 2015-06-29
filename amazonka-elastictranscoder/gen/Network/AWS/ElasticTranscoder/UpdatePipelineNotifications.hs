{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | With the UpdatePipelineNotifications operation, you can update Amazon
-- Simple Notification Service (Amazon SNS) notifications for a pipeline.
--
-- When you update notifications for a pipeline, Elastic Transcoder returns
-- the values that you specified in the request.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/UpdatePipelineNotifications.html>
module Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
    (
    -- * Request
      UpdatePipelineNotifications
    -- ** Request constructor
    , updatePipelineNotifications
    -- ** Request lenses
    , upnId
    , upnNotifications

    -- * Response
    , UpdatePipelineNotificationsResponse
    -- ** Response constructor
    , updatePipelineNotificationsResponse
    -- ** Response lenses
    , upnrPipeline
    , upnrStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @UpdatePipelineNotificationsRequest@ structure.
--
-- /See:/ 'updatePipelineNotifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upnId'
--
-- * 'upnNotifications'
data UpdatePipelineNotifications = UpdatePipelineNotifications'
    { _upnId            :: !Text
    , _upnNotifications :: !Notifications
    } deriving (Eq,Read,Show)

-- | 'UpdatePipelineNotifications' smart constructor.
updatePipelineNotifications :: Text -> Notifications -> UpdatePipelineNotifications
updatePipelineNotifications pId pNotifications =
    UpdatePipelineNotifications'
    { _upnId = pId
    , _upnNotifications = pNotifications
    }

-- | The identifier of the pipeline for which you want to change notification
-- settings.
upnId :: Lens' UpdatePipelineNotifications Text
upnId = lens _upnId (\ s a -> s{_upnId = a});

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS)
-- topic that you want to notify to report job status.
--
-- To receive notifications, you must also subscribe to the new topic in
-- the Amazon SNS console.
--
-- -   __Progressing__: The topic ARN for the Amazon Simple Notification
--     Service (Amazon SNS) topic that you want to notify when Elastic
--     Transcoder has started to process jobs that are added to this
--     pipeline. This is the ARN that Amazon SNS returned when you created
--     the topic.
-- -   __Completed__: The topic ARN for the Amazon SNS topic that you want
--     to notify when Elastic Transcoder has finished processing a job.
--     This is the ARN that Amazon SNS returned when you created the topic.
-- -   __Warning__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters a warning condition. This
--     is the ARN that Amazon SNS returned when you created the topic.
-- -   __Error__: The topic ARN for the Amazon SNS topic that you want to
--     notify when Elastic Transcoder encounters an error condition. This
--     is the ARN that Amazon SNS returned when you created the topic.
upnNotifications :: Lens' UpdatePipelineNotifications Notifications
upnNotifications = lens _upnNotifications (\ s a -> s{_upnNotifications = a});

instance AWSRequest UpdatePipelineNotifications where
        type Sv UpdatePipelineNotifications =
             ElasticTranscoder
        type Rs UpdatePipelineNotifications =
             UpdatePipelineNotificationsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdatePipelineNotificationsResponse' <$>
                   (x .?> "Pipeline") <*> (pure (fromEnum s)))

instance ToHeaders UpdatePipelineNotifications where
        toHeaders = const mempty

instance ToJSON UpdatePipelineNotifications where
        toJSON UpdatePipelineNotifications'{..}
          = object ["Notifications" .= _upnNotifications]

instance ToPath UpdatePipelineNotifications where
        toPath UpdatePipelineNotifications'{..}
          = mconcat
              ["/2012-09-25/pipelines/", toText _upnId,
               "/notifications"]

instance ToQuery UpdatePipelineNotifications where
        toQuery = const mempty

-- | The @UpdatePipelineNotificationsResponse@ structure.
--
-- /See:/ 'updatePipelineNotificationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upnrPipeline'
--
-- * 'upnrStatus'
data UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse'
    { _upnrPipeline :: !(Maybe Pipeline)
    , _upnrStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'UpdatePipelineNotificationsResponse' smart constructor.
updatePipelineNotificationsResponse :: Int -> UpdatePipelineNotificationsResponse
updatePipelineNotificationsResponse pStatus =
    UpdatePipelineNotificationsResponse'
    { _upnrPipeline = Nothing
    , _upnrStatus = pStatus
    }

-- | A section of the response body that provides information about the
-- pipeline.
upnrPipeline :: Lens' UpdatePipelineNotificationsResponse (Maybe Pipeline)
upnrPipeline = lens _upnrPipeline (\ s a -> s{_upnrPipeline = a});

-- | FIXME: Undocumented member.
upnrStatus :: Lens' UpdatePipelineNotificationsResponse Int
upnrStatus = lens _upnrStatus (\ s a -> s{_upnrStatus = a});
