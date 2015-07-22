{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- With the UpdatePipelineNotifications operation, you can update Amazon
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
    , upnrqId
    , upnrqNotifications

    -- * Response
    , UpdatePipelineNotificationsResponse
    -- ** Response constructor
    , updatePipelineNotificationsResponse
    -- ** Response lenses
    , upnrsPipeline
    , upnrsStatus
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
-- * 'upnrqId'
--
-- * 'upnrqNotifications'
data UpdatePipelineNotifications = UpdatePipelineNotifications'
    { _upnrqId            :: !Text
    , _upnrqNotifications :: !Notifications
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipelineNotifications' smart constructor.
updatePipelineNotifications :: Text -> Notifications -> UpdatePipelineNotifications
updatePipelineNotifications pId pNotifications =
    UpdatePipelineNotifications'
    { _upnrqId = pId
    , _upnrqNotifications = pNotifications
    }

-- | The identifier of the pipeline for which you want to change notification
-- settings.
upnrqId :: Lens' UpdatePipelineNotifications Text
upnrqId = lens _upnrqId (\ s a -> s{_upnrqId = a});

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
upnrqNotifications :: Lens' UpdatePipelineNotifications Notifications
upnrqNotifications = lens _upnrqNotifications (\ s a -> s{_upnrqNotifications = a});

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
          = object ["Notifications" .= _upnrqNotifications]

instance ToPath UpdatePipelineNotifications where
        toPath UpdatePipelineNotifications'{..}
          = mconcat
              ["/2012-09-25/pipelines/", toText _upnrqId,
               "/notifications"]

instance ToQuery UpdatePipelineNotifications where
        toQuery = const mempty

-- | The @UpdatePipelineNotificationsResponse@ structure.
--
-- /See:/ 'updatePipelineNotificationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upnrsPipeline'
--
-- * 'upnrsStatus'
data UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse'
    { _upnrsPipeline :: !(Maybe Pipeline)
    , _upnrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdatePipelineNotificationsResponse' smart constructor.
updatePipelineNotificationsResponse :: Int -> UpdatePipelineNotificationsResponse
updatePipelineNotificationsResponse pStatus =
    UpdatePipelineNotificationsResponse'
    { _upnrsPipeline = Nothing
    , _upnrsStatus = pStatus
    }

-- | A section of the response body that provides information about the
-- pipeline.
upnrsPipeline :: Lens' UpdatePipelineNotificationsResponse (Maybe Pipeline)
upnrsPipeline = lens _upnrsPipeline (\ s a -> s{_upnrsPipeline = a});

-- | FIXME: Undocumented member.
upnrsStatus :: Lens' UpdatePipelineNotificationsResponse Int
upnrsStatus = lens _upnrsStatus (\ s a -> s{_upnrsStatus = a});
