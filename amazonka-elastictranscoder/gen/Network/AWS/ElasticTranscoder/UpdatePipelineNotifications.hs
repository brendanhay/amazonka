{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticTranscoder.UpdatePipelineNotifications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | With the UpdatePipelineNotifications operation, you can update Amazon
-- Simple Notification Service (Amazon SNS) notifications for a pipeline. When
-- you update notifications for a pipeline, Elastic Transcoder returns the
-- values that you specified in the request.
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ElasticTranscoder.Types

data UpdatePipelineNotifications = UpdatePipelineNotifications
    { _upnId            :: Text
    , _upnNotifications :: Notifications
    } deriving (Eq, Show, Generic)

-- | 'UpdatePipelineNotifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upnId' @::@ 'Text'
--
-- * 'upnNotifications' @::@ 'Notifications'
--
updatePipelineNotifications :: Text -- ^ 'upnId'
                            -> Notifications -- ^ 'upnNotifications'
                            -> UpdatePipelineNotifications
updatePipelineNotifications p1 p2 = UpdatePipelineNotifications
    { _upnId            = p1
    , _upnNotifications = p2
    }

-- | The identifier of the pipeline for which you want to change notification
-- settings.
upnId :: Lens' UpdatePipelineNotifications Text
upnId = lens _upnId (\s a -> s { _upnId = a })

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS)
-- topic that you want to notify to report job status. To receive
-- notifications, you must also subscribe to the new topic in the Amazon SNS
-- console. Progressing: The topic ARN for the Amazon Simple Notification
-- Service (Amazon SNS) topic that you want to notify when Elastic
-- Transcoder has started to process jobs that are added to this pipeline.
-- This is the ARN that Amazon SNS returned when you created the topic.
-- Completed: The topic ARN for the Amazon SNS topic that you want to notify
-- when Elastic Transcoder has finished processing a job. This is the ARN
-- that Amazon SNS returned when you created the topic. Warning: The topic
-- ARN for the Amazon SNS topic that you want to notify when Elastic
-- Transcoder encounters a warning condition. This is the ARN that Amazon
-- SNS returned when you created the topic. Error: The topic ARN for the
-- Amazon SNS topic that you want to notify when Elastic Transcoder
-- encounters an error condition. This is the ARN that Amazon SNS returned
-- when you created the topic.
upnNotifications :: Lens' UpdatePipelineNotifications Notifications
upnNotifications = lens _upnNotifications (\s a -> s { _upnNotifications = a })

instance ToPath UpdatePipelineNotifications where
    toPath UpdatePipelineNotifications{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toText _upnId
        , "/notifications"
        ]

instance ToQuery UpdatePipelineNotifications where
    toQuery = const mempty

instance ToHeaders UpdatePipelineNotifications

instance ToBody UpdatePipelineNotifications where
    toBody = toBody . encode . _upnNotifications

newtype UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse
    { _upnrPipeline :: Maybe Pipeline
    } deriving (Eq, Show, Generic)

-- | 'UpdatePipelineNotificationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upnrPipeline' @::@ 'Maybe' 'Pipeline'
--
updatePipelineNotificationsResponse :: UpdatePipelineNotificationsResponse
updatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse
    { _upnrPipeline = Nothing
    }

-- | A section of the response body that provides information about the
-- pipeline.
upnrPipeline :: Lens' UpdatePipelineNotificationsResponse (Maybe Pipeline)
upnrPipeline = lens _upnrPipeline (\s a -> s { _upnrPipeline = a })

instance AWSRequest UpdatePipelineNotifications where
    type Sv UpdatePipelineNotifications = ElasticTranscoder
    type Rs UpdatePipelineNotifications = UpdatePipelineNotificationsResponse

    request  = post
    response = jsonResponse $ \h o -> UpdatePipelineNotificationsResponse
        <$> o .: "Pipeline"
