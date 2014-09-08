{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.UpdatePipelineNotifications
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
-- values that you specified in the request. POST
-- /2012-09-25/pipelines/1111111111111-abcde1/notifications HTTP/1.1
-- Content-Type: application/json; charset=UTF-8 Accept: */* Host:
-- elastictranscoder.[Elastic Transcoder-endpoint].amazonaws.com:443
-- x-amz-date: 20130114T174952Z Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] { "Id":"1111111111111-abcde1",
-- "Notifications":{ "Progressing":"", "Completed":"", "Warning":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors" } } Status: 202
-- Accepted x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9
-- Content-Type: application/json Content-Length:
-- [number-of-characters-in-response] Date: Mon, 14 Jan 2013 06:01:47 GMT {
-- "Id":"1111111111111-abcde1", "Notifications":{ "Completed":"",
-- "Error":"arn:aws:sns:us-east-1:111222333444:ETS_Errors", "Progressing":"",
-- "Warning":"" } }.
module Network.AWS.ElasticTranscoder.V2012_09_25.UpdatePipelineNotifications
    (
    -- * Request
      UpdatePipelineNotifications
    -- ** Request constructor
    , mkUpdatePipelineNotifications
    -- ** Request lenses
    , upnId
    , upnNotifications

    -- * Response
    , UpdatePipelineNotificationsResponse
    -- ** Response constructor
    , mkUpdatePipelineNotificationsResponse
    -- ** Response lenses
    , upnrPipeline
    ) where

import Network.AWS.ElasticTranscoder.V2012_09_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The UpdatePipelineNotificationsRequest structure.
data UpdatePipelineNotifications = UpdatePipelineNotifications
    { _upnId :: Text
    , _upnNotifications :: Notifications
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdatePipelineNotifications' request.
mkUpdatePipelineNotifications :: Text -- ^ 'upnId'
                              -> Notifications -- ^ 'upnNotifications'
                              -> UpdatePipelineNotifications
mkUpdatePipelineNotifications p1 p2 = UpdatePipelineNotifications
    { _upnId = p1
    , _upnNotifications = p2
    }

-- | The identifier of the pipeline for which you want to change notification
-- settings.
upnId :: Lens' UpdatePipelineNotifications Text
upnId = lens _upnId (\s a -> s { _upnId = a })

-- | The topic ARN for the Amazon Simple Notification Service (Amazon SNS) topic
-- that you want to notify to report job status. To receive notifications, you
-- must also subscribe to the new topic in the Amazon SNS console.
-- Progressing: The topic ARN for the Amazon Simple Notification Service
-- (Amazon SNS) topic that you want to notify when Elastic Transcoder has
-- started to process jobs that are added to this pipeline. This is the ARN
-- that Amazon SNS returned when you created the topic. Completed: The topic
-- ARN for the Amazon SNS topic that you want to notify when Elastic
-- Transcoder has finished processing a job. This is the ARN that Amazon SNS
-- returned when you created the topic. Warning: The topic ARN for the Amazon
-- SNS topic that you want to notify when Elastic Transcoder encounters a
-- warning condition. This is the ARN that Amazon SNS returned when you
-- created the topic. Error: The topic ARN for the Amazon SNS topic that you
-- want to notify when Elastic Transcoder encounters an error condition. This
-- is the ARN that Amazon SNS returned when you created the topic.
upnNotifications :: Lens' UpdatePipelineNotifications Notifications
upnNotifications =
    lens _upnNotifications (\s a -> s { _upnNotifications = a })

instance ToPath UpdatePipelineNotifications where
    toPath UpdatePipelineNotifications{..} = mconcat
        [ "/2012-09-25/pipelines/"
        , toBS _upnId
        , "/notifications"
        ]

instance ToQuery UpdatePipelineNotifications

instance ToHeaders UpdatePipelineNotifications

instance ToJSON UpdatePipelineNotifications

-- | The UpdatePipelineNotificationsResponse structure.
newtype UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse
    { _upnrPipeline :: Maybe Pipeline
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdatePipelineNotificationsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdatePipelineNotificationsResponse :: UpdatePipelineNotificationsResponse
mkUpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse
    { _upnrPipeline = Nothing
    }

-- | A section of the response body that provides information about the
-- pipeline.
upnrPipeline :: Lens' UpdatePipelineNotificationsResponse (Maybe Pipeline)
upnrPipeline = lens _upnrPipeline (\s a -> s { _upnrPipeline = a })

instance FromJSON UpdatePipelineNotificationsResponse

instance AWSRequest UpdatePipelineNotifications where
    type Sv UpdatePipelineNotifications = ElasticTranscoder
    type Rs UpdatePipelineNotifications = UpdatePipelineNotificationsResponse

    request = post
    response _ = jsonResponse
