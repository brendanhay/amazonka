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

-- Module      : Network.AWS.CloudTrail.UpdateTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | From the command line, use update-subscription. Updates the settings that
-- specify delivery of log files. Changes to a trail do not require stopping
-- the CloudTrail service. Use this action to designate an existing bucket for
-- log delivery. If the existing bucket has previously been a target for
-- CloudTrail log files, an IAM policy exists for the bucket.
module Network.AWS.CloudTrail.UpdateTrail
    (
    -- * Request
      UpdateTrail
    -- ** Request constructor
    , updateTrail
    -- ** Request lenses
    , utCloudWatchLogsLogGroupArn
    , utCloudWatchLogsRoleArn
    , utIncludeGlobalServiceEvents
    , utName
    , utS3BucketName
    , utS3KeyPrefix
    , utSnsTopicName

    -- * Response
    , UpdateTrailResponse
    -- ** Response constructor
    , updateTrailResponse
    -- ** Response lenses
    , utrCloudWatchLogsLogGroupArn
    , utrCloudWatchLogsRoleArn
    , utrIncludeGlobalServiceEvents
    , utrName
    , utrS3BucketName
    , utrS3KeyPrefix
    , utrSnsTopicName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudTrail.Types

data UpdateTrail = UpdateTrail
    { _utCloudWatchLogsLogGroupArn  :: Maybe Text
    , _utCloudWatchLogsRoleArn      :: Maybe Text
    , _utIncludeGlobalServiceEvents :: Maybe Bool
    , _utName                       :: Text
    , _utS3BucketName               :: Maybe Text
    , _utS3KeyPrefix                :: Maybe Text
    , _utSnsTopicName               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateTrail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utCloudWatchLogsLogGroupArn' @::@ 'Maybe' 'Text'
--
-- * 'utCloudWatchLogsRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'utIncludeGlobalServiceEvents' @::@ 'Maybe' 'Bool'
--
-- * 'utName' @::@ 'Text'
--
-- * 'utS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'utS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'utSnsTopicName' @::@ 'Maybe' 'Text'
--
updateTrail :: Text -- ^ 'utName'
            -> UpdateTrail
updateTrail p1 = UpdateTrail
    { _utName                       = p1
    , _utS3BucketName               = Nothing
    , _utS3KeyPrefix                = Nothing
    , _utSnsTopicName               = Nothing
    , _utIncludeGlobalServiceEvents = Nothing
    , _utCloudWatchLogsLogGroupArn  = Nothing
    , _utCloudWatchLogsRoleArn      = Nothing
    }

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will be
-- delivered. Not required unless you specify CloudWatchLogsRoleArn.
utCloudWatchLogsLogGroupArn :: Lens' UpdateTrail (Maybe Text)
utCloudWatchLogsLogGroupArn =
    lens _utCloudWatchLogsLogGroupArn
        (\s a -> s { _utCloudWatchLogsLogGroupArn = a })

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to
-- a user’s log group.
utCloudWatchLogsRoleArn :: Lens' UpdateTrail (Maybe Text)
utCloudWatchLogsRoleArn =
    lens _utCloudWatchLogsRoleArn (\s a -> s { _utCloudWatchLogsRoleArn = a })

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
utIncludeGlobalServiceEvents :: Lens' UpdateTrail (Maybe Bool)
utIncludeGlobalServiceEvents =
    lens _utIncludeGlobalServiceEvents
        (\s a -> s { _utIncludeGlobalServiceEvents = a })

-- | Specifies the name of the trail.
utName :: Lens' UpdateTrail Text
utName = lens _utName (\s a -> s { _utName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utS3BucketName :: Lens' UpdateTrail (Maybe Text)
utS3BucketName = lens _utS3BucketName (\s a -> s { _utS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
utS3KeyPrefix :: Lens' UpdateTrail (Maybe Text)
utS3KeyPrefix = lens _utS3KeyPrefix (\s a -> s { _utS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
utSnsTopicName :: Lens' UpdateTrail (Maybe Text)
utSnsTopicName = lens _utSnsTopicName (\s a -> s { _utSnsTopicName = a })

instance ToPath UpdateTrail where
    toPath = const "/"

instance ToQuery UpdateTrail where
    toQuery = const mempty

instance ToHeaders UpdateTrail

instance ToBody UpdateTrail where
    toBody = toBody . encode . _utName

data UpdateTrailResponse = UpdateTrailResponse
    { _utrCloudWatchLogsLogGroupArn  :: Maybe Text
    , _utrCloudWatchLogsRoleArn      :: Maybe Text
    , _utrIncludeGlobalServiceEvents :: Maybe Bool
    , _utrName                       :: Maybe Text
    , _utrS3BucketName               :: Maybe Text
    , _utrS3KeyPrefix                :: Maybe Text
    , _utrSnsTopicName               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateTrailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'utrCloudWatchLogsLogGroupArn' @::@ 'Maybe' 'Text'
--
-- * 'utrCloudWatchLogsRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'utrIncludeGlobalServiceEvents' @::@ 'Maybe' 'Bool'
--
-- * 'utrName' @::@ 'Maybe' 'Text'
--
-- * 'utrS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'utrS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'utrSnsTopicName' @::@ 'Maybe' 'Text'
--
updateTrailResponse :: UpdateTrailResponse
updateTrailResponse = UpdateTrailResponse
    { _utrName                       = Nothing
    , _utrS3BucketName               = Nothing
    , _utrS3KeyPrefix                = Nothing
    , _utrSnsTopicName               = Nothing
    , _utrIncludeGlobalServiceEvents = Nothing
    , _utrCloudWatchLogsLogGroupArn  = Nothing
    , _utrCloudWatchLogsRoleArn      = Nothing
    }

-- | Specifies the Amazon Resource Name (ARN) of the log group to which
-- CloudTrail logs will be delivered.
utrCloudWatchLogsLogGroupArn :: Lens' UpdateTrailResponse (Maybe Text)
utrCloudWatchLogsLogGroupArn =
    lens _utrCloudWatchLogsLogGroupArn
        (\s a -> s { _utrCloudWatchLogsLogGroupArn = a })

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to
-- a user’s log group.
utrCloudWatchLogsRoleArn :: Lens' UpdateTrailResponse (Maybe Text)
utrCloudWatchLogsRoleArn =
    lens _utrCloudWatchLogsRoleArn
        (\s a -> s { _utrCloudWatchLogsRoleArn = a })

-- | Specifies whether the trail is publishing events from global services
-- such as IAM to the log files.
utrIncludeGlobalServiceEvents :: Lens' UpdateTrailResponse (Maybe Bool)
utrIncludeGlobalServiceEvents =
    lens _utrIncludeGlobalServiceEvents
        (\s a -> s { _utrIncludeGlobalServiceEvents = a })

-- | Specifies the name of the trail.
utrName :: Lens' UpdateTrailResponse (Maybe Text)
utrName = lens _utrName (\s a -> s { _utrName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
utrS3BucketName :: Lens' UpdateTrailResponse (Maybe Text)
utrS3BucketName = lens _utrS3BucketName (\s a -> s { _utrS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket
-- you have designated for log file delivery.
utrS3KeyPrefix :: Lens' UpdateTrailResponse (Maybe Text)
utrS3KeyPrefix = lens _utrS3KeyPrefix (\s a -> s { _utrS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of
-- log file delivery.
utrSnsTopicName :: Lens' UpdateTrailResponse (Maybe Text)
utrSnsTopicName = lens _utrSnsTopicName (\s a -> s { _utrSnsTopicName = a })

-- FromJSON

instance AWSRequest UpdateTrail where
    type Sv UpdateTrail = CloudTrail
    type Rs UpdateTrail = UpdateTrailResponse

    request  = post'
    response = jsonResponse $ \h o -> UpdateTrailResponse
        <$> o .: "CloudWatchLogsLogGroupArn"
        <*> o .: "CloudWatchLogsRoleArn"
        <*> o .: "IncludeGlobalServiceEvents"
        <*> o .: "Name"
        <*> o .: "S3BucketName"
        <*> o .: "S3KeyPrefix"
        <*> o .: "SnsTopicName"
