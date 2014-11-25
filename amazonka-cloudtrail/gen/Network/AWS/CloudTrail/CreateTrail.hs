{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.CreateTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | From the command line, use 'create-subscription'.
--
-- Creates a trail that specifies the settings for delivery of log data to an
-- Amazon S3 bucket.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_CreateTrail.html>
module Network.AWS.CloudTrail.CreateTrail
    (
    -- * Request
      CreateTrail
    -- ** Request constructor
    , createTrail
    -- ** Request lenses
    , ctCloudWatchLogsLogGroupArn
    , ctCloudWatchLogsRoleArn
    , ctIncludeGlobalServiceEvents
    , ctName
    , ctS3BucketName
    , ctS3KeyPrefix
    , ctSnsTopicName

    -- * Response
    , CreateTrailResponse
    -- ** Response constructor
    , createTrailResponse
    -- ** Response lenses
    , ctrCloudWatchLogsLogGroupArn
    , ctrCloudWatchLogsRoleArn
    , ctrIncludeGlobalServiceEvents
    , ctrName
    , ctrS3BucketName
    , ctrS3KeyPrefix
    , ctrSnsTopicName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.Types
import qualified GHC.Exts

data CreateTrail = CreateTrail
    { _ctCloudWatchLogsLogGroupArn  :: Maybe Text
    , _ctCloudWatchLogsRoleArn      :: Maybe Text
    , _ctIncludeGlobalServiceEvents :: Maybe Bool
    , _ctName                       :: Text
    , _ctS3BucketName               :: Text
    , _ctS3KeyPrefix                :: Maybe Text
    , _ctSnsTopicName               :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateTrail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctCloudWatchLogsLogGroupArn' @::@ 'Maybe' 'Text'
--
-- * 'ctCloudWatchLogsRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'ctIncludeGlobalServiceEvents' @::@ 'Maybe' 'Bool'
--
-- * 'ctName' @::@ 'Text'
--
-- * 'ctS3BucketName' @::@ 'Text'
--
-- * 'ctS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'ctSnsTopicName' @::@ 'Maybe' 'Text'
--
createTrail :: Text -- ^ 'ctName'
            -> Text -- ^ 'ctS3BucketName'
            -> CreateTrail
createTrail p1 p2 = CreateTrail
    { _ctName                       = p1
    , _ctS3BucketName               = p2
    , _ctS3KeyPrefix                = Nothing
    , _ctSnsTopicName               = Nothing
    , _ctIncludeGlobalServiceEvents = Nothing
    , _ctCloudWatchLogsLogGroupArn  = Nothing
    , _ctCloudWatchLogsRoleArn      = Nothing
    }

-- | Specifies a log group name using an Amazon Resource Name (ARN), a unique
-- identifier that represents the log group to which CloudTrail logs will be
-- delivered. Not required unless you specify CloudWatchLogsRoleArn.
--
ctCloudWatchLogsLogGroupArn :: Lens' CreateTrail (Maybe Text)
ctCloudWatchLogsLogGroupArn =
    lens _ctCloudWatchLogsLogGroupArn
        (\s a -> s { _ctCloudWatchLogsLogGroupArn = a })

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a
-- user’s log group.
--
ctCloudWatchLogsRoleArn :: Lens' CreateTrail (Maybe Text)
ctCloudWatchLogsRoleArn =
    lens _ctCloudWatchLogsRoleArn (\s a -> s { _ctCloudWatchLogsRoleArn = a })

-- | Specifies whether the trail is publishing events from global services such as
-- IAM to the log files.
--
ctIncludeGlobalServiceEvents :: Lens' CreateTrail (Maybe Bool)
ctIncludeGlobalServiceEvents =
    lens _ctIncludeGlobalServiceEvents
        (\s a -> s { _ctIncludeGlobalServiceEvents = a })

-- | Specifies the name of the trail.
--
ctName :: Lens' CreateTrail Text
ctName = lens _ctName (\s a -> s { _ctName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
--
ctS3BucketName :: Lens' CreateTrail Text
ctS3BucketName = lens _ctS3BucketName (\s a -> s { _ctS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
--
ctS3KeyPrefix :: Lens' CreateTrail (Maybe Text)
ctS3KeyPrefix = lens _ctS3KeyPrefix (\s a -> s { _ctS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
--
ctSnsTopicName :: Lens' CreateTrail (Maybe Text)
ctSnsTopicName = lens _ctSnsTopicName (\s a -> s { _ctSnsTopicName = a })

data CreateTrailResponse = CreateTrailResponse
    { _ctrCloudWatchLogsLogGroupArn  :: Maybe Text
    , _ctrCloudWatchLogsRoleArn      :: Maybe Text
    , _ctrIncludeGlobalServiceEvents :: Maybe Bool
    , _ctrName                       :: Maybe Text
    , _ctrS3BucketName               :: Maybe Text
    , _ctrS3KeyPrefix                :: Maybe Text
    , _ctrSnsTopicName               :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateTrailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrCloudWatchLogsLogGroupArn' @::@ 'Maybe' 'Text'
--
-- * 'ctrCloudWatchLogsRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'ctrIncludeGlobalServiceEvents' @::@ 'Maybe' 'Bool'
--
-- * 'ctrName' @::@ 'Maybe' 'Text'
--
-- * 'ctrS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'ctrS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'ctrSnsTopicName' @::@ 'Maybe' 'Text'
--
createTrailResponse :: CreateTrailResponse
createTrailResponse = CreateTrailResponse
    { _ctrName                       = Nothing
    , _ctrS3BucketName               = Nothing
    , _ctrS3KeyPrefix                = Nothing
    , _ctrSnsTopicName               = Nothing
    , _ctrIncludeGlobalServiceEvents = Nothing
    , _ctrCloudWatchLogsLogGroupArn  = Nothing
    , _ctrCloudWatchLogsRoleArn      = Nothing
    }

-- | Specifies the Amazon Resource Name (ARN) of the log group to which CloudTrail
-- logs will be delivered.
--
ctrCloudWatchLogsLogGroupArn :: Lens' CreateTrailResponse (Maybe Text)
ctrCloudWatchLogsLogGroupArn =
    lens _ctrCloudWatchLogsLogGroupArn
        (\s a -> s { _ctrCloudWatchLogsLogGroupArn = a })

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a
-- user’s log group.
--
ctrCloudWatchLogsRoleArn :: Lens' CreateTrailResponse (Maybe Text)
ctrCloudWatchLogsRoleArn =
    lens _ctrCloudWatchLogsRoleArn
        (\s a -> s { _ctrCloudWatchLogsRoleArn = a })

-- | Specifies whether the trail is publishing events from global services such as
-- IAM to the log files.
--
ctrIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctrIncludeGlobalServiceEvents =
    lens _ctrIncludeGlobalServiceEvents
        (\s a -> s { _ctrIncludeGlobalServiceEvents = a })

-- | Specifies the name of the trail.
--
ctrName :: Lens' CreateTrailResponse (Maybe Text)
ctrName = lens _ctrName (\s a -> s { _ctrName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
--
ctrS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctrS3BucketName = lens _ctrS3BucketName (\s a -> s { _ctrS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
--
ctrS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctrS3KeyPrefix = lens _ctrS3KeyPrefix (\s a -> s { _ctrS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
--
ctrSnsTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctrSnsTopicName = lens _ctrSnsTopicName (\s a -> s { _ctrSnsTopicName = a })

instance ToPath CreateTrail where
    toPath = const "/"

instance ToQuery CreateTrail where
    toQuery = const mempty

instance ToHeaders CreateTrail

instance ToJSON CreateTrail where
    toJSON CreateTrail{..} = object
        [ "Name"                       .= _ctName
        , "S3BucketName"               .= _ctS3BucketName
        , "S3KeyPrefix"                .= _ctS3KeyPrefix
        , "SnsTopicName"               .= _ctSnsTopicName
        , "IncludeGlobalServiceEvents" .= _ctIncludeGlobalServiceEvents
        , "CloudWatchLogsLogGroupArn"  .= _ctCloudWatchLogsLogGroupArn
        , "CloudWatchLogsRoleArn"      .= _ctCloudWatchLogsRoleArn
        ]

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request  = post "CreateTrail"
    response = jsonResponse

instance FromJSON CreateTrailResponse where
    parseJSON = withObject "CreateTrailResponse" $ \o -> CreateTrailResponse
        <$> o .:? "CloudWatchLogsLogGroupArn"
        <*> o .:? "CloudWatchLogsRoleArn"
        <*> o .:? "IncludeGlobalServiceEvents"
        <*> o .:? "Name"
        <*> o .:? "S3BucketName"
        <*> o .:? "S3KeyPrefix"
        <*> o .:? "SnsTopicName"
