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

-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.CloudTrail.Types
    (
    -- * Service
      CloudTrail
    -- ** Error
    , JSONError

    -- * Trail
    , Trail
    , trail
    , tCloudWatchLogsLogGroupArn
    , tCloudWatchLogsRoleArn
    , tIncludeGlobalServiceEvents
    , tName
    , tS3BucketName
    , tS3KeyPrefix
    , tSnsTopicName
    ) where

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2013-11-01@ of the Amazon CloudTrail service.
data CloudTrail

instance AWSService CloudTrail where
    type Sg CloudTrail = V4
    type Er CloudTrail = JSONError

    service = Service
        { _svcAbbrev       = "CloudTrail"
        , _svcPrefix       = "cloudtrail"
        , _svcVersion      = "2013-11-01"
        , _svcTargetPrefix = Just "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101"
        , _svcJSONVersion  = Just "1.1"
        }

    handle = jsonError statusSuccess

data Trail = Trail
    { _tCloudWatchLogsLogGroupArn  :: Maybe Text
    , _tCloudWatchLogsRoleArn      :: Maybe Text
    , _tIncludeGlobalServiceEvents :: Maybe Bool
    , _tName                       :: Maybe Text
    , _tS3BucketName               :: Maybe Text
    , _tS3KeyPrefix                :: Maybe Text
    , _tSnsTopicName               :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'Trail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tCloudWatchLogsLogGroupArn' @::@ 'Maybe' 'Text'
--
-- * 'tCloudWatchLogsRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'tIncludeGlobalServiceEvents' @::@ 'Maybe' 'Bool'
--
-- * 'tName' @::@ 'Maybe' 'Text'
--
-- * 'tS3BucketName' @::@ 'Maybe' 'Text'
--
-- * 'tS3KeyPrefix' @::@ 'Maybe' 'Text'
--
-- * 'tSnsTopicName' @::@ 'Maybe' 'Text'
--
trail :: Trail
trail = Trail
    { _tName                       = Nothing
    , _tS3BucketName               = Nothing
    , _tS3KeyPrefix                = Nothing
    , _tSnsTopicName               = Nothing
    , _tIncludeGlobalServiceEvents = Nothing
    , _tCloudWatchLogsLogGroupArn  = Nothing
    , _tCloudWatchLogsRoleArn      = Nothing
    }

-- | Specifies an Amazon Resource Name (ARN), a unique identifier that represents
-- the log group to which CloudTrail logs will be delivered.
tCloudWatchLogsLogGroupArn :: Lens' Trail (Maybe Text)
tCloudWatchLogsLogGroupArn =
    lens _tCloudWatchLogsLogGroupArn
        (\s a -> s { _tCloudWatchLogsLogGroupArn = a })

-- | Specifies the role for the CloudWatch Logs endpoint to assume to write to a
-- user’s log group.
tCloudWatchLogsRoleArn :: Lens' Trail (Maybe Text)
tCloudWatchLogsRoleArn =
    lens _tCloudWatchLogsRoleArn (\s a -> s { _tCloudWatchLogsRoleArn = a })

-- | Set to True to include AWS API calls from AWS global services such as IAM.
-- Otherwise, False.
tIncludeGlobalServiceEvents :: Lens' Trail (Maybe Bool)
tIncludeGlobalServiceEvents =
    lens _tIncludeGlobalServiceEvents
        (\s a -> s { _tIncludeGlobalServiceEvents = a })

-- | Name of the trail set by calling 'CreateTrail'.
tName :: Lens' Trail (Maybe Text)
tName = lens _tName (\s a -> s { _tName = a })

-- | Name of the Amazon S3 bucket into which CloudTrail delivers your trail files.
tS3BucketName :: Lens' Trail (Maybe Text)
tS3BucketName = lens _tS3BucketName (\s a -> s { _tS3BucketName = a })

-- | Value of the Amazon S3 prefix.
tS3KeyPrefix :: Lens' Trail (Maybe Text)
tS3KeyPrefix = lens _tS3KeyPrefix (\s a -> s { _tS3KeyPrefix = a })

-- | Name of the existing Amazon SNS topic that CloudTrail uses to notify the
-- account owner when new CloudTrail log files have been delivered.
tSnsTopicName :: Lens' Trail (Maybe Text)
tSnsTopicName = lens _tSnsTopicName (\s a -> s { _tSnsTopicName = a })

instance FromJSON Trail where
    parseJSON = withObject "Trail" $ \o -> Trail
        <$> o .:? "CloudWatchLogsLogGroupArn"
        <*> o .:? "CloudWatchLogsRoleArn"
        <*> o .:? "IncludeGlobalServiceEvents"
        <*> o .:? "Name"
        <*> o .:? "S3BucketName"
        <*> o .:? "S3KeyPrefix"
        <*> o .:? "SnsTopicName"

instance ToJSON Trail where
    toJSON Trail{..} = object
        [ "Name"                       .= _tName
        , "S3BucketName"               .= _tS3BucketName
        , "S3KeyPrefix"                .= _tS3KeyPrefix
        , "SnsTopicName"               .= _tSnsTopicName
        , "IncludeGlobalServiceEvents" .= _tIncludeGlobalServiceEvents
        , "CloudWatchLogsLogGroupArn"  .= _tCloudWatchLogsLogGroupArn
        , "CloudWatchLogsRoleArn"      .= _tCloudWatchLogsRoleArn
        ]
