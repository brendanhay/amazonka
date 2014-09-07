{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.CreateTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | From the command line, use create-subscription. Creates a trail that
-- specifies the settings for delivery of log data to an Amazon S3 bucket.
module Network.AWS.CloudTrail.V2013_11_01.CreateTrail
    (
    -- * Request
      CreateTrail
    -- ** Request constructor
    , mkCreateTrail
    -- ** Request lenses
    , ctName
    , ctS3BucketName
    , ctS3KeyPrefix
    , ctSnsTopicName
    , ctIncludeGlobalServiceEvents

    -- * Response
    , CreateTrailResponse
    -- ** Response lenses
    , ctrsName
    , ctrsS3BucketName
    , ctrsS3KeyPrefix
    , ctrsSnsTopicName
    , ctrsIncludeGlobalServiceEvents
    ) where

import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Specifies the settings for each trail.
data CreateTrail = CreateTrail
    { _ctName :: Text
    , _ctS3BucketName :: Text
    , _ctS3KeyPrefix :: Maybe Text
    , _ctSnsTopicName :: Maybe Text
    , _ctIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTrail' request.
mkCreateTrail :: Text -- ^ 'ctName'
              -> Text -- ^ 'ctS3BucketName'
              -> CreateTrail
mkCreateTrail p1 p2 = CreateTrail
    { _ctName = p1
    , _ctS3BucketName = p2
    , _ctS3KeyPrefix = Nothing
    , _ctSnsTopicName = Nothing
    , _ctIncludeGlobalServiceEvents = Nothing
    }

-- | Specifies the name of the trail.
ctName :: Lens' CreateTrail Text
ctName = lens _ctName (\s a -> s { _ctName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctS3BucketName :: Lens' CreateTrail Text
ctS3BucketName = lens _ctS3BucketName (\s a -> s { _ctS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctS3KeyPrefix :: Lens' CreateTrail (Maybe Text)
ctS3KeyPrefix = lens _ctS3KeyPrefix (\s a -> s { _ctS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctSnsTopicName :: Lens' CreateTrail (Maybe Text)
ctSnsTopicName = lens _ctSnsTopicName (\s a -> s { _ctSnsTopicName = a })

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctIncludeGlobalServiceEvents :: Lens' CreateTrail (Maybe Bool)
ctIncludeGlobalServiceEvents =
    lens _ctIncludeGlobalServiceEvents
         (\s a -> s { _ctIncludeGlobalServiceEvents = a })

instance ToPath CreateTrail

instance ToQuery CreateTrail

instance ToHeaders CreateTrail

instance ToJSON CreateTrail

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data CreateTrailResponse = CreateTrailResponse
    { _ctrsName :: Maybe Text
    , _ctrsS3BucketName :: Maybe Text
    , _ctrsS3KeyPrefix :: Maybe Text
    , _ctrsSnsTopicName :: Maybe Text
    , _ctrsIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Show, Generic)

-- | Specifies the name of the trail.
ctrsName :: Lens' CreateTrailResponse (Maybe Text)
ctrsName = lens _ctrsName (\s a -> s { _ctrsName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrsS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctrsS3BucketName =
    lens _ctrsS3BucketName (\s a -> s { _ctrsS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctrsS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctrsS3KeyPrefix = lens _ctrsS3KeyPrefix (\s a -> s { _ctrsS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctrsSnsTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctrsSnsTopicName =
    lens _ctrsSnsTopicName (\s a -> s { _ctrsSnsTopicName = a })

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctrsIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctrsIncludeGlobalServiceEvents =
    lens _ctrsIncludeGlobalServiceEvents
         (\s a -> s { _ctrsIncludeGlobalServiceEvents = a })

instance FromJSON CreateTrailResponse

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request = get
    response _ = jsonResponse
