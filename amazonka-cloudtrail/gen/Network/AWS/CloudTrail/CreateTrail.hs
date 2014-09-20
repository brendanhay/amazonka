{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

-- | From the command line, use create-subscription. Creates a trail that
-- specifies the settings for delivery of log data to an Amazon S3 bucket.
module Network.AWS.CloudTrail.CreateTrail
    (
    -- * Request
      CreateTrail
    -- ** Request constructor
    , createTrail
    -- ** Request lenses
    , ctName
    , ctS3BucketName
    , ctS3KeyPrefix
    , ctSnsTopicName
    , ctIncludeGlobalServiceEvents

    -- * Response
    , CreateTrailResponse
    -- ** Response constructor
    , createTrailResponse
    -- ** Response lenses
    , ctrName
    , ctrS3BucketName
    , ctrS3KeyPrefix
    , ctrSnsTopicName
    , ctrIncludeGlobalServiceEvents
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Specifies the settings for each trail.
data CreateTrail = CreateTrail
    { _ctName :: Text
    , _ctS3BucketName :: Text
    , _ctS3KeyPrefix :: Maybe Text
    , _ctSnsTopicName :: Maybe Text
    , _ctIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTrail' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @S3BucketName ::@ @Text@
--
-- * @S3KeyPrefix ::@ @Maybe Text@
--
-- * @SnsTopicName ::@ @Maybe Text@
--
-- * @IncludeGlobalServiceEvents ::@ @Maybe Bool@
--
createTrail :: Text -- ^ 'ctName'
            -> Text -- ^ 'ctS3BucketName'
            -> CreateTrail
createTrail p1 p2 = CreateTrail
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
    { _ctrName :: Maybe Text
    , _ctrS3BucketName :: Maybe Text
    , _ctrS3KeyPrefix :: Maybe Text
    , _ctrSnsTopicName :: Maybe Text
    , _ctrIncludeGlobalServiceEvents :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTrailResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @S3BucketName ::@ @Maybe Text@
--
-- * @S3KeyPrefix ::@ @Maybe Text@
--
-- * @SnsTopicName ::@ @Maybe Text@
--
-- * @IncludeGlobalServiceEvents ::@ @Maybe Bool@
--
createTrailResponse :: CreateTrailResponse
createTrailResponse = CreateTrailResponse
    { _ctrName = Nothing
    , _ctrS3BucketName = Nothing
    , _ctrS3KeyPrefix = Nothing
    , _ctrSnsTopicName = Nothing
    , _ctrIncludeGlobalServiceEvents = Nothing
    }

-- | Specifies the name of the trail.
ctrName :: Lens' CreateTrailResponse (Maybe Text)
ctrName = lens _ctrName (\s a -> s { _ctrName = a })

-- | Specifies the name of the Amazon S3 bucket designated for publishing log
-- files.
ctrS3BucketName :: Lens' CreateTrailResponse (Maybe Text)
ctrS3BucketName = lens _ctrS3BucketName (\s a -> s { _ctrS3BucketName = a })

-- | Specifies the Amazon S3 key prefix that precedes the name of the bucket you
-- have designated for log file delivery.
ctrS3KeyPrefix :: Lens' CreateTrailResponse (Maybe Text)
ctrS3KeyPrefix = lens _ctrS3KeyPrefix (\s a -> s { _ctrS3KeyPrefix = a })

-- | Specifies the name of the Amazon SNS topic defined for notification of log
-- file delivery.
ctrSnsTopicName :: Lens' CreateTrailResponse (Maybe Text)
ctrSnsTopicName = lens _ctrSnsTopicName (\s a -> s { _ctrSnsTopicName = a })

-- | Specifies whether the trail is publishing events from global services such
-- as IAM to the log files.
ctrIncludeGlobalServiceEvents :: Lens' CreateTrailResponse (Maybe Bool)
ctrIncludeGlobalServiceEvents =
    lens _ctrIncludeGlobalServiceEvents
         (\s a -> s { _ctrIncludeGlobalServiceEvents = a })

instance FromJSON CreateTrailResponse

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request = get
    response _ = jsonResponse
