{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CloudTrail.V2013_11_01.CreateTrail where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateTrail' request.
createTrail :: Text -- ^ '_ctrName'
            -> Text -- ^ '_ctrS3BucketName'
            -> CreateTrail
createTrail p1 p2 = CreateTrail
    { _ctrName = p1
    , _ctrS3BucketName = p2
    , _ctrIncludeGlobalServiceEvents = Nothing
    , _ctrS3KeyPrefix = Nothing
    , _ctrSnsTopicName = Nothing
    }

data CreateTrail = CreateTrail
    { _ctrName :: Text
      -- ^ Specifies the name of the trail.
    , _ctrS3BucketName :: Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _ctrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _ctrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _ctrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    } deriving (Show, Generic)

makeLenses ''CreateTrail

instance ToPath CreateTrail

instance ToQuery CreateTrail

instance ToHeaders CreateTrail

instance ToJSON CreateTrail

data CreateTrailResponse = CreateTrailResponse
    { _ctsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _ctsS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _ctsSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _ctsName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , _ctsS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    } deriving (Show, Generic)

makeLenses ''CreateTrailResponse

instance FromJSON CreateTrailResponse

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request = get
    response _ = jsonResponse
