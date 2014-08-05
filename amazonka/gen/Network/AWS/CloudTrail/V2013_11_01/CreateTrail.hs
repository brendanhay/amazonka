{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- Support for passing Trail as a parameter ends as early as February 25,
-- 2014. The request and response examples in this topic show the use of
-- parameters as well as a Trail object. Until Trail is removed, you can use
-- either Trail or the parameter list.
module Network.AWS.CloudTrail.V2013_11_01.CreateTrail where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateTrail' request.
createTrail :: CreateTrail
createTrail = CreateTrail
    { _ctrIncludeGlobalServiceEvents = Nothing
    , _ctrS3KeyPrefix = Nothing
    , _ctrSnsTopicName = Nothing
    , _ctrName = Nothing
    , _ctrS3BucketName = Nothing
    , _ctrTrail = Nothing
    }

data CreateTrail = CreateTrail
    { _ctrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _ctrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _ctrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _ctrName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , _ctrS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _ctrTrail :: Maybe Trail
      -- ^ Support for passing a Trail object in the CreateTrail or
      -- UpdateTrail actions will end as early as February 15, 2014.
      -- Instead of the Trail object and its members, use the parameters
      -- listed for these actions.
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
    , _ctsTrail :: Maybe Trail
      -- ^ Support for passing a Trail object in the CreateTrail or
      -- UpdateTrail actions will end as early as February 15, 2014.
      -- Instead of the Trail object and its members, use the parameters
      -- listed for these actions.
    } deriving (Show, Generic)

makeLenses ''CreateTrailResponse

instance FromJSON CreateTrailResponse

instance AWSRequest CreateTrail where
    type Sv CreateTrail = CloudTrail
    type Rs CreateTrail = CreateTrailResponse

    request = get
    response _ = undefined
