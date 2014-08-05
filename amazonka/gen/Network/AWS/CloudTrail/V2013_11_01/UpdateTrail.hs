{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.UpdateTrail
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
-- CloudTrail log files, an IAM policy exists for the bucket. Support for
-- passing Trail as a parameter ends as early as February 25, 2014. The
-- request and response examples in this topic show the use of parameters as
-- well as a Trail object. Until Trail is removed, you can use either Trail or
-- the parameter list.
module Network.AWS.CloudTrail.V2013_11_01.UpdateTrail where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateTrail' request.
updateTrail :: UpdateTrail
updateTrail = UpdateTrail
    { _utrIncludeGlobalServiceEvents = Nothing
    , _utrS3KeyPrefix = Nothing
    , _utrSnsTopicName = Nothing
    , _utrName = Nothing
    , _utrS3BucketName = Nothing
    , _utrTrail = Nothing
    }

data UpdateTrail = UpdateTrail
    { _utrIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _utrS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _utrSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _utrName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , _utrS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _utrTrail :: Maybe Trail
      -- ^ Support for passing a Trail object in the CreateTrail or
      -- UpdateTrail actions will end as early as February 15, 2014.
      -- Instead of the Trail object and its members, use the parameters
      -- listed for these actions.
    } deriving (Show, Generic)

makeLenses ''UpdateTrail

instance ToPath UpdateTrail

instance ToQuery UpdateTrail

instance ToHeaders UpdateTrail

instance ToJSON UpdateTrail

data UpdateTrailResponse = UpdateTrailResponse
    { _utsIncludeGlobalServiceEvents :: Maybe Bool
      -- ^ Specifies whether the trail is publishing events from global
      -- services such as IAM to the log files.
    , _utsS3KeyPrefix :: Maybe Text
      -- ^ Specifies the Amazon S3 key prefix that precedes the name of the
      -- bucket you have designated for log file delivery.
    , _utsSnsTopicName :: Maybe Text
      -- ^ Specifies the name of the Amazon SNS topic defined for
      -- notification of log file delivery.
    , _utsName :: Maybe Text
      -- ^ Specifies the name of the trail.
    , _utsS3BucketName :: Maybe Text
      -- ^ Specifies the name of the Amazon S3 bucket designated for
      -- publishing log files.
    , _utsTrail :: Maybe Trail
      -- ^ Represents the CloudTrail settings that were updated by calling
      -- UpdateTrail.
    } deriving (Show, Generic)

makeLenses ''UpdateTrailResponse

instance FromJSON UpdateTrailResponse

instance AWSRequest UpdateTrail where
    type Sv UpdateTrail = CloudTrail
    type Rs UpdateTrail = UpdateTrailResponse

    request = get
    response _ = undefined
