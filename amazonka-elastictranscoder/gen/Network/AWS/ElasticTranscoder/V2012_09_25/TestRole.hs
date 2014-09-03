{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticTranscoder.V2012_09_25.TestRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The TestRole operation tests the IAM role used to create the pipeline. The
-- TestRole action lets you determine whether the IAM role you are using has
-- sufficient permissions to let Elastic Transcoder perform tasks associated
-- with the transcoding process. The action attempts to assume the specified
-- IAM role, checks read access to the input and output buckets, and tries to
-- send a test notification to Amazon SNS topics that you specify. POST
-- /2012-09-25/roleTests HTTP/1.1 Content-Type: application/json;
-- charset=UTF-8 Accept: */* Host: elastictranscoder.[Elastic
-- Transcoder-endpoint].amazonaws.com:443 x-amz-date: 20130114T174952Z
-- Authorization: AWS4-HMAC-SHA256
-- Credential=[access-key-id]/[request-date]/[Elastic
-- Transcoder-endpoint]/ets/aws4_request,
-- SignedHeaders=host;x-amz-date;x-amz-target,
-- Signature=[calculated-signature] Content-Length:
-- [number-of-characters-in-JSON-string] {
-- "InputBucket":"salesoffice.example.com-source",
-- "OutputBucket":"salesoffice.example.com-public-promos",
-- "Role":"arn:aws:iam::123456789012:role/transcode-service", "Topics":
-- ["arn:aws:sns:us-east-1:111222333444:ETS_Errors",
-- "arn:aws:sns:us-east-1:111222333444:ETS_Progressing"] } Status: 200 OK
-- x-amzn-RequestId: c321ec43-378e-11e2-8e4c-4d5b971203e9 Content-Type:
-- application/json Content-Length: [number-of-characters-in-response] Date:
-- Mon, 14 Jan 2013 06:01:47 GMT { "Messages":[ "The role
-- arn:aws:iam::123456789012:role/transcode-service does not have access to
-- the bucket: salesoffice.example.com-source", "The role
-- arn:aws:iam::123456789012:role/transcode-service does not have access to
-- the topic: arn:aws:sns:us-east-1:111222333444:ETS_Errors" ], "Success":
-- "false" }.
module Network.AWS.ElasticTranscoder.V2012_09_25.TestRole
    (
    -- * Request
      TestRole
    -- ** Request constructor
    , testRole
    -- ** Request lenses
    , trrInputBucket
    , trrOutputBucket
    , trrRole
    , trrTopics

    -- * Response
    , TestRoleResponse
    -- ** Response lenses
    , trsMessages
    , trsSuccess
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'TestRole' request.
testRole :: Text -- ^ 'trrInputBucket'
         -> Text -- ^ 'trrOutputBucket'
         -> Text -- ^ 'trrRole'
         -> [Text] -- ^ 'trrTopics'
         -> TestRole
testRole p1 p2 p3 p4 = TestRole
    { _trrInputBucket = p1
    , _trrOutputBucket = p2
    , _trrRole = p3
    , _trrTopics = p4
    }

data TestRole = TestRole
    { _trrInputBucket :: Text
      -- ^ The Amazon S3 bucket that contains media files to be transcoded.
      -- The action attempts to read from this bucket.
    , _trrOutputBucket :: Text
      -- ^ The Amazon S3 bucket that Elastic Transcoder will write
      -- transcoded media files to. The action attempts to read from this
      -- bucket.
    , _trrRole :: Text
      -- ^ The IAM Amazon Resource Name (ARN) for the role that you want
      -- Elastic Transcoder to test.
    , _trrTopics :: [Text]
      -- ^ The ARNs of one or more Amazon Simple Notification Service
      -- (Amazon SNS) topics that you want the action to send a test
      -- notification to.
    } deriving (Show, Generic)

-- | The Amazon S3 bucket that contains media files to be transcoded. The action
-- attempts to read from this bucket.
trrInputBucket
    :: Functor f
    => (Text
    -> f (Text))
    -> TestRole
    -> f TestRole
trrInputBucket f x =
    (\y -> x { _trrInputBucket = y })
       <$> f (_trrInputBucket x)
{-# INLINE trrInputBucket #-}

-- | The Amazon S3 bucket that Elastic Transcoder will write transcoded media
-- files to. The action attempts to read from this bucket.
trrOutputBucket
    :: Functor f
    => (Text
    -> f (Text))
    -> TestRole
    -> f TestRole
trrOutputBucket f x =
    (\y -> x { _trrOutputBucket = y })
       <$> f (_trrOutputBucket x)
{-# INLINE trrOutputBucket #-}

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to test.
trrRole
    :: Functor f
    => (Text
    -> f (Text))
    -> TestRole
    -> f TestRole
trrRole f x =
    (\y -> x { _trrRole = y })
       <$> f (_trrRole x)
{-# INLINE trrRole #-}

-- | The ARNs of one or more Amazon Simple Notification Service (Amazon SNS)
-- topics that you want the action to send a test notification to.
trrTopics
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> TestRole
    -> f TestRole
trrTopics f x =
    (\y -> x { _trrTopics = y })
       <$> f (_trrTopics x)
{-# INLINE trrTopics #-}

instance ToPath TestRole where
    toPath = const "/2012-09-25/roleTests"

instance ToQuery TestRole

instance ToHeaders TestRole

instance ToJSON TestRole

data TestRoleResponse = TestRoleResponse
    { _trsMessages :: [Text]
      -- ^ If the Success element contains false, this value is an array of
      -- one or more error messages that were generated during the test
      -- process.
    , _trsSuccess :: Maybe Text
      -- ^ If the operation is successful, this value is true; otherwise,
      -- the value is false.
    } deriving (Show, Generic)

-- | If the Success element contains false, this value is an array of one or
-- more error messages that were generated during the test process.
trsMessages
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> TestRoleResponse
    -> f TestRoleResponse
trsMessages f x =
    (\y -> x { _trsMessages = y })
       <$> f (_trsMessages x)
{-# INLINE trsMessages #-}

-- | If the operation is successful, this value is true; otherwise, the value is
-- false.
trsSuccess
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TestRoleResponse
    -> f TestRoleResponse
trsSuccess f x =
    (\y -> x { _trsSuccess = y })
       <$> f (_trsSuccess x)
{-# INLINE trsSuccess #-}

instance FromJSON TestRoleResponse

instance AWSRequest TestRole where
    type Sv TestRole = ElasticTranscoder
    type Rs TestRole = TestRoleResponse

    request = post
    response _ = jsonResponse
