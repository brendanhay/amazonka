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
    , mkTestRoleRequest
    -- ** Request lenses
    , trrRole
    , trrInputBucket
    , trrOutputBucket
    , trrTopics

    -- * Response
    , TestRoleResponse
    -- ** Response lenses
    , trsSuccess
    , trsMessages
    ) where

import           Network.AWS.ElasticTranscoder.V2012_09_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TestRole' request.
mkTestRoleRequest :: Text -- ^ 'trrRole'
                  -> Text -- ^ 'trrInputBucket'
                  -> Text -- ^ 'trrOutputBucket'
                  -> [Text] -- ^ 'trrTopics'
                  -> TestRole
mkTestRoleRequest p1 p2 p3 p4 = TestRole
    { _trrRole = p1
    , _trrInputBucket = p2
    , _trrOutputBucket = p3
    , _trrTopics = p4
    }
{-# INLINE mkTestRoleRequest #-}

data TestRole = TestRole
    { _trrRole :: Text
      -- ^ The IAM Amazon Resource Name (ARN) for the role that you want
      -- Elastic Transcoder to test.
    , _trrInputBucket :: Text
      -- ^ The Amazon S3 bucket that contains media files to be transcoded.
      -- The action attempts to read from this bucket.
    , _trrOutputBucket :: Text
      -- ^ The Amazon S3 bucket that Elastic Transcoder will write
      -- transcoded media files to. The action attempts to read from this
      -- bucket.
    , _trrTopics :: [Text]
      -- ^ The ARNs of one or more Amazon Simple Notification Service
      -- (Amazon SNS) topics that you want the action to send a test
      -- notification to.
    } deriving (Show, Generic)

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to test.
trrRole :: Lens' TestRole (Text)
trrRole = lens _trrRole (\s a -> s { _trrRole = a })
{-# INLINE trrRole #-}

-- | The Amazon S3 bucket that contains media files to be transcoded. The action
-- attempts to read from this bucket.
trrInputBucket :: Lens' TestRole (Text)
trrInputBucket = lens _trrInputBucket (\s a -> s { _trrInputBucket = a })
{-# INLINE trrInputBucket #-}

-- | The Amazon S3 bucket that Elastic Transcoder will write transcoded media
-- files to. The action attempts to read from this bucket.
trrOutputBucket :: Lens' TestRole (Text)
trrOutputBucket = lens _trrOutputBucket (\s a -> s { _trrOutputBucket = a })
{-# INLINE trrOutputBucket #-}

-- | The ARNs of one or more Amazon Simple Notification Service (Amazon SNS)
-- topics that you want the action to send a test notification to.
trrTopics :: Lens' TestRole ([Text])
trrTopics = lens _trrTopics (\s a -> s { _trrTopics = a })
{-# INLINE trrTopics #-}

instance ToPath TestRole where
    toPath = const "/2012-09-25/roleTests"

instance ToQuery TestRole

instance ToHeaders TestRole

instance ToJSON TestRole

data TestRoleResponse = TestRoleResponse
    { _trsSuccess :: Maybe Text
      -- ^ If the operation is successful, this value is true; otherwise,
      -- the value is false.
    , _trsMessages :: [Text]
      -- ^ If the Success element contains false, this value is an array of
      -- one or more error messages that were generated during the test
      -- process.
    } deriving (Show, Generic)

-- | If the operation is successful, this value is true; otherwise, the value is
-- false.
trsSuccess :: Lens' TestRoleResponse (Maybe Text)
trsSuccess = lens _trsSuccess (\s a -> s { _trsSuccess = a })
{-# INLINE trsSuccess #-}

-- | If the Success element contains false, this value is an array of one or
-- more error messages that were generated during the test process.
trsMessages :: Lens' TestRoleResponse ([Text])
trsMessages = lens _trsMessages (\s a -> s { _trsMessages = a })
{-# INLINE trsMessages #-}

instance FromJSON TestRoleResponse

instance AWSRequest TestRole where
    type Sv TestRole = ElasticTranscoder
    type Rs TestRole = TestRoleResponse

    request = post
    response _ = jsonResponse
