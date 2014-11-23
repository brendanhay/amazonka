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

-- Module      : Network.AWS.ElasticTranscoder.TestRole
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
-- send a test notification to Amazon SNS topics that you specify.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/TestRole.html>
module Network.AWS.ElasticTranscoder.TestRole
    (
    -- * Request
      TestRole
    -- ** Request constructor
    , testRole
    -- ** Request lenses
    , trInputBucket
    , trOutputBucket
    , trRole
    , trTopics

    -- * Response
    , TestRoleResponse
    -- ** Response constructor
    , testRoleResponse
    -- ** Response lenses
    , trrMessages
    , trrSuccess
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.ElasticTranscoder.Types
import qualified GHC.Exts

data TestRole = TestRole
    { _trInputBucket  :: Text
    , _trOutputBucket :: Text
    , _trRole         :: Text
    , _trTopics       :: List "Topics" Text
    } deriving (Eq, Ord, Show)

-- | 'TestRole' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trInputBucket' @::@ 'Text'
--
-- * 'trOutputBucket' @::@ 'Text'
--
-- * 'trRole' @::@ 'Text'
--
-- * 'trTopics' @::@ ['Text']
--
testRole :: Text -- ^ 'trRole'
         -> Text -- ^ 'trInputBucket'
         -> Text -- ^ 'trOutputBucket'
         -> TestRole
testRole p1 p2 p3 = TestRole
    { _trRole         = p1
    , _trInputBucket  = p2
    , _trOutputBucket = p3
    , _trTopics       = mempty
    }

-- | The Amazon S3 bucket that contains media files to be transcoded. The
-- action attempts to read from this bucket.
trInputBucket :: Lens' TestRole Text
trInputBucket = lens _trInputBucket (\s a -> s { _trInputBucket = a })

-- | The Amazon S3 bucket that Elastic Transcoder will write transcoded media
-- files to. The action attempts to read from this bucket.
trOutputBucket :: Lens' TestRole Text
trOutputBucket = lens _trOutputBucket (\s a -> s { _trOutputBucket = a })

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to test.
trRole :: Lens' TestRole Text
trRole = lens _trRole (\s a -> s { _trRole = a })

-- | The ARNs of one or more Amazon Simple Notification Service (Amazon SNS)
-- topics that you want the action to send a test notification to.
trTopics :: Lens' TestRole [Text]
trTopics = lens _trTopics (\s a -> s { _trTopics = a }) . _List

data TestRoleResponse = TestRoleResponse
    { _trrMessages :: List "Messages" Text
    , _trrSuccess  :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'TestRoleResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trrMessages' @::@ ['Text']
--
-- * 'trrSuccess' @::@ 'Maybe' 'Text'
--
testRoleResponse :: TestRoleResponse
testRoleResponse = TestRoleResponse
    { _trrSuccess  = Nothing
    , _trrMessages = mempty
    }

-- | If the Success element contains false, this value is an array of one or
-- more error messages that were generated during the test process.
trrMessages :: Lens' TestRoleResponse [Text]
trrMessages = lens _trrMessages (\s a -> s { _trrMessages = a }) . _List

-- | If the operation is successful, this value is true; otherwise, the value
-- is false.
trrSuccess :: Lens' TestRoleResponse (Maybe Text)
trrSuccess = lens _trrSuccess (\s a -> s { _trrSuccess = a })

instance ToPath TestRole where
    toPath = const "/2012-09-25/roleTests"

instance ToQuery TestRole where
    toQuery = const mempty

instance ToHeaders TestRole

instance ToJSON TestRole where
    toJSON TestRole{..} = object
        [ "Role"         .= _trRole
        , "InputBucket"  .= _trInputBucket
        , "OutputBucket" .= _trOutputBucket
        , "Topics"       .= _trTopics
        ]

instance AWSRequest TestRole where
    type Sv TestRole = ElasticTranscoder
    type Rs TestRole = TestRoleResponse

    request  = post
    response = jsonResponse

instance FromJSON TestRoleResponse where
    parseJSON = withObject "TestRoleResponse" $ \o -> TestRoleResponse
        <$> o .:? "Messages"
        <*> o .:? "Success"
