{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.TestRole
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The TestRole operation tests the IAM role used to create the pipeline.
--
-- The @TestRole@ action lets you determine whether the IAM role you are
-- using has sufficient permissions to let Elastic Transcoder perform tasks
-- associated with the transcoding process. The action attempts to assume
-- the specified IAM role, checks read access to the input and output
-- buckets, and tries to send a test notification to Amazon SNS topics that
-- you specify.
--
-- <http://docs.aws.amazon.com/elastictranscoder/latest/developerguide/TestRole.html>
module Network.AWS.ElasticTranscoder.TestRole
    (
    -- * Request
      TestRole
    -- ** Request constructor
    , testRole
    -- ** Request lenses
    , trRole
    , trInputBucket
    , trOutputBucket
    , trTopics

    -- * Response
    , TestRoleResponse
    -- ** Response constructor
    , testRoleResponse
    -- ** Response lenses
    , trrsSuccess
    , trrsMessages
    , trrsStatus
    ) where

import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The @TestRoleRequest@ structure.
--
-- /See:/ 'testRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trRole'
--
-- * 'trInputBucket'
--
-- * 'trOutputBucket'
--
-- * 'trTopics'
data TestRole = TestRole'
    { _trRole         :: !Text
    , _trInputBucket  :: !Text
    , _trOutputBucket :: !Text
    , _trTopics       :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TestRole' smart constructor.
testRole :: Text -> Text -> Text -> TestRole
testRole pRole_ pInputBucket_ pOutputBucket_ =
    TestRole'
    { _trRole = pRole_
    , _trInputBucket = pInputBucket_
    , _trOutputBucket = pOutputBucket_
    , _trTopics = mempty
    }

-- | The IAM Amazon Resource Name (ARN) for the role that you want Elastic
-- Transcoder to test.
trRole :: Lens' TestRole Text
trRole = lens _trRole (\ s a -> s{_trRole = a});

-- | The Amazon S3 bucket that contains media files to be transcoded. The
-- action attempts to read from this bucket.
trInputBucket :: Lens' TestRole Text
trInputBucket = lens _trInputBucket (\ s a -> s{_trInputBucket = a});

-- | The Amazon S3 bucket that Elastic Transcoder will write transcoded media
-- files to. The action attempts to read from this bucket.
trOutputBucket :: Lens' TestRole Text
trOutputBucket = lens _trOutputBucket (\ s a -> s{_trOutputBucket = a});

-- | The ARNs of one or more Amazon Simple Notification Service (Amazon SNS)
-- topics that you want the action to send a test notification to.
trTopics :: Lens' TestRole [Text]
trTopics = lens _trTopics (\ s a -> s{_trTopics = a});

instance AWSRequest TestRole where
        type Sv TestRole = ElasticTranscoder
        type Rs TestRole = TestRoleResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 TestRoleResponse' <$>
                   (x .?> "Success") <*> (x .?> "Messages" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders TestRole where
        toHeaders = const mempty

instance ToJSON TestRole where
        toJSON TestRole'{..}
          = object
              ["Role" .= _trRole, "InputBucket" .= _trInputBucket,
               "OutputBucket" .= _trOutputBucket,
               "Topics" .= _trTopics]

instance ToPath TestRole where
        toPath = const "/2012-09-25/roleTests"

instance ToQuery TestRole where
        toQuery = const mempty

-- | The @TestRoleResponse@ structure.
--
-- /See:/ 'testRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trrsSuccess'
--
-- * 'trrsMessages'
--
-- * 'trrsStatus'
data TestRoleResponse = TestRoleResponse'
    { _trrsSuccess  :: !(Maybe Text)
    , _trrsMessages :: !(Maybe [Text])
    , _trrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TestRoleResponse' smart constructor.
testRoleResponse :: Int -> TestRoleResponse
testRoleResponse pStatus_ =
    TestRoleResponse'
    { _trrsSuccess = Nothing
    , _trrsMessages = Nothing
    , _trrsStatus = pStatus_
    }

-- | If the operation is successful, this value is @true@; otherwise, the
-- value is @false@.
trrsSuccess :: Lens' TestRoleResponse (Maybe Text)
trrsSuccess = lens _trrsSuccess (\ s a -> s{_trrsSuccess = a});

-- | If the @Success@ element contains @false@, this value is an array of one
-- or more error messages that were generated during the test process.
trrsMessages :: Lens' TestRoleResponse [Text]
trrsMessages = lens _trrsMessages (\ s a -> s{_trrsMessages = a}) . _Default;

-- | FIXME: Undocumented member.
trrsStatus :: Lens' TestRoleResponse Int
trrsStatus = lens _trrsStatus (\ s a -> s{_trrsStatus = a});
