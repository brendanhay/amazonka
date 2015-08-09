{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.TestMetricFilter
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the filter pattern of a metric filter against a sample of log
-- event messages. You can use this operation to validate the correctness
-- of a metric filter pattern.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_TestMetricFilter.html AWS API Reference> for TestMetricFilter.
module Network.AWS.CloudWatchLogs.TestMetricFilter
    (
    -- * Creating a Request
      TestMetricFilter
    , testMetricFilter
    -- * Request Lenses
    , tmfFilterPattern
    , tmfLogEventMessages

    -- * Destructuring the Response
    , TestMetricFilterResponse
    , testMetricFilterResponse
    -- * Response Lenses
    , tmfrsMatches
    , tmfrsStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testMetricFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tmfFilterPattern'
--
-- * 'tmfLogEventMessages'
data TestMetricFilter = TestMetricFilter'
    { _tmfFilterPattern :: !Text
    , _tmfLogEventMessages :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TestMetricFilter' smart constructor.
testMetricFilter :: Text -> NonEmpty Text -> TestMetricFilter
testMetricFilter pFilterPattern_ pLogEventMessages_ = 
    TestMetricFilter'
    { _tmfFilterPattern = pFilterPattern_
    , _tmfLogEventMessages = _List1 # pLogEventMessages_
    }

-- | Undocumented member.
tmfFilterPattern :: Lens' TestMetricFilter Text
tmfFilterPattern = lens _tmfFilterPattern (\ s a -> s{_tmfFilterPattern = a});

-- | A list of log event messages to test.
tmfLogEventMessages :: Lens' TestMetricFilter (NonEmpty Text)
tmfLogEventMessages = lens _tmfLogEventMessages (\ s a -> s{_tmfLogEventMessages = a}) . _List1;

instance AWSRequest TestMetricFilter where
        type Sv TestMetricFilter = CloudWatchLogs
        type Rs TestMetricFilter = TestMetricFilterResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 TestMetricFilterResponse' <$>
                   (x .?> "matches" .!@ mempty) <*> (pure (fromEnum s)))

instance ToHeaders TestMetricFilter where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.TestMetricFilter" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TestMetricFilter where
        toJSON TestMetricFilter'{..}
          = object
              ["filterPattern" .= _tmfFilterPattern,
               "logEventMessages" .= _tmfLogEventMessages]

instance ToPath TestMetricFilter where
        toPath = const "/"

instance ToQuery TestMetricFilter where
        toQuery = const mempty

-- | /See:/ 'testMetricFilterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tmfrsMatches'
--
-- * 'tmfrsStatus'
data TestMetricFilterResponse = TestMetricFilterResponse'
    { _tmfrsMatches :: !(Maybe [MetricFilterMatchRecord])
    , _tmfrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TestMetricFilterResponse' smart constructor.
testMetricFilterResponse :: Int -> TestMetricFilterResponse
testMetricFilterResponse pStatus_ = 
    TestMetricFilterResponse'
    { _tmfrsMatches = Nothing
    , _tmfrsStatus = pStatus_
    }

-- | Undocumented member.
tmfrsMatches :: Lens' TestMetricFilterResponse [MetricFilterMatchRecord]
tmfrsMatches = lens _tmfrsMatches (\ s a -> s{_tmfrsMatches = a}) . _Default . _Coerce;

-- | Undocumented member.
tmfrsStatus :: Lens' TestMetricFilterResponse Int
tmfrsStatus = lens _tmfrsStatus (\ s a -> s{_tmfrsStatus = a});
