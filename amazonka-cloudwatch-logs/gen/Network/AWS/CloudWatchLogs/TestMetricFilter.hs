{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatchLogs.TestMetricFilter
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Tests the filter pattern of a metric filter against a sample of log
-- event messages. You can use this operation to validate the correctness
-- of a metric filter pattern.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_TestMetricFilter.html>
module Network.AWS.CloudWatchLogs.TestMetricFilter
    (
    -- * Request
      TestMetricFilter
    -- ** Request constructor
    , testMetricFilter
    -- ** Request lenses
    , tmfFilterPattern
    , tmfLogEventMessages

    -- * Response
    , TestMetricFilterResponse
    -- ** Response constructor
    , testMetricFilterResponse
    -- ** Response lenses
    , tmfrMatches
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudWatchLogs.Types

-- | /See:/ 'testMetricFilter' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tmfFilterPattern'
--
-- * 'tmfLogEventMessages'
data TestMetricFilter = TestMetricFilter'{_tmfFilterPattern :: Text, _tmfLogEventMessages :: List1 Text} deriving (Eq, Read, Show)

-- | 'TestMetricFilter' smart constructor.
testMetricFilter :: Text -> NonEmpty Text -> TestMetricFilter
testMetricFilter pFilterPattern pLogEventMessages = TestMetricFilter'{_tmfFilterPattern = pFilterPattern, _tmfLogEventMessages = _List1 # pLogEventMessages};

-- | FIXME: Undocumented member.
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
                   (x .?> "matches" .!@ mempty))

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
-- * 'tmfrMatches'
newtype TestMetricFilterResponse = TestMetricFilterResponse'{_tmfrMatches :: Maybe [MetricFilterMatchRecord]} deriving (Eq, Read, Show)

-- | 'TestMetricFilterResponse' smart constructor.
testMetricFilterResponse :: TestMetricFilterResponse
testMetricFilterResponse = TestMetricFilterResponse'{_tmfrMatches = Nothing};

-- | FIXME: Undocumented member.
tmfrMatches :: Lens' TestMetricFilterResponse [MetricFilterMatchRecord]
tmfrMatches = lens _tmfrMatches (\ s a -> s{_tmfrMatches = a}) . _Default;
