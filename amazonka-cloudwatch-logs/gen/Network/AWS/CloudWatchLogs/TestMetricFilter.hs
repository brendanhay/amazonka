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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the filter pattern of a metric filter against a sample of log event messages. You can use this operation to validate the correctness of a metric filter pattern.
--
--
module Network.AWS.CloudWatchLogs.TestMetricFilter
    (
    -- * Creating a Request
      testMetricFilter
    , TestMetricFilter
    -- * Request Lenses
    , tmfFilterPattern
    , tmfLogEventMessages

    -- * Destructuring the Response
    , testMetricFilterResponse
    , TestMetricFilterResponse
    -- * Response Lenses
    , tmfrsMatches
    , tmfrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testMetricFilter' smart constructor.
data TestMetricFilter = TestMetricFilter'
  { _tmfFilterPattern    :: !Text
  , _tmfLogEventMessages :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestMetricFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmfFilterPattern' - Undocumented member.
--
-- * 'tmfLogEventMessages' - The log event messages to test.
testMetricFilter
    :: Text -- ^ 'tmfFilterPattern'
    -> NonEmpty Text -- ^ 'tmfLogEventMessages'
    -> TestMetricFilter
testMetricFilter pFilterPattern_ pLogEventMessages_ =
  TestMetricFilter'
    { _tmfFilterPattern = pFilterPattern_
    , _tmfLogEventMessages = _List1 # pLogEventMessages_
    }


-- | Undocumented member.
tmfFilterPattern :: Lens' TestMetricFilter Text
tmfFilterPattern = lens _tmfFilterPattern (\ s a -> s{_tmfFilterPattern = a})

-- | The log event messages to test.
tmfLogEventMessages :: Lens' TestMetricFilter (NonEmpty Text)
tmfLogEventMessages = lens _tmfLogEventMessages (\ s a -> s{_tmfLogEventMessages = a}) . _List1

instance AWSRequest TestMetricFilter where
        type Rs TestMetricFilter = TestMetricFilterResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 TestMetricFilterResponse' <$>
                   (x .?> "matches" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable TestMetricFilter where

instance NFData TestMetricFilter where

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
              (catMaybes
                 [Just ("filterPattern" .= _tmfFilterPattern),
                  Just ("logEventMessages" .= _tmfLogEventMessages)])

instance ToPath TestMetricFilter where
        toPath = const "/"

instance ToQuery TestMetricFilter where
        toQuery = const mempty

-- | /See:/ 'testMetricFilterResponse' smart constructor.
data TestMetricFilterResponse = TestMetricFilterResponse'
  { _tmfrsMatches        :: !(Maybe [MetricFilterMatchRecord])
  , _tmfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestMetricFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmfrsMatches' - The matched events.
--
-- * 'tmfrsResponseStatus' - -- | The response status code.
testMetricFilterResponse
    :: Int -- ^ 'tmfrsResponseStatus'
    -> TestMetricFilterResponse
testMetricFilterResponse pResponseStatus_ =
  TestMetricFilterResponse'
    {_tmfrsMatches = Nothing, _tmfrsResponseStatus = pResponseStatus_}


-- | The matched events.
tmfrsMatches :: Lens' TestMetricFilterResponse [MetricFilterMatchRecord]
tmfrsMatches = lens _tmfrsMatches (\ s a -> s{_tmfrsMatches = a}) . _Default . _Coerce

-- | -- | The response status code.
tmfrsResponseStatus :: Lens' TestMetricFilterResponse Int
tmfrsResponseStatus = lens _tmfrsResponseStatus (\ s a -> s{_tmfrsResponseStatus = a})

instance NFData TestMetricFilterResponse where
