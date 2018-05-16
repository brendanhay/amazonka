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
-- Module      : Network.AWS.CloudWatchEvents.TestEventPattern
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests whether the specified event pattern matches the provided event.
--
--
-- Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.
--
module Network.AWS.CloudWatchEvents.TestEventPattern
    (
    -- * Creating a Request
      testEventPattern
    , TestEventPattern
    -- * Request Lenses
    , tepEventPattern
    , tepEvent

    -- * Destructuring the Response
    , testEventPatternResponse
    , TestEventPatternResponse
    -- * Response Lenses
    , teprsResult
    , teprsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'testEventPattern' smart constructor.
data TestEventPattern = TestEventPattern'
  { _tepEventPattern :: !Text
  , _tepEvent        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestEventPattern' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tepEventPattern' - The event pattern. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
--
-- * 'tepEvent' - The event, in JSON format, to test against the event pattern.
testEventPattern
    :: Text -- ^ 'tepEventPattern'
    -> Text -- ^ 'tepEvent'
    -> TestEventPattern
testEventPattern pEventPattern_ pEvent_ =
  TestEventPattern' {_tepEventPattern = pEventPattern_, _tepEvent = pEvent_}


-- | The event pattern. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/CloudWatchEventsandEventPatterns.html Events and Event Patterns> in the /Amazon CloudWatch Events User Guide/ .
tepEventPattern :: Lens' TestEventPattern Text
tepEventPattern = lens _tepEventPattern (\ s a -> s{_tepEventPattern = a})

-- | The event, in JSON format, to test against the event pattern.
tepEvent :: Lens' TestEventPattern Text
tepEvent = lens _tepEvent (\ s a -> s{_tepEvent = a})

instance AWSRequest TestEventPattern where
        type Rs TestEventPattern = TestEventPatternResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 TestEventPatternResponse' <$>
                   (x .?> "Result") <*> (pure (fromEnum s)))

instance Hashable TestEventPattern where

instance NFData TestEventPattern where

instance ToHeaders TestEventPattern where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.TestEventPattern" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TestEventPattern where
        toJSON TestEventPattern'{..}
          = object
              (catMaybes
                 [Just ("EventPattern" .= _tepEventPattern),
                  Just ("Event" .= _tepEvent)])

instance ToPath TestEventPattern where
        toPath = const "/"

instance ToQuery TestEventPattern where
        toQuery = const mempty

-- | /See:/ 'testEventPatternResponse' smart constructor.
data TestEventPatternResponse = TestEventPatternResponse'
  { _teprsResult         :: !(Maybe Bool)
  , _teprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TestEventPatternResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'teprsResult' - Indicates whether the event matches the event pattern.
--
-- * 'teprsResponseStatus' - -- | The response status code.
testEventPatternResponse
    :: Int -- ^ 'teprsResponseStatus'
    -> TestEventPatternResponse
testEventPatternResponse pResponseStatus_ =
  TestEventPatternResponse'
    {_teprsResult = Nothing, _teprsResponseStatus = pResponseStatus_}


-- | Indicates whether the event matches the event pattern.
teprsResult :: Lens' TestEventPatternResponse (Maybe Bool)
teprsResult = lens _teprsResult (\ s a -> s{_teprsResult = a})

-- | -- | The response status code.
teprsResponseStatus :: Lens' TestEventPatternResponse Int
teprsResponseStatus = lens _teprsResponseStatus (\ s a -> s{_teprsResponseStatus = a})

instance NFData TestEventPatternResponse where
