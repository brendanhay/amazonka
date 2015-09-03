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
-- Module      : Network.AWS.SQS.GetQueueAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets attributes for the specified queue. The following attributes are
-- supported:
--
-- -   'All' - returns all values.
-- -   'ApproximateNumberOfMessages' - returns the approximate number of
--     visible messages in a queue. For more information, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages>
--     in the /Amazon SQS Developer Guide/.
-- -   'ApproximateNumberOfMessagesNotVisible' - returns the approximate
--     number of messages that are not timed-out and not deleted. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages>
--     in the /Amazon SQS Developer Guide/.
-- -   'VisibilityTimeout' - returns the visibility timeout for the queue.
--     For more information about visibility timeout, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
-- -   'CreatedTimestamp' - returns the time when the queue was created
--     (epoch time in seconds).
-- -   'LastModifiedTimestamp' - returns the time when the queue was last
--     changed (epoch time in seconds).
-- -   'Policy' - returns the queue\'s policy.
-- -   'MaximumMessageSize' - returns the limit of how many bytes a message
--     can contain before Amazon SQS rejects it.
-- -   'MessageRetentionPeriod' - returns the number of seconds Amazon SQS
--     retains a message.
-- -   'QueueArn' - returns the queue\'s Amazon resource name (ARN).
-- -   'ApproximateNumberOfMessagesDelayed' - returns the approximate
--     number of messages that are pending to be added to the queue.
-- -   'DelaySeconds' - returns the default delay on the queue in seconds.
-- -   'ReceiveMessageWaitTimeSeconds' - returns the time for which a
--     ReceiveMessage call will wait for a message to arrive.
-- -   'RedrivePolicy' - returns the parameters for dead letter queue
--     functionality of the source queue. For more information about
--     RedrivePolicy and dead letter queues, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/SQSDeadLetterQueue.html Using Amazon SQS Dead Letter Queues>
--     in the /Amazon SQS Developer Guide/.
--
-- Going forward, new attributes might be added. If you are writing code
-- that calls this action, we recommend that you structure your code so
-- that it can handle new attributes gracefully.
--
-- Some API actions take lists of parameters. These lists are specified
-- using the 'param.n' notation. Values of 'n' are integers starting from
-- 1. For example, a parameter list with two elements looks like this:
--
-- '&Attribute.1=this'
--
-- '&Attribute.2=that'
--
-- /See:/ <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_GetQueueAttributes.html AWS API Reference> for GetQueueAttributes.
module Network.AWS.SQS.GetQueueAttributes
    (
    -- * Creating a Request
      getQueueAttributes
    , GetQueueAttributes
    -- * Request Lenses
    , gqaAttributeNames
    , gqaQueueURL

    -- * Destructuring the Response
    , getQueueAttributesResponse
    , GetQueueAttributesResponse
    -- * Response Lenses
    , gqarsAttributes
    , gqarsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types
import           Network.AWS.SQS.Types.Product

-- | /See:/ 'getQueueAttributes' smart constructor.
data GetQueueAttributes = GetQueueAttributes'
    { _gqaAttributeNames :: !(Maybe [QueueAttributeName])
    , _gqaQueueURL       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetQueueAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqaAttributeNames'
--
-- * 'gqaQueueURL'
getQueueAttributes
    :: Text -- ^ 'gqaQueueURL'
    -> GetQueueAttributes
getQueueAttributes pQueueURL_ =
    GetQueueAttributes'
    { _gqaAttributeNames = Nothing
    , _gqaQueueURL = pQueueURL_
    }

-- | A list of attributes to retrieve information for.
gqaAttributeNames :: Lens' GetQueueAttributes [QueueAttributeName]
gqaAttributeNames = lens _gqaAttributeNames (\ s a -> s{_gqaAttributeNames = a}) . _Default . _Coerce;

-- | The URL of the Amazon SQS queue to take action on.
gqaQueueURL :: Lens' GetQueueAttributes Text
gqaQueueURL = lens _gqaQueueURL (\ s a -> s{_gqaQueueURL = a});

instance AWSRequest GetQueueAttributes where
        type Rs GetQueueAttributes =
             GetQueueAttributesResponse
        request = postQuery sQS
        response
          = receiveXMLWrapper "GetQueueAttributesResult"
              (\ s h x ->
                 GetQueueAttributesResponse' <$>
                   (may (parseXMLMap "Attribute" "Name" "Value") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetQueueAttributes where
        toHeaders = const mempty

instance ToPath GetQueueAttributes where
        toPath = const "/"

instance ToQuery GetQueueAttributes where
        toQuery GetQueueAttributes'{..}
          = mconcat
              ["Action" =: ("GetQueueAttributes" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               toQuery
                 (toQueryList "AttributeName" <$> _gqaAttributeNames),
               "QueueUrl" =: _gqaQueueURL]

-- | A list of returned queue attributes.
--
-- /See:/ 'getQueueAttributesResponse' smart constructor.
data GetQueueAttributesResponse = GetQueueAttributesResponse'
    { _gqarsAttributes     :: !(Maybe (Map QueueAttributeName Text))
    , _gqarsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetQueueAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqarsAttributes'
--
-- * 'gqarsResponseStatus'
getQueueAttributesResponse
    :: Int -- ^ 'gqarsResponseStatus'
    -> GetQueueAttributesResponse
getQueueAttributesResponse pResponseStatus_ =
    GetQueueAttributesResponse'
    { _gqarsAttributes = Nothing
    , _gqarsResponseStatus = pResponseStatus_
    }

-- | A map of attributes to the respective values.
gqarsAttributes :: Lens' GetQueueAttributesResponse (HashMap QueueAttributeName Text)
gqarsAttributes = lens _gqarsAttributes (\ s a -> s{_gqarsAttributes = a}) . _Default . _Map;

-- | The response status code.
gqarsResponseStatus :: Lens' GetQueueAttributesResponse Int
gqarsResponseStatus = lens _gqarsResponseStatus (\ s a -> s{_gqarsResponseStatus = a});
