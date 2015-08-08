{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
-- -   @All@ - returns all values.
-- -   @ApproximateNumberOfMessages@ - returns the approximate number of
--     visible messages in a queue. For more information, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages>
--     in the /Amazon SQS Developer Guide/.
-- -   @ApproximateNumberOfMessagesNotVisible@ - returns the approximate
--     number of messages that are not timed-out and not deleted. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/ApproximateNumber.html Resources Required to Process Messages>
--     in the /Amazon SQS Developer Guide/.
-- -   @VisibilityTimeout@ - returns the visibility timeout for the queue.
--     For more information about visibility timeout, see
--     <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AboutVT.html Visibility Timeout>
--     in the /Amazon SQS Developer Guide/.
-- -   @CreatedTimestamp@ - returns the time when the queue was created
--     (epoch time in seconds).
-- -   @LastModifiedTimestamp@ - returns the time when the queue was last
--     changed (epoch time in seconds).
-- -   @Policy@ - returns the queue\'s policy.
-- -   @MaximumMessageSize@ - returns the limit of how many bytes a message
--     can contain before Amazon SQS rejects it.
-- -   @MessageRetentionPeriod@ - returns the number of seconds Amazon SQS
--     retains a message.
-- -   @QueueArn@ - returns the queue\'s Amazon resource name (ARN).
-- -   @ApproximateNumberOfMessagesDelayed@ - returns the approximate
--     number of messages that are pending to be added to the queue.
-- -   @DelaySeconds@ - returns the default delay on the queue in seconds.
-- -   @ReceiveMessageWaitTimeSeconds@ - returns the time for which a
--     ReceiveMessage call will wait for a message to arrive.
-- -   @RedrivePolicy@ - returns the parameters for dead letter queue
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
-- using the @param.n@ notation. Values of @n@ are integers starting from
-- 1. For example, a parameter list with two elements looks like this:
--
-- @&Attribute.1=this@
--
-- @&Attribute.2=that@
--
-- /See:/ <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_GetQueueAttributes.html AWS API Reference> for GetQueueAttributes.
module Network.AWS.SQS.GetQueueAttributes
    (
    -- * Creating a Request
      GetQueueAttributes
    , getQueueAttributes
    -- * Request Lenses
    , gqaAttributeNames
    , gqaQueueURL

    -- * Destructuring the Response
    , GetQueueAttributesResponse
    , getQueueAttributesResponse
    -- * Response Lenses
    , gqarsAttributes
    , gqarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'getQueueAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqaAttributeNames'
--
-- * 'gqaQueueURL'
data GetQueueAttributes = GetQueueAttributes'
    { _gqaAttributeNames :: !(Maybe [QueueAttributeName])
    , _gqaQueueURL       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetQueueAttributes' smart constructor.
getQueueAttributes :: Text -> GetQueueAttributes
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
        type Sv GetQueueAttributes = SQS
        type Rs GetQueueAttributes =
             GetQueueAttributesResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqarsAttributes'
--
-- * 'gqarsStatus'
data GetQueueAttributesResponse = GetQueueAttributesResponse'
    { _gqarsAttributes :: !(Maybe (Map QueueAttributeName Text))
    , _gqarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetQueueAttributesResponse' smart constructor.
getQueueAttributesResponse :: Int -> GetQueueAttributesResponse
getQueueAttributesResponse pStatus_ =
    GetQueueAttributesResponse'
    { _gqarsAttributes = Nothing
    , _gqarsStatus = pStatus_
    }

-- | A map of attributes to the respective values.
gqarsAttributes :: Lens' GetQueueAttributesResponse (HashMap QueueAttributeName Text)
gqarsAttributes = lens _gqarsAttributes (\ s a -> s{_gqarsAttributes = a}) . _Default . _Map;

-- | Undocumented member.
gqarsStatus :: Lens' GetQueueAttributesResponse Int
gqarsStatus = lens _gqarsStatus (\ s a -> s{_gqarsStatus = a});
