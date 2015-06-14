{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SQS.GetQueueAttributes
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

-- | Gets attributes for the specified queue. The following attributes are
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
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_GetQueueAttributes.html>
module Network.AWS.SQS.GetQueueAttributes
    (
    -- * Request
      GetQueueAttributes
    -- ** Request constructor
    , getQueueAttributes
    -- ** Request lenses
    , gqaAttributeNames
    , gqaQueueURL

    -- * Response
    , GetQueueAttributesResponse
    -- ** Response constructor
    , getQueueAttributesResponse
    -- ** Response lenses
    , gqarAttributes
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SQS.Types

-- | /See:/ 'getQueueAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqaAttributeNames'
--
-- * 'gqaQueueURL'
data GetQueueAttributes = GetQueueAttributes'{_gqaAttributeNames :: Maybe [QueueAttributeName], _gqaQueueURL :: Text} deriving (Eq, Read, Show)

-- | 'GetQueueAttributes' smart constructor.
getQueueAttributes :: Text -> GetQueueAttributes
getQueueAttributes pQueueURL = GetQueueAttributes'{_gqaAttributeNames = Nothing, _gqaQueueURL = pQueueURL};

-- | A list of attributes to retrieve information for.
gqaAttributeNames :: Lens' GetQueueAttributes (Maybe [QueueAttributeName])
gqaAttributeNames = lens _gqaAttributeNames (\ s a -> s{_gqaAttributeNames = a});

-- | The URL of the Amazon SQS queue to take action on.
gqaQueueURL :: Lens' GetQueueAttributes Text
gqaQueueURL = lens _gqaQueueURL (\ s a -> s{_gqaQueueURL = a});

instance AWSRequest GetQueueAttributes where
        type Sv GetQueueAttributes = SQS
        type Rs GetQueueAttributes =
             GetQueueAttributesResponse
        request = post
        response
          = receiveXMLWrapper "GetQueueAttributesResult"
              (\ s h x ->
                 GetQueueAttributesResponse' <$>
                   parseXMLMap "Attribute" "Name" "Value" x)

instance ToHeaders GetQueueAttributes where
        toHeaders = const mempty

instance ToPath GetQueueAttributes where
        toPath = const "/"

instance ToQuery GetQueueAttributes where
        toQuery GetQueueAttributes'{..}
          = mconcat
              ["Action" =: ("GetQueueAttributes" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "AttributeName" =: _gqaAttributeNames,
               "QueueUrl" =: _gqaQueueURL]

-- | /See:/ 'getQueueAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqarAttributes'
newtype GetQueueAttributesResponse = GetQueueAttributesResponse'{_gqarAttributes :: Maybe (HashMap QueueAttributeName Text)} deriving (Eq, Read, Show)

-- | 'GetQueueAttributesResponse' smart constructor.
getQueueAttributesResponse :: GetQueueAttributesResponse
getQueueAttributesResponse = GetQueueAttributesResponse'{_gqarAttributes = Nothing};

-- | A map of attributes to the respective values.
gqarAttributes :: Lens' GetQueueAttributesResponse (Maybe (HashMap QueueAttributeName Text))
gqarAttributes = lens _gqarAttributes (\ s a -> s{_gqarAttributes = a}) . mapping _Coerce;
