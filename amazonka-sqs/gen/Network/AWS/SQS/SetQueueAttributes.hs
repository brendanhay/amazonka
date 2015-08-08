{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.SetQueueAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the value of one or more queue attributes. When you change a
-- queue\'s attributes, the change can take up to 60 seconds for most of
-- the attributes to propagate throughout the SQS system. Changes made to
-- the @MessageRetentionPeriod@ attribute can take up to 15 minutes.
--
-- Going forward, new attributes might be added. If you are writing code
-- that calls this action, we recommend that you structure your code so
-- that it can handle new attributes gracefully.
--
-- /See:/ <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SetQueueAttributes.html AWS API Reference> for SetQueueAttributes.
module Network.AWS.SQS.SetQueueAttributes
    (
    -- * Creating a Request
      SetQueueAttributes
    , setQueueAttributes
    -- * Request Lenses
    , sqaQueueURL
    , sqaAttributes

    -- * Destructuring the Response
    , SetQueueAttributesResponse
    , setQueueAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'setQueueAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sqaQueueURL'
--
-- * 'sqaAttributes'
data SetQueueAttributes = SetQueueAttributes'
    { _sqaQueueURL   :: !Text
    , _sqaAttributes :: !(Map QueueAttributeName Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetQueueAttributes' smart constructor.
setQueueAttributes :: Text -> SetQueueAttributes
setQueueAttributes pQueueURL_ =
    SetQueueAttributes'
    { _sqaQueueURL = pQueueURL_
    , _sqaAttributes = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
sqaQueueURL :: Lens' SetQueueAttributes Text
sqaQueueURL = lens _sqaQueueURL (\ s a -> s{_sqaQueueURL = a});

-- | A map of attributes to set.
--
-- The following lists the names, descriptions, and values of the special
-- request parameters the @SetQueueAttributes@ action uses:
--
-- -   @DelaySeconds@ - The time in seconds that the delivery of all
--     messages in the queue will be delayed. An integer from 0 to 900 (15
--     minutes). The default for this attribute is 0 (zero).
-- -   @MaximumMessageSize@ - The limit of how many bytes a message can
--     contain before Amazon SQS rejects it. An integer from 1024 bytes (1
--     KiB) up to 262144 bytes (256 KiB). The default for this attribute is
--     262144 (256 KiB).
-- -   @MessageRetentionPeriod@ - The number of seconds Amazon SQS retains
--     a message. Integer representing seconds, from 60 (1 minute) to
--     1209600 (14 days). The default for this attribute is 345600 (4
--     days).
-- -   @Policy@ - The queue\'s policy. A valid AWS policy. For more
--     information about policy structure, see
--     <http://docs.aws.amazon.com/IAM/latest/UserGuide/PoliciesOverview.html Overview of AWS IAM Policies>
--     in the /Amazon IAM User Guide/.
-- -   @ReceiveMessageWaitTimeSeconds@ - The time for which a
--     ReceiveMessage call will wait for a message to arrive. An integer
--     from 0 to 20 (seconds). The default for this attribute is 0.
-- -   @VisibilityTimeout@ - The visibility timeout for the queue. An
--     integer from 0 to 43200 (12 hours). The default for this attribute
--     is 30. For more information about visibility timeout, see Visibility
--     Timeout in the /Amazon SQS Developer Guide/.
-- -   @RedrivePolicy@ - The parameters for dead letter queue functionality
--     of the source queue. For more information about RedrivePolicy and
--     dead letter queues, see Using Amazon SQS Dead Letter Queues in the
--     /Amazon SQS Developer Guide/.
sqaAttributes :: Lens' SetQueueAttributes (HashMap QueueAttributeName Text)
sqaAttributes = lens _sqaAttributes (\ s a -> s{_sqaAttributes = a}) . _Map;

instance AWSRequest SetQueueAttributes where
        type Sv SetQueueAttributes = SQS
        type Rs SetQueueAttributes =
             SetQueueAttributesResponse
        request = postQuery
        response = receiveNull SetQueueAttributesResponse'

instance ToHeaders SetQueueAttributes where
        toHeaders = const mempty

instance ToPath SetQueueAttributes where
        toPath = const "/"

instance ToQuery SetQueueAttributes where
        toQuery SetQueueAttributes'{..}
          = mconcat
              ["Action" =: ("SetQueueAttributes" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _sqaQueueURL,
               toQueryMap "Attribute" "Name" "Value" _sqaAttributes]

-- | /See:/ 'setQueueAttributesResponse' smart constructor.
data SetQueueAttributesResponse =
    SetQueueAttributesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetQueueAttributesResponse' smart constructor.
setQueueAttributesResponse :: SetQueueAttributesResponse
setQueueAttributesResponse = SetQueueAttributesResponse'
