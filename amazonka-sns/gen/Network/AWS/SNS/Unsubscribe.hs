{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription. If the subscription requires authentication for
-- deletion, only the owner of the subscription or the topic\'s owner can
-- unsubscribe, and an AWS signature is required. If the @Unsubscribe@ call
-- does not require authentication and the requester is not the
-- subscription owner, a final cancellation message is delivered to the
-- endpoint, so that the endpoint owner can easily resubscribe to the topic
-- if the @Unsubscribe@ request was unintended.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_Unsubscribe.html>
module Network.AWS.SNS.Unsubscribe
    (
    -- * Request
      Unsubscribe
    -- ** Request constructor
    , unsubscribe
    -- ** Request lenses
    , unsSubscriptionARN

    -- * Response
    , UnsubscribeResponse
    -- ** Response constructor
    , unsubscribeResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for Unsubscribe action.
--
-- /See:/ 'unsubscribe' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'unsSubscriptionARN'
newtype Unsubscribe = Unsubscribe'
    { _unsSubscriptionARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Unsubscribe' smart constructor.
unsubscribe :: Text -> Unsubscribe
unsubscribe pSubscriptionARN =
    Unsubscribe'
    { _unsSubscriptionARN = pSubscriptionARN
    }

-- | The ARN of the subscription to be deleted.
unsSubscriptionARN :: Lens' Unsubscribe Text
unsSubscriptionARN = lens _unsSubscriptionARN (\ s a -> s{_unsSubscriptionARN = a});

instance AWSRequest Unsubscribe where
        type Sv Unsubscribe = SNS
        type Rs Unsubscribe = UnsubscribeResponse
        request = post
        response = receiveNull UnsubscribeResponse'

instance ToHeaders Unsubscribe where
        toHeaders = const mempty

instance ToPath Unsubscribe where
        toPath = const "/"

instance ToQuery Unsubscribe where
        toQuery Unsubscribe'{..}
          = mconcat
              ["Action" =: ("Unsubscribe" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "SubscriptionArn" =: _unsSubscriptionARN]

-- | /See:/ 'unsubscribeResponse' smart constructor.
data UnsubscribeResponse =
    UnsubscribeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnsubscribeResponse' smart constructor.
unsubscribeResponse :: UnsubscribeResponse
unsubscribeResponse = UnsubscribeResponse'
