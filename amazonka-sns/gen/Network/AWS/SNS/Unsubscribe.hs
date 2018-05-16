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
-- Module      : Network.AWS.SNS.Unsubscribe
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription. If the subscription requires authentication for deletion, only the owner of the subscription or the topic's owner can unsubscribe, and an AWS signature is required. If the @Unsubscribe@ call does not require authentication and the requester is not the subscription owner, a final cancellation message is delivered to the endpoint, so that the endpoint owner can easily resubscribe to the topic if the @Unsubscribe@ request was unintended.
--
--
module Network.AWS.SNS.Unsubscribe
    (
    -- * Creating a Request
      unsubscribe
    , Unsubscribe
    -- * Request Lenses
    , uSubscriptionARN

    -- * Destructuring the Response
    , unsubscribeResponse
    , UnsubscribeResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for Unsubscribe action.
--
--
--
-- /See:/ 'unsubscribe' smart constructor.
newtype Unsubscribe = Unsubscribe'
  { _uSubscriptionARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Unsubscribe' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uSubscriptionARN' - The ARN of the subscription to be deleted.
unsubscribe
    :: Text -- ^ 'uSubscriptionARN'
    -> Unsubscribe
unsubscribe pSubscriptionARN_ =
  Unsubscribe' {_uSubscriptionARN = pSubscriptionARN_}


-- | The ARN of the subscription to be deleted.
uSubscriptionARN :: Lens' Unsubscribe Text
uSubscriptionARN = lens _uSubscriptionARN (\ s a -> s{_uSubscriptionARN = a})

instance AWSRequest Unsubscribe where
        type Rs Unsubscribe = UnsubscribeResponse
        request = postQuery sns
        response = receiveNull UnsubscribeResponse'

instance Hashable Unsubscribe where

instance NFData Unsubscribe where

instance ToHeaders Unsubscribe where
        toHeaders = const mempty

instance ToPath Unsubscribe where
        toPath = const "/"

instance ToQuery Unsubscribe where
        toQuery Unsubscribe'{..}
          = mconcat
              ["Action" =: ("Unsubscribe" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "SubscriptionArn" =: _uSubscriptionARN]

-- | /See:/ 'unsubscribeResponse' smart constructor.
data UnsubscribeResponse =
  UnsubscribeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsubscribeResponse' with the minimum fields required to make a request.
--
unsubscribeResponse
    :: UnsubscribeResponse
unsubscribeResponse = UnsubscribeResponse'


instance NFData UnsubscribeResponse where
