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
-- Module      : Network.AWS.SNS.ConfirmSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an endpoint owner's intent to receive messages by validating the token sent to the endpoint by an earlier @Subscribe@ action. If the token is valid, the action creates a new subscription and returns its Amazon Resource Name (ARN). This call requires an AWS signature only when the @AuthenticateOnUnsubscribe@ flag is set to "true".
--
--
module Network.AWS.SNS.ConfirmSubscription
    (
    -- * Creating a Request
      confirmSubscription
    , ConfirmSubscription
    -- * Request Lenses
    , csAuthenticateOnUnsubscribe
    , csTopicARN
    , csToken

    -- * Destructuring the Response
    , confirmSubscriptionResponse
    , ConfirmSubscriptionResponse
    -- * Response Lenses
    , csrsSubscriptionARN
    , csrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for ConfirmSubscription action.
--
--
--
-- /See:/ 'confirmSubscription' smart constructor.
data ConfirmSubscription = ConfirmSubscription'
  { _csAuthenticateOnUnsubscribe :: !(Maybe Text)
  , _csTopicARN                  :: !Text
  , _csToken                     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csAuthenticateOnUnsubscribe' - Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
--
-- * 'csTopicARN' - The ARN of the topic for which you wish to confirm a subscription.
--
-- * 'csToken' - Short-lived token sent to an endpoint during the @Subscribe@ action.
confirmSubscription
    :: Text -- ^ 'csTopicARN'
    -> Text -- ^ 'csToken'
    -> ConfirmSubscription
confirmSubscription pTopicARN_ pToken_ =
  ConfirmSubscription'
    { _csAuthenticateOnUnsubscribe = Nothing
    , _csTopicARN = pTopicARN_
    , _csToken = pToken_
    }


-- | Disallows unauthenticated unsubscribes of the subscription. If the value of this parameter is @true@ and the request has an AWS signature, then only the topic owner and the subscription owner can unsubscribe the endpoint. The unsubscribe action requires AWS authentication.
csAuthenticateOnUnsubscribe :: Lens' ConfirmSubscription (Maybe Text)
csAuthenticateOnUnsubscribe = lens _csAuthenticateOnUnsubscribe (\ s a -> s{_csAuthenticateOnUnsubscribe = a})

-- | The ARN of the topic for which you wish to confirm a subscription.
csTopicARN :: Lens' ConfirmSubscription Text
csTopicARN = lens _csTopicARN (\ s a -> s{_csTopicARN = a})

-- | Short-lived token sent to an endpoint during the @Subscribe@ action.
csToken :: Lens' ConfirmSubscription Text
csToken = lens _csToken (\ s a -> s{_csToken = a})

instance AWSRequest ConfirmSubscription where
        type Rs ConfirmSubscription =
             ConfirmSubscriptionResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "ConfirmSubscriptionResult"
              (\ s h x ->
                 ConfirmSubscriptionResponse' <$>
                   (x .@? "SubscriptionArn") <*> (pure (fromEnum s)))

instance Hashable ConfirmSubscription where

instance NFData ConfirmSubscription where

instance ToHeaders ConfirmSubscription where
        toHeaders = const mempty

instance ToPath ConfirmSubscription where
        toPath = const "/"

instance ToQuery ConfirmSubscription where
        toQuery ConfirmSubscription'{..}
          = mconcat
              ["Action" =: ("ConfirmSubscription" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "AuthenticateOnUnsubscribe" =:
                 _csAuthenticateOnUnsubscribe,
               "TopicArn" =: _csTopicARN, "Token" =: _csToken]

-- | Response for ConfirmSubscriptions action.
--
--
--
-- /See:/ 'confirmSubscriptionResponse' smart constructor.
data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse'
  { _csrsSubscriptionARN :: !(Maybe Text)
  , _csrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsSubscriptionARN' - The ARN of the created subscription.
--
-- * 'csrsResponseStatus' - -- | The response status code.
confirmSubscriptionResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> ConfirmSubscriptionResponse
confirmSubscriptionResponse pResponseStatus_ =
  ConfirmSubscriptionResponse'
    {_csrsSubscriptionARN = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | The ARN of the created subscription.
csrsSubscriptionARN :: Lens' ConfirmSubscriptionResponse (Maybe Text)
csrsSubscriptionARN = lens _csrsSubscriptionARN (\ s a -> s{_csrsSubscriptionARN = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' ConfirmSubscriptionResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData ConfirmSubscriptionResponse where
