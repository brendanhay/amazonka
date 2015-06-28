{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.ConfirmSubscription
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

-- | Verifies an endpoint owner\'s intent to receive messages by validating
-- the token sent to the endpoint by an earlier @Subscribe@ action. If the
-- token is valid, the action creates a new subscription and returns its
-- Amazon Resource Name (ARN). This call requires an AWS signature only
-- when the @AuthenticateOnUnsubscribe@ flag is set to \"true\".
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ConfirmSubscription.html>
module Network.AWS.SNS.ConfirmSubscription
    (
    -- * Request
      ConfirmSubscription
    -- ** Request constructor
    , confirmSubscription
    -- ** Request lenses
    , csAuthenticateOnUnsubscribe
    , csTopicARN
    , csToken

    -- * Response
    , ConfirmSubscriptionResponse
    -- ** Response constructor
    , confirmSubscriptionResponse
    -- ** Response lenses
    , csrSubscriptionARN
    , csrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for ConfirmSubscription action.
--
-- /See:/ 'confirmSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csAuthenticateOnUnsubscribe'
--
-- * 'csTopicARN'
--
-- * 'csToken'
data ConfirmSubscription = ConfirmSubscription'
    { _csAuthenticateOnUnsubscribe :: !(Maybe Text)
    , _csTopicARN                  :: !Text
    , _csToken                     :: !Text
    } deriving (Eq,Read,Show)

-- | 'ConfirmSubscription' smart constructor.
confirmSubscription :: Text -> Text -> ConfirmSubscription
confirmSubscription pTopicARN pToken =
    ConfirmSubscription'
    { _csAuthenticateOnUnsubscribe = Nothing
    , _csTopicARN = pTopicARN
    , _csToken = pToken
    }

-- | Disallows unauthenticated unsubscribes of the subscription. If the value
-- of this parameter is @true@ and the request has an AWS signature, then
-- only the topic owner and the subscription owner can unsubscribe the
-- endpoint. The unsubscribe action requires AWS authentication.
csAuthenticateOnUnsubscribe :: Lens' ConfirmSubscription (Maybe Text)
csAuthenticateOnUnsubscribe = lens _csAuthenticateOnUnsubscribe (\ s a -> s{_csAuthenticateOnUnsubscribe = a});

-- | The ARN of the topic for which you wish to confirm a subscription.
csTopicARN :: Lens' ConfirmSubscription Text
csTopicARN = lens _csTopicARN (\ s a -> s{_csTopicARN = a});

-- | Short-lived token sent to an endpoint during the @Subscribe@ action.
csToken :: Lens' ConfirmSubscription Text
csToken = lens _csToken (\ s a -> s{_csToken = a});

instance AWSRequest ConfirmSubscription where
        type Sv ConfirmSubscription = SNS
        type Rs ConfirmSubscription =
             ConfirmSubscriptionResponse
        request = post
        response
          = receiveXMLWrapper "ConfirmSubscriptionResult"
              (\ s h x ->
                 ConfirmSubscriptionResponse' <$>
                   (x .@? "SubscriptionArn") <*> (pure s))

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
-- /See:/ 'confirmSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSubscriptionARN'
--
-- * 'csrStatus'
data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse'
    { _csrSubscriptionARN :: !(Maybe Text)
    , _csrStatus          :: !Status
    } deriving (Eq,Show)

-- | 'ConfirmSubscriptionResponse' smart constructor.
confirmSubscriptionResponse :: Status -> ConfirmSubscriptionResponse
confirmSubscriptionResponse pStatus =
    ConfirmSubscriptionResponse'
    { _csrSubscriptionARN = Nothing
    , _csrStatus = pStatus
    }

-- | The ARN of the created subscription.
csrSubscriptionARN :: Lens' ConfirmSubscriptionResponse (Maybe Text)
csrSubscriptionARN = lens _csrSubscriptionARN (\ s a -> s{_csrSubscriptionARN = a});

-- | FIXME: Undocumented member.
csrStatus :: Lens' ConfirmSubscriptionResponse Status
csrStatus = lens _csrStatus (\ s a -> s{_csrStatus = a});
