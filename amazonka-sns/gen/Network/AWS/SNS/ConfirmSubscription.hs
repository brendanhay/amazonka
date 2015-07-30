{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ConfirmSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Verifies an endpoint owner\'s intent to receive messages by validating
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
    , csrsSubscriptionARN
    , csrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmSubscription' smart constructor.
confirmSubscription :: Text -> Text -> ConfirmSubscription
confirmSubscription pTopicARN_ pToken_ =
    ConfirmSubscription'
    { _csAuthenticateOnUnsubscribe = Nothing
    , _csTopicARN = pTopicARN_
    , _csToken = pToken_
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
        request = postQuery
        response
          = receiveXMLWrapper "ConfirmSubscriptionResult"
              (\ s h x ->
                 ConfirmSubscriptionResponse' <$>
                   (x .@? "SubscriptionArn") <*> (pure (fromEnum s)))

instance ToHeaders ConfirmSubscription where
        toHeaders = const mempty

instance ToPath ConfirmSubscription where
        toPath = const mempty

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
-- * 'csrsSubscriptionARN'
--
-- * 'csrsStatus'
data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse'
    { _csrsSubscriptionARN :: !(Maybe Text)
    , _csrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmSubscriptionResponse' smart constructor.
confirmSubscriptionResponse :: Int -> ConfirmSubscriptionResponse
confirmSubscriptionResponse pStatus_ =
    ConfirmSubscriptionResponse'
    { _csrsSubscriptionARN = Nothing
    , _csrsStatus = pStatus_
    }

-- | The ARN of the created subscription.
csrsSubscriptionARN :: Lens' ConfirmSubscriptionResponse (Maybe Text)
csrsSubscriptionARN = lens _csrsSubscriptionARN (\ s a -> s{_csrsSubscriptionARN = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' ConfirmSubscriptionResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
