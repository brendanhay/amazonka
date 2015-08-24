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
-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a subscription.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_GetSubscriptionAttributes.html AWS API Reference> for GetSubscriptionAttributes.
module Network.AWS.SNS.GetSubscriptionAttributes
    (
    -- * Creating a Request
      getSubscriptionAttributes
    , GetSubscriptionAttributes
    -- * Request Lenses
    , gsaSubscriptionARN

    -- * Destructuring the Response
    , getSubscriptionAttributesResponse
    , GetSubscriptionAttributesResponse
    -- * Response Lenses
    , gsarsAttributes
    , gsarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for GetSubscriptionAttributes.
--
-- /See:/ 'getSubscriptionAttributes' smart constructor.
newtype GetSubscriptionAttributes = GetSubscriptionAttributes'
    { _gsaSubscriptionARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsaSubscriptionARN'
getSubscriptionAttributes
    :: Text -- ^ 'gsaSubscriptionARN'
    -> GetSubscriptionAttributes
getSubscriptionAttributes pSubscriptionARN_ =
    GetSubscriptionAttributes'
    { _gsaSubscriptionARN = pSubscriptionARN_
    }

-- | The ARN of the subscription whose properties you want to get.
gsaSubscriptionARN :: Lens' GetSubscriptionAttributes Text
gsaSubscriptionARN = lens _gsaSubscriptionARN (\ s a -> s{_gsaSubscriptionARN = a});

instance AWSRequest GetSubscriptionAttributes where
        type Rs GetSubscriptionAttributes =
             GetSubscriptionAttributesResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper "GetSubscriptionAttributesResult"
              (\ s h x ->
                 GetSubscriptionAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance ToHeaders GetSubscriptionAttributes where
        toHeaders = const mempty

instance ToPath GetSubscriptionAttributes where
        toPath = const "/"

instance ToQuery GetSubscriptionAttributes where
        toQuery GetSubscriptionAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetSubscriptionAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "SubscriptionArn" =: _gsaSubscriptionARN]

-- | Response for GetSubscriptionAttributes action.
--
-- /See:/ 'getSubscriptionAttributesResponse' smart constructor.
data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse'
    { _gsarsAttributes :: !(Maybe (Map Text Text))
    , _gsarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSubscriptionAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsarsAttributes'
--
-- * 'gsarsStatus'
getSubscriptionAttributesResponse
    :: Int -- ^ 'gsarsStatus'
    -> GetSubscriptionAttributesResponse
getSubscriptionAttributesResponse pStatus_ =
    GetSubscriptionAttributesResponse'
    { _gsarsAttributes = Nothing
    , _gsarsStatus = pStatus_
    }

-- | A map of the subscription\'s attributes. Attributes in this map include
-- the following:
--
-- -   'SubscriptionArn' -- the subscription\'s ARN
-- -   'TopicArn' -- the topic ARN that the subscription is associated with
-- -   'Owner' -- the AWS account ID of the subscription\'s owner
-- -   'ConfirmationWasAuthenticated' -- true if the subscription
--     confirmation request was authenticated
-- -   'DeliveryPolicy' -- the JSON serialization of the subscription\'s
--     delivery policy
-- -   'EffectiveDeliveryPolicy' -- the JSON serialization of the effective
--     delivery policy that takes into account the topic delivery policy
--     and account system defaults
gsarsAttributes :: Lens' GetSubscriptionAttributesResponse (HashMap Text Text)
gsarsAttributes = lens _gsarsAttributes (\ s a -> s{_gsarsAttributes = a}) . _Default . _Map;

-- | The response status code.
gsarsStatus :: Lens' GetSubscriptionAttributesResponse Int
gsarsStatus = lens _gsarsStatus (\ s a -> s{_gsarsStatus = a});
