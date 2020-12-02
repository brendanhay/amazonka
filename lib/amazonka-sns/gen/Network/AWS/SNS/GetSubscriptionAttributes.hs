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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a subscription.
--
--
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
    , gsarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for GetSubscriptionAttributes.
--
--
--
-- /See:/ 'getSubscriptionAttributes' smart constructor.
newtype GetSubscriptionAttributes = GetSubscriptionAttributes'
  { _gsaSubscriptionARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsaSubscriptionARN' - The ARN of the subscription whose properties you want to get.
getSubscriptionAttributes
    :: Text -- ^ 'gsaSubscriptionARN'
    -> GetSubscriptionAttributes
getSubscriptionAttributes pSubscriptionARN_ =
  GetSubscriptionAttributes' {_gsaSubscriptionARN = pSubscriptionARN_}


-- | The ARN of the subscription whose properties you want to get.
gsaSubscriptionARN :: Lens' GetSubscriptionAttributes Text
gsaSubscriptionARN = lens _gsaSubscriptionARN (\ s a -> s{_gsaSubscriptionARN = a})

instance AWSRequest GetSubscriptionAttributes where
        type Rs GetSubscriptionAttributes =
             GetSubscriptionAttributesResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "GetSubscriptionAttributesResult"
              (\ s h x ->
                 GetSubscriptionAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetSubscriptionAttributes where

instance NFData GetSubscriptionAttributes where

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
--
--
-- /See:/ 'getSubscriptionAttributesResponse' smart constructor.
data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse'
  { _gsarsAttributes     :: !(Maybe (Map Text Text))
  , _gsarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSubscriptionAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsarsAttributes' - A map of the subscription's attributes. Attributes in this map include the following:     * @SubscriptionArn@ -- the subscription's ARN     * @TopicArn@ -- the topic ARN that the subscription is associated with     * @Owner@ -- the AWS account ID of the subscription's owner     * @ConfirmationWasAuthenticated@ -- true if the subscription confirmation request was authenticated     * @DeliveryPolicy@ -- the JSON serialization of the subscription's delivery policy     * @EffectiveDeliveryPolicy@ -- the JSON serialization of the effective delivery policy that takes into account the topic delivery policy and account system defaults
--
-- * 'gsarsResponseStatus' - -- | The response status code.
getSubscriptionAttributesResponse
    :: Int -- ^ 'gsarsResponseStatus'
    -> GetSubscriptionAttributesResponse
getSubscriptionAttributesResponse pResponseStatus_ =
  GetSubscriptionAttributesResponse'
    {_gsarsAttributes = Nothing, _gsarsResponseStatus = pResponseStatus_}


-- | A map of the subscription's attributes. Attributes in this map include the following:     * @SubscriptionArn@ -- the subscription's ARN     * @TopicArn@ -- the topic ARN that the subscription is associated with     * @Owner@ -- the AWS account ID of the subscription's owner     * @ConfirmationWasAuthenticated@ -- true if the subscription confirmation request was authenticated     * @DeliveryPolicy@ -- the JSON serialization of the subscription's delivery policy     * @EffectiveDeliveryPolicy@ -- the JSON serialization of the effective delivery policy that takes into account the topic delivery policy and account system defaults
gsarsAttributes :: Lens' GetSubscriptionAttributesResponse (HashMap Text Text)
gsarsAttributes = lens _gsarsAttributes (\ s a -> s{_gsarsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
gsarsResponseStatus :: Lens' GetSubscriptionAttributesResponse Int
gsarsResponseStatus = lens _gsarsResponseStatus (\ s a -> s{_gsarsResponseStatus = a})

instance NFData GetSubscriptionAttributesResponse
         where
