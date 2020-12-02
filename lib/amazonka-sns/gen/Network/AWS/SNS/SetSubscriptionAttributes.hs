{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the subscription to a new value.
module Network.AWS.SNS.SetSubscriptionAttributes
  ( -- * Creating a Request
    setSubscriptionAttributes,
    SetSubscriptionAttributes,

    -- * Request Lenses
    ssaAttributeValue,
    ssaSubscriptionARN,
    ssaAttributeName,

    -- * Destructuring the Response
    setSubscriptionAttributesResponse,
    SetSubscriptionAttributesResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for SetSubscriptionAttributes action.
--
--
--
-- /See:/ 'setSubscriptionAttributes' smart constructor.
data SetSubscriptionAttributes = SetSubscriptionAttributes'
  { _ssaAttributeValue ::
      !(Maybe Text),
    _ssaSubscriptionARN :: !Text,
    _ssaAttributeName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssaAttributeValue' - The new value for the attribute in JSON format.
--
-- * 'ssaSubscriptionARN' - The ARN of the subscription to modify.
--
-- * 'ssaAttributeName' - A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that this action uses:     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.     * @FilterPolicy@ – The simple JSON object that lets your subscriber receive only a subset of messages, rather than receiving every message published to the topic.     * @RawMessageDelivery@ – When set to @true@ , enables raw message delivery to Amazon SQS or HTTP/S endpoints. This eliminates the need for the endpoints to process JSON formatting, which is otherwise created for Amazon SNS metadata.     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
setSubscriptionAttributes ::
  -- | 'ssaSubscriptionARN'
  Text ->
  -- | 'ssaAttributeName'
  Text ->
  SetSubscriptionAttributes
setSubscriptionAttributes pSubscriptionARN_ pAttributeName_ =
  SetSubscriptionAttributes'
    { _ssaAttributeValue = Nothing,
      _ssaSubscriptionARN = pSubscriptionARN_,
      _ssaAttributeName = pAttributeName_
    }

-- | The new value for the attribute in JSON format.
ssaAttributeValue :: Lens' SetSubscriptionAttributes (Maybe Text)
ssaAttributeValue = lens _ssaAttributeValue (\s a -> s {_ssaAttributeValue = a})

-- | The ARN of the subscription to modify.
ssaSubscriptionARN :: Lens' SetSubscriptionAttributes Text
ssaSubscriptionARN = lens _ssaSubscriptionARN (\s a -> s {_ssaSubscriptionARN = a})

-- | A map of attributes with their corresponding values. The following lists the names, descriptions, and values of the special request parameters that this action uses:     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.     * @FilterPolicy@ – The simple JSON object that lets your subscriber receive only a subset of messages, rather than receiving every message published to the topic.     * @RawMessageDelivery@ – When set to @true@ , enables raw message delivery to Amazon SQS or HTTP/S endpoints. This eliminates the need for the endpoints to process JSON formatting, which is otherwise created for Amazon SNS metadata.     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
ssaAttributeName :: Lens' SetSubscriptionAttributes Text
ssaAttributeName = lens _ssaAttributeName (\s a -> s {_ssaAttributeName = a})

instance AWSRequest SetSubscriptionAttributes where
  type
    Rs SetSubscriptionAttributes =
      SetSubscriptionAttributesResponse
  request = postQuery sns
  response = receiveNull SetSubscriptionAttributesResponse'

instance Hashable SetSubscriptionAttributes

instance NFData SetSubscriptionAttributes

instance ToHeaders SetSubscriptionAttributes where
  toHeaders = const mempty

instance ToPath SetSubscriptionAttributes where
  toPath = const "/"

instance ToQuery SetSubscriptionAttributes where
  toQuery SetSubscriptionAttributes' {..} =
    mconcat
      [ "Action" =: ("SetSubscriptionAttributes" :: ByteString),
        "Version" =: ("2010-03-31" :: ByteString),
        "AttributeValue" =: _ssaAttributeValue,
        "SubscriptionArn" =: _ssaSubscriptionARN,
        "AttributeName" =: _ssaAttributeName
      ]

-- | /See:/ 'setSubscriptionAttributesResponse' smart constructor.
data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SetSubscriptionAttributesResponse' with the minimum fields required to make a request.
setSubscriptionAttributesResponse ::
  SetSubscriptionAttributesResponse
setSubscriptionAttributesResponse =
  SetSubscriptionAttributesResponse'

instance NFData SetSubscriptionAttributesResponse
