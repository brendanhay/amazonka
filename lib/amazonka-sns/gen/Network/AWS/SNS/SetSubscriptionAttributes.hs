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
-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the topic to a new value.
--
--
module Network.AWS.SNS.SetSubscriptionAttributes
    (
    -- * Creating a Request
      setSubscriptionAttributes
    , SetSubscriptionAttributes
    -- * Request Lenses
    , ssaAttributeValue
    , ssaSubscriptionARN
    , ssaAttributeName

    -- * Destructuring the Response
    , setSubscriptionAttributesResponse
    , SetSubscriptionAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetSubscriptionAttributes action.
--
--
--
-- /See:/ 'setSubscriptionAttributes' smart constructor.
data SetSubscriptionAttributes = SetSubscriptionAttributes'
  { _ssaAttributeValue  :: !(Maybe Text)
  , _ssaSubscriptionARN :: !Text
  , _ssaAttributeName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssaAttributeValue' - The new value for the attribute in JSON format.
--
-- * 'ssaSubscriptionARN' - The ARN of the subscription to modify.
--
-- * 'ssaAttributeName' - The name of the attribute you want to set. Only a subset of the subscriptions attributes are mutable. Valid values: @DeliveryPolicy@ | @RawMessageDelivery@
setSubscriptionAttributes
    :: Text -- ^ 'ssaSubscriptionARN'
    -> Text -- ^ 'ssaAttributeName'
    -> SetSubscriptionAttributes
setSubscriptionAttributes pSubscriptionARN_ pAttributeName_ =
  SetSubscriptionAttributes'
    { _ssaAttributeValue = Nothing
    , _ssaSubscriptionARN = pSubscriptionARN_
    , _ssaAttributeName = pAttributeName_
    }


-- | The new value for the attribute in JSON format.
ssaAttributeValue :: Lens' SetSubscriptionAttributes (Maybe Text)
ssaAttributeValue = lens _ssaAttributeValue (\ s a -> s{_ssaAttributeValue = a})

-- | The ARN of the subscription to modify.
ssaSubscriptionARN :: Lens' SetSubscriptionAttributes Text
ssaSubscriptionARN = lens _ssaSubscriptionARN (\ s a -> s{_ssaSubscriptionARN = a})

-- | The name of the attribute you want to set. Only a subset of the subscriptions attributes are mutable. Valid values: @DeliveryPolicy@ | @RawMessageDelivery@
ssaAttributeName :: Lens' SetSubscriptionAttributes Text
ssaAttributeName = lens _ssaAttributeName (\ s a -> s{_ssaAttributeName = a})

instance AWSRequest SetSubscriptionAttributes where
        type Rs SetSubscriptionAttributes =
             SetSubscriptionAttributesResponse
        request = postQuery sns
        response
          = receiveNull SetSubscriptionAttributesResponse'

instance Hashable SetSubscriptionAttributes where

instance NFData SetSubscriptionAttributes where

instance ToHeaders SetSubscriptionAttributes where
        toHeaders = const mempty

instance ToPath SetSubscriptionAttributes where
        toPath = const "/"

instance ToQuery SetSubscriptionAttributes where
        toQuery SetSubscriptionAttributes'{..}
          = mconcat
              ["Action" =:
                 ("SetSubscriptionAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "AttributeValue" =: _ssaAttributeValue,
               "SubscriptionArn" =: _ssaSubscriptionARN,
               "AttributeName" =: _ssaAttributeName]

-- | /See:/ 'setSubscriptionAttributesResponse' smart constructor.
data SetSubscriptionAttributesResponse =
  SetSubscriptionAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetSubscriptionAttributesResponse' with the minimum fields required to make a request.
--
setSubscriptionAttributesResponse
    :: SetSubscriptionAttributesResponse
setSubscriptionAttributesResponse = SetSubscriptionAttributesResponse'


instance NFData SetSubscriptionAttributesResponse
         where
