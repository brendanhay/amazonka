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
-- Module      : Network.AWS.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes for an endpoint for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
module Network.AWS.SNS.SetEndpointAttributes
    (
    -- * Creating a Request
      setEndpointAttributes
    , SetEndpointAttributes
    -- * Request Lenses
    , seaEndpointARN
    , seaAttributes

    -- * Destructuring the Response
    , setEndpointAttributesResponse
    , SetEndpointAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetEndpointAttributes action.
--
--
--
-- /See:/ 'setEndpointAttributes' smart constructor.
data SetEndpointAttributes = SetEndpointAttributes'
  { _seaEndpointARN :: !Text
  , _seaAttributes  :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetEndpointAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seaEndpointARN' - EndpointArn used for SetEndpointAttributes action.
--
-- * 'seaAttributes' - A map of the endpoint attributes. Attributes in this map include the following:     * @CustomUserData@ -- arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.     * @Enabled@ -- flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.     * @Token@ -- device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
setEndpointAttributes
    :: Text -- ^ 'seaEndpointARN'
    -> SetEndpointAttributes
setEndpointAttributes pEndpointARN_ =
  SetEndpointAttributes'
    {_seaEndpointARN = pEndpointARN_, _seaAttributes = mempty}


-- | EndpointArn used for SetEndpointAttributes action.
seaEndpointARN :: Lens' SetEndpointAttributes Text
seaEndpointARN = lens _seaEndpointARN (\ s a -> s{_seaEndpointARN = a})

-- | A map of the endpoint attributes. Attributes in this map include the following:     * @CustomUserData@ -- arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.     * @Enabled@ -- flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.     * @Token@ -- device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
seaAttributes :: Lens' SetEndpointAttributes (HashMap Text Text)
seaAttributes = lens _seaAttributes (\ s a -> s{_seaAttributes = a}) . _Map

instance AWSRequest SetEndpointAttributes where
        type Rs SetEndpointAttributes =
             SetEndpointAttributesResponse
        request = postQuery sns
        response = receiveNull SetEndpointAttributesResponse'

instance Hashable SetEndpointAttributes where

instance NFData SetEndpointAttributes where

instance ToHeaders SetEndpointAttributes where
        toHeaders = const mempty

instance ToPath SetEndpointAttributes where
        toPath = const "/"

instance ToQuery SetEndpointAttributes where
        toQuery SetEndpointAttributes'{..}
          = mconcat
              ["Action" =: ("SetEndpointAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "EndpointArn" =: _seaEndpointARN,
               "Attributes" =:
                 toQueryMap "entry" "key" "value" _seaAttributes]

-- | /See:/ 'setEndpointAttributesResponse' smart constructor.
data SetEndpointAttributesResponse =
  SetEndpointAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetEndpointAttributesResponse' with the minimum fields required to make a request.
--
setEndpointAttributesResponse
    :: SetEndpointAttributesResponse
setEndpointAttributesResponse = SetEndpointAttributesResponse'


instance NFData SetEndpointAttributesResponse where
