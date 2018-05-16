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
-- Module      : Network.AWS.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the endpoint attributes for a device on one of the supported push notification services, such as GCM and APNS. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
module Network.AWS.SNS.GetEndpointAttributes
    (
    -- * Creating a Request
      getEndpointAttributes
    , GetEndpointAttributes
    -- * Request Lenses
    , geaEndpointARN

    -- * Destructuring the Response
    , getEndpointAttributesResponse
    , GetEndpointAttributesResponse
    -- * Response Lenses
    , gearsAttributes
    , gearsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for GetEndpointAttributes action.
--
--
--
-- /See:/ 'getEndpointAttributes' smart constructor.
newtype GetEndpointAttributes = GetEndpointAttributes'
  { _geaEndpointARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEndpointAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geaEndpointARN' - EndpointArn for GetEndpointAttributes input.
getEndpointAttributes
    :: Text -- ^ 'geaEndpointARN'
    -> GetEndpointAttributes
getEndpointAttributes pEndpointARN_ =
  GetEndpointAttributes' {_geaEndpointARN = pEndpointARN_}


-- | EndpointArn for GetEndpointAttributes input.
geaEndpointARN :: Lens' GetEndpointAttributes Text
geaEndpointARN = lens _geaEndpointARN (\ s a -> s{_geaEndpointARN = a})

instance AWSRequest GetEndpointAttributes where
        type Rs GetEndpointAttributes =
             GetEndpointAttributesResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "GetEndpointAttributesResult"
              (\ s h x ->
                 GetEndpointAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetEndpointAttributes where

instance NFData GetEndpointAttributes where

instance ToHeaders GetEndpointAttributes where
        toHeaders = const mempty

instance ToPath GetEndpointAttributes where
        toPath = const "/"

instance ToQuery GetEndpointAttributes where
        toQuery GetEndpointAttributes'{..}
          = mconcat
              ["Action" =: ("GetEndpointAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "EndpointArn" =: _geaEndpointARN]

-- | Response from GetEndpointAttributes of the EndpointArn.
--
--
--
-- /See:/ 'getEndpointAttributesResponse' smart constructor.
data GetEndpointAttributesResponse = GetEndpointAttributesResponse'
  { _gearsAttributes     :: !(Maybe (Map Text Text))
  , _gearsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetEndpointAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gearsAttributes' - Attributes include the following:     * @CustomUserData@ -- arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.     * @Enabled@ -- flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.     * @Token@ -- device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
--
-- * 'gearsResponseStatus' - -- | The response status code.
getEndpointAttributesResponse
    :: Int -- ^ 'gearsResponseStatus'
    -> GetEndpointAttributesResponse
getEndpointAttributesResponse pResponseStatus_ =
  GetEndpointAttributesResponse'
    {_gearsAttributes = Nothing, _gearsResponseStatus = pResponseStatus_}


-- | Attributes include the following:     * @CustomUserData@ -- arbitrary user data to associate with the endpoint. Amazon SNS does not use this data. The data must be in UTF-8 format and less than 2KB.     * @Enabled@ -- flag that enables/disables delivery to the endpoint. Amazon SNS will set this to false when a notification service indicates to Amazon SNS that the endpoint is invalid. Users can set it back to true, typically after updating Token.     * @Token@ -- device token, also referred to as a registration id, for an app and mobile device. This is returned from the notification service when an app and mobile device are registered with the notification service.
gearsAttributes :: Lens' GetEndpointAttributesResponse (HashMap Text Text)
gearsAttributes = lens _gearsAttributes (\ s a -> s{_gearsAttributes = a}) . _Default . _Map

-- | -- | The response status code.
gearsResponseStatus :: Lens' GetEndpointAttributesResponse Int
gearsResponseStatus = lens _gearsResponseStatus (\ s a -> s{_gearsResponseStatus = a})

instance NFData GetEndpointAttributesResponse where
