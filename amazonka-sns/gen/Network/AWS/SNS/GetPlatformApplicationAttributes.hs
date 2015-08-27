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
-- Module      : Network.AWS.SNS.GetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the attributes of the platform application object for the
-- supported push notification services, such as APNS and GCM. For more
-- information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_GetPlatformApplicationAttributes.html AWS API Reference> for GetPlatformApplicationAttributes.
module Network.AWS.SNS.GetPlatformApplicationAttributes
    (
    -- * Creating a Request
      getPlatformApplicationAttributes
    , GetPlatformApplicationAttributes
    -- * Request Lenses
    , gpaaPlatformApplicationARN

    -- * Destructuring the Response
    , getPlatformApplicationAttributesResponse
    , GetPlatformApplicationAttributesResponse
    -- * Response Lenses
    , gpaarsAttributes
    , gpaarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for GetPlatformApplicationAttributes action.
--
-- /See:/ 'getPlatformApplicationAttributes' smart constructor.
newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes'
    { _gpaaPlatformApplicationARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPlatformApplicationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpaaPlatformApplicationARN'
getPlatformApplicationAttributes
    :: Text -- ^ 'gpaaPlatformApplicationARN'
    -> GetPlatformApplicationAttributes
getPlatformApplicationAttributes pPlatformApplicationARN_ =
    GetPlatformApplicationAttributes'
    { _gpaaPlatformApplicationARN = pPlatformApplicationARN_
    }

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
gpaaPlatformApplicationARN :: Lens' GetPlatformApplicationAttributes Text
gpaaPlatformApplicationARN = lens _gpaaPlatformApplicationARN (\ s a -> s{_gpaaPlatformApplicationARN = a});

instance AWSRequest GetPlatformApplicationAttributes
         where
        type Rs GetPlatformApplicationAttributes =
             GetPlatformApplicationAttributesResponse
        request = postQuery sNS
        response
          = receiveXMLWrapper
              "GetPlatformApplicationAttributesResult"
              (\ s h x ->
                 GetPlatformApplicationAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance ToHeaders GetPlatformApplicationAttributes
         where
        toHeaders = const mempty

instance ToPath GetPlatformApplicationAttributes
         where
        toPath = const "/"

instance ToQuery GetPlatformApplicationAttributes
         where
        toQuery GetPlatformApplicationAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetPlatformApplicationAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "PlatformApplicationArn" =:
                 _gpaaPlatformApplicationARN]

-- | Response for GetPlatformApplicationAttributes action.
--
-- /See:/ 'getPlatformApplicationAttributesResponse' smart constructor.
data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse'
    { _gpaarsAttributes :: !(Maybe (Map Text Text))
    , _gpaarsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetPlatformApplicationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpaarsAttributes'
--
-- * 'gpaarsStatus'
getPlatformApplicationAttributesResponse
    :: Int -- ^ 'gpaarsStatus'
    -> GetPlatformApplicationAttributesResponse
getPlatformApplicationAttributesResponse pStatus_ =
    GetPlatformApplicationAttributesResponse'
    { _gpaarsAttributes = Nothing
    , _gpaarsStatus = pStatus_
    }

-- | Attributes include the following:
--
-- -   'EventEndpointCreated' -- Topic ARN to which EndpointCreated event
--     notifications should be sent.
-- -   'EventEndpointDeleted' -- Topic ARN to which EndpointDeleted event
--     notifications should be sent.
-- -   'EventEndpointUpdated' -- Topic ARN to which EndpointUpdate event
--     notifications should be sent.
-- -   'EventDeliveryFailure' -- Topic ARN to which DeliveryFailure event
--     notifications should be sent upon Direct Publish delivery failure
--     (permanent) to one of the application\'s endpoints.
gpaarsAttributes :: Lens' GetPlatformApplicationAttributesResponse (HashMap Text Text)
gpaarsAttributes = lens _gpaarsAttributes (\ s a -> s{_gpaarsAttributes = a}) . _Default . _Map;

-- | The response status code.
gpaarsStatus :: Lens' GetPlatformApplicationAttributesResponse Int
gpaarsStatus = lens _gpaarsStatus (\ s a -> s{_gpaarsStatus = a});
