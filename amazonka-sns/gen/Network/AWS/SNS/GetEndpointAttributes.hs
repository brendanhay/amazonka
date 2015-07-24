{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the endpoint attributes for a device on one of the supported
-- push notification services, such as GCM and APNS. For more information,
-- see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_GetEndpointAttributes.html>
module Network.AWS.SNS.GetEndpointAttributes
    (
    -- * Request
      GetEndpointAttributes
    -- ** Request constructor
    , getEndpointAttributes
    -- ** Request lenses
    , geaEndpointARN

    -- * Response
    , GetEndpointAttributesResponse
    -- ** Response constructor
    , getEndpointAttributesResponse
    -- ** Response lenses
    , gearsAttributes
    , gearsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for GetEndpointAttributes action.
--
-- /See:/ 'getEndpointAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'geaEndpointARN'
newtype GetEndpointAttributes = GetEndpointAttributes'
    { _geaEndpointARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetEndpointAttributes' smart constructor.
getEndpointAttributes :: Text -> GetEndpointAttributes
getEndpointAttributes pEndpointARN_ =
    GetEndpointAttributes'
    { _geaEndpointARN = pEndpointARN_
    }

-- | EndpointArn for GetEndpointAttributes input.
geaEndpointARN :: Lens' GetEndpointAttributes Text
geaEndpointARN = lens _geaEndpointARN (\ s a -> s{_geaEndpointARN = a});

instance AWSRequest GetEndpointAttributes where
        type Sv GetEndpointAttributes = SNS
        type Rs GetEndpointAttributes =
             GetEndpointAttributesResponse
        request = postQuery
        response
          = receiveXMLWrapper "GetEndpointAttributesResult"
              (\ s h x ->
                 GetEndpointAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

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
-- /See:/ 'getEndpointAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gearsAttributes'
--
-- * 'gearsStatus'
data GetEndpointAttributesResponse = GetEndpointAttributesResponse'
    { _gearsAttributes :: !(Maybe (Map Text Text))
    , _gearsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetEndpointAttributesResponse' smart constructor.
getEndpointAttributesResponse :: Int -> GetEndpointAttributesResponse
getEndpointAttributesResponse pStatus_ =
    GetEndpointAttributesResponse'
    { _gearsAttributes = Nothing
    , _gearsStatus = pStatus_
    }

-- | Attributes include the following:
--
-- -   @CustomUserData@ -- arbitrary user data to associate with the
--     endpoint. Amazon SNS does not use this data. The data must be in
--     UTF-8 format and less than 2KB.
-- -   @Enabled@ -- flag that enables\/disables delivery to the endpoint.
--     Amazon SNS will set this to false when a notification service
--     indicates to Amazon SNS that the endpoint is invalid. Users can set
--     it back to true, typically after updating Token.
-- -   @Token@ -- device token, also referred to as a registration id, for
--     an app and mobile device. This is returned from the notification
--     service when an app and mobile device are registered with the
--     notification service.
gearsAttributes :: Lens' GetEndpointAttributesResponse (HashMap Text Text)
gearsAttributes = lens _gearsAttributes (\ s a -> s{_gearsAttributes = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
gearsStatus :: Lens' GetEndpointAttributesResponse Int
gearsStatus = lens _gearsStatus (\ s a -> s{_gearsStatus = a});
