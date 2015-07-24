{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreatePlatformEndpoint
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a device and mobile app on one of the supported
-- push notification services, such as GCM and APNS.
-- @CreatePlatformEndpoint@ requires the PlatformApplicationArn that is
-- returned from @CreatePlatformApplication@. The EndpointArn that is
-- returned when using @CreatePlatformEndpoint@ can then be used by the
-- @Publish@ action to send a message to a mobile app or by the @Subscribe@
-- action for subscription to a topic. The @CreatePlatformEndpoint@ action
-- is idempotent, so if the requester already owns an endpoint with the
-- same device token and attributes, that endpoint\'s ARN is returned
-- without creating a new endpoint. For more information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- When using @CreatePlatformEndpoint@ with Baidu, two attributes must be
-- provided: ChannelId and UserId. The token field must also contain the
-- ChannelId. For more information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePushBaiduEndpoint.html Creating an Amazon SNS Endpoint for Baidu>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_CreatePlatformEndpoint.html>
module Network.AWS.SNS.CreatePlatformEndpoint
    (
    -- * Request
      CreatePlatformEndpoint
    -- ** Request constructor
    , createPlatformEndpoint
    -- ** Request lenses
    , cpeCustomUserData
    , cpeAttributes
    , cpePlatformApplicationARN
    , cpeToken

    -- * Response
    , CreatePlatformEndpointResponse
    -- ** Response constructor
    , createPlatformEndpointResponse
    -- ** Response lenses
    , cpersEndpointARN
    , cpersStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for CreatePlatformEndpoint action.
--
-- /See:/ 'createPlatformEndpoint' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpeCustomUserData'
--
-- * 'cpeAttributes'
--
-- * 'cpePlatformApplicationARN'
--
-- * 'cpeToken'
data CreatePlatformEndpoint = CreatePlatformEndpoint'
    { _cpeCustomUserData         :: !(Maybe Text)
    , _cpeAttributes             :: !(Maybe (Map Text Text))
    , _cpePlatformApplicationARN :: !Text
    , _cpeToken                  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlatformEndpoint' smart constructor.
createPlatformEndpoint :: Text -> Text -> CreatePlatformEndpoint
createPlatformEndpoint pPlatformApplicationARN_ pToken_ =
    CreatePlatformEndpoint'
    { _cpeCustomUserData = Nothing
    , _cpeAttributes = Nothing
    , _cpePlatformApplicationARN = pPlatformApplicationARN_
    , _cpeToken = pToken_
    }

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not
-- use this data. The data must be in UTF-8 format and less than 2KB.
cpeCustomUserData :: Lens' CreatePlatformEndpoint (Maybe Text)
cpeCustomUserData = lens _cpeCustomUserData (\ s a -> s{_cpeCustomUserData = a});

-- | For a list of attributes, see
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetEndpointAttributes.html SetEndpointAttributes>.
cpeAttributes :: Lens' CreatePlatformEndpoint (HashMap Text Text)
cpeAttributes = lens _cpeAttributes (\ s a -> s{_cpeAttributes = a}) . _Default . _Map;

-- | PlatformApplicationArn returned from CreatePlatformApplication is used
-- to create a an endpoint.
cpePlatformApplicationARN :: Lens' CreatePlatformEndpoint Text
cpePlatformApplicationARN = lens _cpePlatformApplicationARN (\ s a -> s{_cpePlatformApplicationARN = a});

-- | Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when
-- using GCM or ADM, the device token equivalent is called the registration
-- ID.
cpeToken :: Lens' CreatePlatformEndpoint Text
cpeToken = lens _cpeToken (\ s a -> s{_cpeToken = a});

instance AWSRequest CreatePlatformEndpoint where
        type Sv CreatePlatformEndpoint = SNS
        type Rs CreatePlatformEndpoint =
             CreatePlatformEndpointResponse
        request = post "CreatePlatformEndpoint"
        response
          = receiveXMLWrapper "CreatePlatformEndpointResult"
              (\ s h x ->
                 CreatePlatformEndpointResponse' <$>
                   (x .@? "EndpointArn") <*> (pure (fromEnum s)))

instance ToHeaders CreatePlatformEndpoint where
        toHeaders = const mempty

instance ToPath CreatePlatformEndpoint where
        toPath = const "/"

instance ToQuery CreatePlatformEndpoint where
        toQuery CreatePlatformEndpoint'{..}
          = mconcat
              ["Action" =:
                 ("CreatePlatformEndpoint" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "CustomUserData" =: _cpeCustomUserData,
               "Attributes" =:
                 toQuery
                   (toQueryMap "entry" "key" "value" <$>
                      _cpeAttributes),
               "PlatformApplicationArn" =:
                 _cpePlatformApplicationARN,
               "Token" =: _cpeToken]

-- | Response from CreateEndpoint action.
--
-- /See:/ 'createPlatformEndpointResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpersEndpointARN'
--
-- * 'cpersStatus'
data CreatePlatformEndpointResponse = CreatePlatformEndpointResponse'
    { _cpersEndpointARN :: !(Maybe Text)
    , _cpersStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlatformEndpointResponse' smart constructor.
createPlatformEndpointResponse :: Int -> CreatePlatformEndpointResponse
createPlatformEndpointResponse pStatus_ =
    CreatePlatformEndpointResponse'
    { _cpersEndpointARN = Nothing
    , _cpersStatus = pStatus_
    }

-- | EndpointArn returned from CreateEndpoint action.
cpersEndpointARN :: Lens' CreatePlatformEndpointResponse (Maybe Text)
cpersEndpointARN = lens _cpersEndpointARN (\ s a -> s{_cpersEndpointARN = a});

-- | FIXME: Undocumented member.
cpersStatus :: Lens' CreatePlatformEndpointResponse Int
cpersStatus = lens _cpersStatus (\ s a -> s{_cpersStatus = a});
