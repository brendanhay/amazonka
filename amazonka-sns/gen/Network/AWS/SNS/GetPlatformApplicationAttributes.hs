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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
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
    , gpaarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for GetPlatformApplicationAttributes action.
--
--
--
-- /See:/ 'getPlatformApplicationAttributes' smart constructor.
newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes'
  { _gpaaPlatformApplicationARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPlatformApplicationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpaaPlatformApplicationARN' - PlatformApplicationArn for GetPlatformApplicationAttributesInput.
getPlatformApplicationAttributes
    :: Text -- ^ 'gpaaPlatformApplicationARN'
    -> GetPlatformApplicationAttributes
getPlatformApplicationAttributes pPlatformApplicationARN_ =
  GetPlatformApplicationAttributes'
    {_gpaaPlatformApplicationARN = pPlatformApplicationARN_}


-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
gpaaPlatformApplicationARN :: Lens' GetPlatformApplicationAttributes Text
gpaaPlatformApplicationARN = lens _gpaaPlatformApplicationARN (\ s a -> s{_gpaaPlatformApplicationARN = a})

instance AWSRequest GetPlatformApplicationAttributes
         where
        type Rs GetPlatformApplicationAttributes =
             GetPlatformApplicationAttributesResponse
        request = postQuery sns
        response
          = receiveXMLWrapper
              "GetPlatformApplicationAttributesResult"
              (\ s h x ->
                 GetPlatformApplicationAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetPlatformApplicationAttributes
         where

instance NFData GetPlatformApplicationAttributes
         where

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
--
--
-- /See:/ 'getPlatformApplicationAttributesResponse' smart constructor.
data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse'
  { _gpaarsAttributes     :: !(Maybe (Map Text Text))
  , _gpaarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPlatformApplicationAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpaarsAttributes' - Attributes include the following:     * @EventEndpointCreated@
