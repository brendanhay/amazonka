{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SNS.GetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the attributes of the platform application object for the
-- supported push notification services, such as APNS and GCM. For more
-- information, see Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.GetPlatformApplicationAttributes
    (
    -- * Request
      GetPlatformApplicationAttributesInput
    -- ** Request constructor
    , getPlatformApplicationAttributesInput
    -- ** Request lenses
    , gpaaiPlatformApplicationArn

    -- * Response
    , GetPlatformApplicationAttributesResponse
    -- ** Response constructor
    , getPlatformApplicationAttributesResponse
    -- ** Response lenses
    , gpaarAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype GetPlatformApplicationAttributesInput = GetPlatformApplicationAttributesInput
    { _gpaaiPlatformApplicationArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetPlatformApplicationAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpaaiPlatformApplicationArn' @::@ 'Text'
--
getPlatformApplicationAttributesInput :: Text -- ^ 'gpaaiPlatformApplicationArn'
                                      -> GetPlatformApplicationAttributesInput
getPlatformApplicationAttributesInput p1 = GetPlatformApplicationAttributesInput
    { _gpaaiPlatformApplicationArn = p1
    }

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
gpaaiPlatformApplicationArn :: Lens' GetPlatformApplicationAttributesInput Text
gpaaiPlatformApplicationArn =
    lens _gpaaiPlatformApplicationArn
        (\s a -> s { _gpaaiPlatformApplicationArn = a })

instance ToQuery GetPlatformApplicationAttributesInput

instance ToPath GetPlatformApplicationAttributesInput where
    toPath = const "/"

newtype GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse
    { _gpaarAttributes :: Map Text Text
    } deriving (Eq, Show, Generic, Monoid, Semigroup, IsString)

instance IsList GetPlatformApplicationAttributesResponse
    type Item GetPlatformApplicationAttributesResponse = (Text, Text)

    fromList = GetPlatformApplicationAttributesResponse . fromList
    toList   = toList . _gpaarAttributes

-- | 'GetPlatformApplicationAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpaarAttributes' @::@ 'HashMap' 'Text' 'Text'
--
getPlatformApplicationAttributesResponse :: GetPlatformApplicationAttributesResponse
getPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse
    { _gpaarAttributes = mempty
    }

-- | Attributes include the following: EventEndpointCreated -- Topic ARN to
-- which EndpointCreated event notifications should be sent.
-- EventEndpointDeleted -- Topic ARN to which EndpointDeleted event
-- notifications should be sent. EventEndpointUpdated -- Topic ARN to which
-- EndpointUpdate event notifications should be sent. EventDeliveryFailure
-- -- Topic ARN to which DeliveryFailure event notifications should be sent
-- upon Direct Publish delivery failure (permanent) to one of the
-- application's endpoints.
gpaarAttributes :: Lens' GetPlatformApplicationAttributesResponse (HashMap Text Text)
gpaarAttributes = lens _gpaarAttributes (\s a -> s { _gpaarAttributes = a })
    . _Map

instance FromXML GetPlatformApplicationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetPlatformApplicationAttributesResponse"

instance AWSRequest GetPlatformApplicationAttributesInput where
    type Sv GetPlatformApplicationAttributesInput = SNS
    type Rs GetPlatformApplicationAttributesInput = GetPlatformApplicationAttributesResponse

    request  = post "GetPlatformApplicationAttributes"
    response = xmlResponse $ \h x -> GetPlatformApplicationAttributesResponse
        <$> x %| "Attributes"
