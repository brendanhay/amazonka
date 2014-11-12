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

-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the attributes of the platform application object for the supported
-- push notification services, such as APNS and GCM. For more information, see
-- Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.SetPlatformApplicationAttributes
    (
    -- * Request
      SetPlatformApplicationAttributesInput
    -- ** Request constructor
    , setPlatformApplicationAttributesInput
    -- ** Request lenses
    , spaaiAttributes
    , spaaiPlatformApplicationArn

    -- * Response
    , SetPlatformApplicationAttributesResponse
    -- ** Response constructor
    , setPlatformApplicationAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data SetPlatformApplicationAttributesInput = SetPlatformApplicationAttributesInput
    { _spaaiAttributes             :: Map Text Text
    , _spaaiPlatformApplicationArn :: Text
    } (Eq, Show, Generic)

-- | 'SetPlatformApplicationAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spaaiAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'spaaiPlatformApplicationArn' @::@ 'Text'
--
setPlatformApplicationAttributesInput :: Text -- ^ 'spaaiPlatformApplicationArn'
                                      -> SetPlatformApplicationAttributesInput
setPlatformApplicationAttributesInput p1 = SetPlatformApplicationAttributesInput
    { _spaaiPlatformApplicationArn = p1
    , _spaaiAttributes             = mempty
    }

-- | A map of the platform application attributes. Attributes in this map
-- include the following: PlatformCredential -- The credential received from
-- the notification service. For APNS/APNS_SANDBOX, PlatformCredential is
-- "private key". For GCM, PlatformCredential is "API key". For ADM,
-- PlatformCredential is "client secret". PlatformPrincipal -- The principal
-- received from the notification service. For APNS/APNS_SANDBOX,
-- PlatformPrincipal is "SSL certificate". For GCM, PlatformPrincipal is not
-- applicable. For ADM, PlatformPrincipal is "client id".
-- EventEndpointCreated -- Topic ARN to which EndpointCreated event
-- notifications should be sent. EventEndpointDeleted -- Topic ARN to which
-- EndpointDeleted event notifications should be sent. EventEndpointUpdated
-- -- Topic ARN to which EndpointUpdate event notifications should be sent.
-- EventDeliveryFailure -- Topic ARN to which DeliveryFailure event
-- notifications should be sent upon Direct Publish delivery failure
-- (permanent) to one of the application's endpoints.
spaaiAttributes :: Lens' SetPlatformApplicationAttributesInput (HashMap Text Text)
spaaiAttributes = lens _spaaiAttributes (\s a -> s { _spaaiAttributes = a })
    . _Map

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
spaaiPlatformApplicationArn :: Lens' SetPlatformApplicationAttributesInput Text
spaaiPlatformApplicationArn =
    lens _spaaiPlatformApplicationArn
        (\s a -> s { _spaaiPlatformApplicationArn = a })
instance ToQuery SetPlatformApplicationAttributesInput

instance ToPath SetPlatformApplicationAttributesInput where
    toPath = const "/"

data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetPlatformApplicationAttributesResponse' constructor.
setPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse
setPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse

instance FromXML SetPlatformApplicationAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetPlatformApplicationAttributesResponse"

instance AWSRequest SetPlatformApplicationAttributesInput where
    type Sv SetPlatformApplicationAttributesInput = SNS
    type Rs SetPlatformApplicationAttributesInput = SetPlatformApplicationAttributesResponse

    request  = post "SetPlatformApplicationAttributes"
    response = nullaryResponse SetPlatformApplicationAttributesResponse
