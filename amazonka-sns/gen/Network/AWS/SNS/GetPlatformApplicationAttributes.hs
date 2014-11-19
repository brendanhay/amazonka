{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_GetPlatformApplicationAttributes.html>
module Network.AWS.SNS.GetPlatformApplicationAttributes
    (
    -- * Request
      GetPlatformApplicationAttributes
    -- ** Request constructor
    , getPlatformApplicationAttributes
    -- ** Request lenses
    , gpaaPlatformApplicationArn

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
import qualified GHC.Exts

newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes
    { _gpaaPlatformApplicationArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetPlatformApplicationAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpaaPlatformApplicationArn' @::@ 'Text'
--
getPlatformApplicationAttributes :: Text -- ^ 'gpaaPlatformApplicationArn'
                                 -> GetPlatformApplicationAttributes
getPlatformApplicationAttributes p1 = GetPlatformApplicationAttributes
    { _gpaaPlatformApplicationArn = p1
    }

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
gpaaPlatformApplicationArn :: Lens' GetPlatformApplicationAttributes Text
gpaaPlatformApplicationArn =
    lens _gpaaPlatformApplicationArn
        (\s a -> s { _gpaaPlatformApplicationArn = a })

newtype GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse
    { _gpaarAttributes :: Map "entry" "key" "value" Text Text
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

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
gpaarAttributes = lens _gpaarAttributes (\s a -> s { _gpaarAttributes = a }) . _Map

instance ToPath GetPlatformApplicationAttributes where
    toPath = const "/"

instance ToQuery GetPlatformApplicationAttributes where
    toQuery GetPlatformApplicationAttributes{..} = mconcat
        [ "PlatformApplicationArn" =? _gpaaPlatformApplicationArn
        ]

instance ToHeaders GetPlatformApplicationAttributes

instance AWSRequest GetPlatformApplicationAttributes where
    type Sv GetPlatformApplicationAttributes = SNS
    type Rs GetPlatformApplicationAttributes = GetPlatformApplicationAttributesResponse

    request  = post "GetPlatformApplicationAttributes"
    response = xmlResponse

instance FromXML GetPlatformApplicationAttributesResponse where
    parseXML = withElement "GetPlatformApplicationAttributesResult" $ \x -> GetPlatformApplicationAttributesResponse
        <$> x .@ "Attributes"
