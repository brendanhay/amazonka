{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.1.key=EventEndpointCreated&amp;PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=SetPlatformApplicationAttributes
-- &amp;SignatureMethod=HmacSHA256
-- &amp;Attributes.entry.1.value=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Atopicarn
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &amp;SignatureVersion=2
-- &amp;Version=2010-03-31
-- &amp;Signature=06L2TsW3jiH%2FGKDYuT8w4NojSrTf4Ig2GKqGeJPhPT4%3D
-- &amp;Timestamp=2013-07-01T22%3A53%3A17.800Z HTTP/1.1 200 OK ...
-- &lt;SetPlatformApplicationAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;cf577bcc-b3dc-5463-88f1-3180b9412395&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt;
-- &lt;/SetPlatformApplicationAttributesResponse&gt;.
module Network.AWS.SNS.SetPlatformApplicationAttributes
    (
    -- * Request
      SetPlatformApplicationAttributes
    -- ** Request constructor
    , setPlatformApplicationAttributes
    -- ** Request lenses
    , spaaPlatformApplicationArn
    , spaaAttributes

    -- * Response
    , SetPlatformApplicationAttributesResponse
    -- ** Response constructor
    , setPlatformApplicationAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for SetPlatformApplicationAttributes action.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes
    { _spaaPlatformApplicationArn :: Text
    , _spaaAttributes :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetPlatformApplicationAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PlatformApplicationArn ::@ @Text@
--
-- * @Attributes ::@ @Map Text Text@
--
setPlatformApplicationAttributes :: Text -- ^ 'spaaPlatformApplicationArn'
                                 -> Map Text Text -- ^ 'spaaAttributes'
                                 -> SetPlatformApplicationAttributes
setPlatformApplicationAttributes p1 p2 = SetPlatformApplicationAttributes
    { _spaaPlatformApplicationArn = p1
    , _spaaAttributes = p2
    }

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
spaaPlatformApplicationArn :: Lens' SetPlatformApplicationAttributes Text
spaaPlatformApplicationArn =
    lens _spaaPlatformApplicationArn
         (\s a -> s { _spaaPlatformApplicationArn = a })

-- | A map of the platform application attributes. Attributes in this map
-- include the following: PlatformCredential -- The credential received from
-- the notification service. For APNS/APNS_SANDBOX, PlatformCredential is
-- "private key". For GCM, PlatformCredential is "API key". For ADM,
-- PlatformCredential is "client secret". PlatformPrincipal -- The principal
-- received from the notification service. For APNS/APNS_SANDBOX,
-- PlatformPrincipal is "SSL certificate". For GCM, PlatformPrincipal is not
-- applicable. For ADM, PlatformPrincipal is "client id". EventEndpointCreated
-- -- Topic ARN to which EndpointCreated event notifications should be sent.
-- EventEndpointDeleted -- Topic ARN to which EndpointDeleted event
-- notifications should be sent. EventEndpointUpdated -- Topic ARN to which
-- EndpointUpdate event notifications should be sent. EventDeliveryFailure --
-- Topic ARN to which DeliveryFailure event notifications should be sent upon
-- Direct Publish delivery failure (permanent) to one of the application's
-- endpoints.
spaaAttributes :: Lens' SetPlatformApplicationAttributes (Map Text Text)
spaaAttributes = lens _spaaAttributes (\s a -> s { _spaaAttributes = a })

instance ToQuery SetPlatformApplicationAttributes where
    toQuery = genericQuery def

data SetPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetPlatformApplicationAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
setPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse
setPlatformApplicationAttributesResponse = SetPlatformApplicationAttributesResponse

instance AWSRequest SetPlatformApplicationAttributes where
    type Sv SetPlatformApplicationAttributes = SNS
    type Rs SetPlatformApplicationAttributes = SetPlatformApplicationAttributesResponse

    request = post "SetPlatformApplicationAttributes"
    response _ = nullaryResponse SetPlatformApplicationAttributesResponse
