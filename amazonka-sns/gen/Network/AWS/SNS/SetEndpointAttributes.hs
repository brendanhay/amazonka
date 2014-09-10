{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the attributes for an endpoint for a device on one of the supported
-- push notification services, such as GCM and APNS. For more information, see
-- Using Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- Attributes.entry.1.key=CustomUserData &amp;Action=SetEndpointAttributes
-- &amp;SignatureMethod=HmacSHA256
-- &amp;Attributes.entry.1.value=My+custom+userdata
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;EndpointArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aendpoint%2FGCM%2Fgcmpushapp%2F5e3e9847-3183-3f18-a7e8-671c3a57d4b3
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=CFTGfGOS5vgSU3%2FZgv2h%2FJdWgr2JQdDJSrUU9k38wSM%3D
-- &amp;Timestamp=2013-07-01T22%3A56%3A45.582Z HTTP/1.1 200 OK ...
-- &lt;SetEndpointAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;2fe0bfc7-3e85-5ee5-a9e2-f58b35e85f6a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SetEndpointAttributesResponse&gt;.
module Network.AWS.SNS
    (
    -- * Request
      SetEndpointAttributes
    -- ** Request constructor
    , mkSetEndpointAttributes
    -- ** Request lenses
    , seaEndpointArn
    , seaAttributes

    -- * Response
    , SetEndpointAttributesResponse
    -- ** Response constructor
    , mkSetEndpointAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for SetEndpointAttributes action.
data SetEndpointAttributes = SetEndpointAttributes
    { _seaEndpointArn :: !Text
    , _seaAttributes :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetEndpointAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EndpointArn ::@ @Text@
--
-- * @Attributes ::@ @Map Text Text@
--
mkSetEndpointAttributes :: Text -- ^ 'seaEndpointArn'
                        -> Map Text Text -- ^ 'seaAttributes'
                        -> SetEndpointAttributes
mkSetEndpointAttributes p1 p2 = SetEndpointAttributes
    { _seaEndpointArn = p1
    , _seaAttributes = p2
    }

-- | EndpointArn used for SetEndpointAttributes action.
seaEndpointArn :: Lens' SetEndpointAttributes Text
seaEndpointArn = lens _seaEndpointArn (\s a -> s { _seaEndpointArn = a })

-- | A map of the endpoint attributes. Attributes in this map include the
-- following: CustomUserData -- arbitrary user data to associate with the
-- endpoint. Amazon SNS does not use this data. The data must be in UTF-8
-- format and less than 2KB. Enabled -- flag that enables/disables delivery to
-- the endpoint. Amazon SNS will set this to false when a notification service
-- indicates to Amazon SNS that the endpoint is invalid. Users can set it back
-- to true, typically after updating Token. Token -- device token, also
-- referred to as a registration id, for an app and mobile device. This is
-- returned from the notification service when an app and mobile device are
-- registered with the notification service.
seaAttributes :: Lens' SetEndpointAttributes (Map Text Text)
seaAttributes = lens _seaAttributes (\s a -> s { _seaAttributes = a })

instance ToQuery SetEndpointAttributes where
    toQuery = genericQuery def

data SetEndpointAttributesResponse = SetEndpointAttributesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetEndpointAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetEndpointAttributesResponse :: SetEndpointAttributesResponse
mkSetEndpointAttributesResponse = SetEndpointAttributesResponse

instance AWSRequest SetEndpointAttributes where
    type Sv SetEndpointAttributes = SNS
    type Rs SetEndpointAttributes = SetEndpointAttributesResponse

    request = post "SetEndpointAttributes"
    response _ = nullaryResponse SetEndpointAttributesResponse
