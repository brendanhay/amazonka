{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given a list of verified identities (email addresses and/or domains),
-- returns a structure describing identity notification attributes. This
-- action is throttled at one request per second. For more information about
-- using notifications with Amazon SES, see the Amazon SES Developer Guide.
-- POST / HTTP/1.1 Date: Fri, 15 Jun 2012 20:51:42 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=ee9aH6tUW5wBPoh01Tz3w4H+z4avrMmvmRYbfORC7OI=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 173
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=GetIdentityNotificationAttributes
-- &Identities.member.1=user%40example.com
-- &Timestamp=2012-06-15T20%3A51%3A42.000Z &Version=2010-12-01
-- user@example.com true arn:aws:sns:us-east-1:123456789012:example
-- arn:aws:sns:us-east-1:123456789012:example
-- arn:aws:sns:us-east-1:123456789012:example
-- e038e509-b72a-11e1-901f-1fbd90e8104f.
module Network.AWS.SES.V2010_12_01.GetIdentityNotificationAttributes
    (
    -- * Request
      GetIdentityNotificationAttributes
    -- ** Request constructor
    , mkGetIdentityNotificationAttributes
    -- ** Request lenses
    , ginaIdentities

    -- * Response
    , GetIdentityNotificationAttributesResponse
    -- ** Response lenses
    , ginarsNotificationAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Returns the notification attributes for a given list of identities (email
-- address or domain names).
newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes
    { _ginaIdentities :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetIdentityNotificationAttributes' request.
mkGetIdentityNotificationAttributes :: [Text] -- ^ 'ginaIdentities'
                                    -> GetIdentityNotificationAttributes
mkGetIdentityNotificationAttributes p1 = GetIdentityNotificationAttributes
    { _ginaIdentities = p1
    }
{-# INLINE mkGetIdentityNotificationAttributes #-}

-- | A list of one or more identities.
ginaIdentities :: Lens' GetIdentityNotificationAttributes [Text]
ginaIdentities = lens _ginaIdentities (\s a -> s { _ginaIdentities = a })
{-# INLINE ginaIdentities #-}

instance ToQuery GetIdentityNotificationAttributes where
    toQuery = genericQuery def

-- | Describes whether an identity has Amazon Simple Notification Service
-- (Amazon SNS) topics set for bounce, complaint, and/or delivery
-- notifications, and specifies whether feedback forwarding is enabled for
-- bounce and complaint notifications.
newtype GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse
    { _ginarsNotificationAttributes :: Map Text IdentityNotificationAttributes
    } deriving (Show, Generic)

-- | A map of Identity to IdentityNotificationAttributes.
ginarsNotificationAttributes :: Lens' GetIdentityNotificationAttributesResponse (Map Text IdentityNotificationAttributes)
ginarsNotificationAttributes =
    lens _ginarsNotificationAttributes
         (\s a -> s { _ginarsNotificationAttributes = a })
{-# INLINE ginarsNotificationAttributes #-}

instance FromXML GetIdentityNotificationAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetIdentityNotificationAttributes where
    type Sv GetIdentityNotificationAttributes = SES
    type Rs GetIdentityNotificationAttributes = GetIdentityNotificationAttributesResponse

    request = post "GetIdentityNotificationAttributes"
    response _ = xmlResponse
