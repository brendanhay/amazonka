{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given a list of identities (email addresses and/or domains), returns the
-- verification status and (for domain identities) the verification token for
-- each identity. This action is throttled at one request per second. POST /
-- HTTP/1.1 Date: Sat, 12 May 2012 05:27:54 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=3+KQ4VHx991T7Kb41HmFcZJxuHz4/6mf2H5FxY+tuLc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 203
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &Action=GetIdentityVerificationAttributes
-- &Identities.member.1=user%40domain.com &Identities.member.2=domain.com
-- &Timestamp=2012-05-12T05%3A27%3A54.000Z &Version=2010-12-01 domain.com
-- Pending QTKknzFg2J4ygwa+XvHAxUl1hyHoY0gVfZdfjIedHZ0= user@domain.com
-- Pending 1d0c29f1-9bf3-11e1-8ee7-c98a0037a2b6.
module Network.AWS.SES.V2010_12_01.GetIdentityVerificationAttributes
    (
    -- * Request
      GetIdentityVerificationAttributes
    -- ** Request constructor
    , mkGetIdentityVerificationAttributes
    -- ** Request lenses
    , givaIdentities

    -- * Response
    , GetIdentityVerificationAttributesResponse
    -- ** Response constructor
    , mkGetIdentityVerificationAttributesResponse
    -- ** Response lenses
    , givarVerificationAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to provide the verification
-- attributes for a list of identities.
newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes
    { _givaIdentities :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetIdentityVerificationAttributes' request.
mkGetIdentityVerificationAttributes :: [Text] -- ^ 'givaIdentities'
                                    -> GetIdentityVerificationAttributes
mkGetIdentityVerificationAttributes p1 = GetIdentityVerificationAttributes
    { _givaIdentities = p1
    }

-- | A list of identities.
givaIdentities :: Lens' GetIdentityVerificationAttributes [Text]
givaIdentities = lens _givaIdentities (\s a -> s { _givaIdentities = a })

instance ToQuery GetIdentityVerificationAttributes where
    toQuery = genericQuery def

-- | Represents the verification attributes for a list of identities.
newtype GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse
    { _givarVerificationAttributes :: Map Text IdentityVerificationAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetIdentityVerificationAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkGetIdentityVerificationAttributesResponse :: Map Text IdentityVerificationAttributes -- ^ 'givarVerificationAttributes'
                                            -> GetIdentityVerificationAttributesResponse
mkGetIdentityVerificationAttributesResponse p1 = GetIdentityVerificationAttributesResponse
    { _givarVerificationAttributes = p1
    }

-- | A map of Identities to IdentityVerificationAttributes objects.
givarVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (Map Text IdentityVerificationAttributes)
givarVerificationAttributes =
    lens _givarVerificationAttributes
         (\s a -> s { _givarVerificationAttributes = a })

instance FromXML GetIdentityVerificationAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetIdentityVerificationAttributes where
    type Sv GetIdentityVerificationAttributes = SES
    type Rs GetIdentityVerificationAttributes = GetIdentityVerificationAttributesResponse

    request = post "GetIdentityVerificationAttributes"
    response _ = xmlResponse
