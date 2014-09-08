{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current status of Easy DKIM signing for an entity. For domain
-- name identities, this action also returns the DKIM tokens that are required
-- for Easy DKIM signing, and whether Amazon SES has successfully verified
-- that these tokens have been published. This action takes a list of
-- identities as input and returns the following information for each: Whether
-- Easy DKIM signing is enabled or disabled. A set of DKIM tokens that
-- represent the identity. If the identity is an email address, the tokens
-- represent the domain of that address. Whether Amazon SES has successfully
-- verified the DKIM tokens published in the domain's DNS. This information is
-- only returned for domain name identities, not for email addresses. This
-- action is throttled at one request per second. For more information about
-- creating DNS records using DKIM tokens, go to the Amazon SES Developer
-- Guide. POST / HTTP/1.1 Date: Fri, 29 Jun 2012 22:41:32 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=MJdhrIAt3c4BRC6jdzueMM+AJLEx17bnIHjZwlSenyk=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 165
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=GetIdentityDkimAttributes
-- &Identities.member.1=example.com &Timestamp=2012-06-29T22%3A41%3A32.000Z
-- &Version=2010-12-01 amazon.com true Success
-- vvjuipp74whm76gqoni7qmwwn4w4qusjiainivf6f
-- 3frqe7jn4obpuxjpwpolz6ipb3k5nvt2nhjpik2oy
-- wrqplteh7oodxnad7hsl4mixg2uavzneazxv5sxi2
-- bb5a105d-c468-11e1-82eb-dff885ccc06a.
module Network.AWS.SES.V2010_12_01.GetIdentityDkimAttributes
    (
    -- * Request
      GetIdentityDkimAttributes
    -- ** Request constructor
    , mkGetIdentityDkimAttributes
    -- ** Request lenses
    , gidaIdentities

    -- * Response
    , GetIdentityDkimAttributesResponse
    -- ** Response constructor
    , mkGetIdentityDkimAttributesResponse
    -- ** Response lenses
    , gidarDkimAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Given a list of verified identities, describes their DKIM attributes. The
-- DKIM attributes of an email address identity includes whether DKIM signing
-- is individually enabled or disabled for that address. The DKIM attributes
-- of a domain name identity includes whether DKIM signing is enabled, as well
-- as the DNS records (tokens) that must remain published in the domain name's
-- DNS.
newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes
    { _gidaIdentities :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetIdentityDkimAttributes' request.
mkGetIdentityDkimAttributes :: [Text] -- ^ 'gidaIdentities'
                            -> GetIdentityDkimAttributes
mkGetIdentityDkimAttributes p1 = GetIdentityDkimAttributes
    { _gidaIdentities = p1
    }

-- | A list of one or more verified identities - email addresses, domains, or
-- both.
gidaIdentities :: Lens' GetIdentityDkimAttributes [Text]
gidaIdentities = lens _gidaIdentities (\s a -> s { _gidaIdentities = a })

instance ToQuery GetIdentityDkimAttributes where
    toQuery = genericQuery def

-- | Represents a list of all the DKIM attributes for the specified identity.
newtype GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse
    { _gidarDkimAttributes :: Map Text IdentityDkimAttributes
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetIdentityDkimAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkGetIdentityDkimAttributesResponse :: Map Text IdentityDkimAttributes -- ^ 'gidarDkimAttributes'
                                    -> GetIdentityDkimAttributesResponse
mkGetIdentityDkimAttributesResponse p1 = GetIdentityDkimAttributesResponse
    { _gidarDkimAttributes = p1
    }

-- | The DKIM attributes for an email address or a domain.
gidarDkimAttributes :: Lens' GetIdentityDkimAttributesResponse (Map Text IdentityDkimAttributes)
gidarDkimAttributes =
    lens _gidarDkimAttributes (\s a -> s { _gidarDkimAttributes = a })

instance FromXML GetIdentityDkimAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetIdentityDkimAttributes where
    type Sv GetIdentityDkimAttributes = SES
    type Rs GetIdentityDkimAttributes = GetIdentityDkimAttributesResponse

    request = post "GetIdentityDkimAttributes"
    response _ = xmlResponse
