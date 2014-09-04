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
    , getIdentityVerificationAttributes
    -- ** Request lenses
    , givarIdentities

    -- * Response
    , GetIdentityVerificationAttributesResponse
    -- ** Response lenses
    , givasVerificationAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetIdentityVerificationAttributes' request.
getIdentityVerificationAttributes :: [Text] -- ^ 'givarIdentities'
                                  -> GetIdentityVerificationAttributes
getIdentityVerificationAttributes p1 = GetIdentityVerificationAttributes
    { _givarIdentities = p1
    }
{-# INLINE getIdentityVerificationAttributes #-}

data GetIdentityVerificationAttributes = GetIdentityVerificationAttributes
    { _givarIdentities :: [Text]
      -- ^ A list of identities.
    } deriving (Show, Generic)

-- | A list of identities.
givarIdentities :: Lens' GetIdentityVerificationAttributes ([Text])
givarIdentities f x =
    f (_givarIdentities x)
        <&> \y -> x { _givarIdentities = y }
{-# INLINE givarIdentities #-}

instance ToQuery GetIdentityVerificationAttributes where
    toQuery = genericQuery def

data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse
    { _givasVerificationAttributes :: Map Text IdentityVerificationAttributes
      -- ^ A map of Identities to IdentityVerificationAttributes objects.
    } deriving (Show, Generic)

-- | A map of Identities to IdentityVerificationAttributes objects.
givasVerificationAttributes :: Lens' GetIdentityVerificationAttributesResponse (Map Text IdentityVerificationAttributes)
givasVerificationAttributes f x =
    f (_givasVerificationAttributes x)
        <&> \y -> x { _givasVerificationAttributes = y }
{-# INLINE givasVerificationAttributes #-}

instance FromXML GetIdentityVerificationAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetIdentityVerificationAttributes where
    type Sv GetIdentityVerificationAttributes = SES
    type Rs GetIdentityVerificationAttributes = GetIdentityVerificationAttributesResponse

    request = post "GetIdentityVerificationAttributes"
    response _ = xmlResponse
