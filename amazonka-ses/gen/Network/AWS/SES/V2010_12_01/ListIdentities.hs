{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.V2010_12_01.ListIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list containing all of the identities (email addresses and
-- domains) for a specific AWS Account, regardless of verification status.
-- This action is throttled at one request per second. POST / HTTP/1.1 Date:
-- Sat, 12 May 2012 05:18:45 GMT Host: email.us-east-1.amazonaws.com
-- Content-Type: application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=OruiFNV26DCZicLDaQmULHGbjbU8MbC/c5aIo/MMIuM=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 115
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=ListIdentities
-- &Timestamp=2012-05-12T05%3A18%3A45.000Z& Version=2010-12-01 example.com
-- user@example.com cacecf23-9bf1-11e1-9279-0100e8cf109a.
module Network.AWS.SES.V2010_12_01.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , lirIdentityType
    , lirMaxItems
    , lirNextToken

    -- * Response
    , ListIdentitiesResponse
    -- ** Response lenses
    , lisIdentities
    , lisNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListIdentities' request.
listIdentities :: ListIdentities
listIdentities = ListIdentities
    { _lirIdentityType = Nothing
    , _lirMaxItems = Nothing
    , _lirNextToken = Nothing
    }
{-# INLINE listIdentities #-}

data ListIdentities = ListIdentities
    { _lirIdentityType :: Maybe IdentityType
      -- ^ The type of the identities to list. Possible values are
      -- "EmailAddress" and "Domain". If this parameter is omitted, then
      -- all identities will be listed.
    , _lirMaxItems :: Maybe Integer
      -- ^ The maximum number of identities per page. Possible values are
      -- 1-100 inclusive.
    , _lirNextToken :: Maybe Text
      -- ^ The token to use for pagination.
    } deriving (Show, Generic)

-- | The type of the identities to list. Possible values are "EmailAddress" and
-- "Domain". If this parameter is omitted, then all identities will be listed.
lirIdentityType :: Lens' ListIdentities (Maybe IdentityType)
lirIdentityType f x =
    f (_lirIdentityType x)
        <&> \y -> x { _lirIdentityType = y }
{-# INLINE lirIdentityType #-}

-- | The maximum number of identities per page. Possible values are 1-100
-- inclusive.
lirMaxItems :: Lens' ListIdentities (Maybe Integer)
lirMaxItems f x =
    f (_lirMaxItems x)
        <&> \y -> x { _lirMaxItems = y }
{-# INLINE lirMaxItems #-}

-- | The token to use for pagination.
lirNextToken :: Lens' ListIdentities (Maybe Text)
lirNextToken f x =
    f (_lirNextToken x)
        <&> \y -> x { _lirNextToken = y }
{-# INLINE lirNextToken #-}

instance ToQuery ListIdentities where
    toQuery = genericQuery def

data ListIdentitiesResponse = ListIdentitiesResponse
    { _lisIdentities :: [Text]
      -- ^ A list of identities.
    , _lisNextToken :: Maybe Text
      -- ^ The token used for pagination.
    } deriving (Show, Generic)

-- | A list of identities.
lisIdentities :: Lens' ListIdentitiesResponse ([Text])
lisIdentities f x =
    f (_lisIdentities x)
        <&> \y -> x { _lisIdentities = y }
{-# INLINE lisIdentities #-}

-- | The token used for pagination.
lisNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lisNextToken f x =
    f (_lisNextToken x)
        <&> \y -> x { _lisNextToken = y }
{-# INLINE lisNextToken #-}

instance FromXML ListIdentitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListIdentities where
    type Sv ListIdentities = SES
    type Rs ListIdentities = ListIdentitiesResponse

    request = post "ListIdentities"
    response _ = xmlResponse

instance AWSPager ListIdentities where
    next rq rs = (\x -> rq { _lirNextToken = Just x })
        <$> (_lisNextToken rs)
