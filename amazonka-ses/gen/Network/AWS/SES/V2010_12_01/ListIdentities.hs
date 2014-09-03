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
lirIdentityType
    :: Functor f
    => (Maybe IdentityType
    -> f (Maybe IdentityType))
    -> ListIdentities
    -> f ListIdentities
lirIdentityType f x =
    (\y -> x { _lirIdentityType = y })
       <$> f (_lirIdentityType x)
{-# INLINE lirIdentityType #-}

-- | The maximum number of identities per page. Possible values are 1-100
-- inclusive.
lirMaxItems
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ListIdentities
    -> f ListIdentities
lirMaxItems f x =
    (\y -> x { _lirMaxItems = y })
       <$> f (_lirMaxItems x)
{-# INLINE lirMaxItems #-}

-- | The token to use for pagination.
lirNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentities
    -> f ListIdentities
lirNextToken f x =
    (\y -> x { _lirNextToken = y })
       <$> f (_lirNextToken x)
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
lisIdentities
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ListIdentitiesResponse
    -> f ListIdentitiesResponse
lisIdentities f x =
    (\y -> x { _lisIdentities = y })
       <$> f (_lisIdentities x)
{-# INLINE lisIdentities #-}

-- | The token used for pagination.
lisNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentitiesResponse
    -> f ListIdentitiesResponse
lisNextToken f x =
    (\y -> x { _lisNextToken = y })
       <$> f (_lisNextToken x)
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
