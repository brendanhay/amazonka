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
    , mkListIdentities
    -- ** Request lenses
    , liIdentityType
    , liNextToken
    , liMaxItems

    -- * Response
    , ListIdentitiesResponse
    -- ** Response lenses
    , lirsIdentities
    , lirsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.V2010_12_01.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to list all identities for the
-- AWS Account.
data ListIdentities = ListIdentities
    { _liIdentityType :: Maybe IdentityType
    , _liNextToken :: Maybe Text
    , _liMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentities' request.
mkListIdentities :: ListIdentities
mkListIdentities = ListIdentities
    { _liIdentityType = Nothing
    , _liNextToken = Nothing
    , _liMaxItems = Nothing
    }
{-# INLINE mkListIdentities #-}

-- | The type of the identities to list. Possible values are "EmailAddress" and
-- "Domain". If this parameter is omitted, then all identities will be listed.
liIdentityType :: Lens' ListIdentities (Maybe IdentityType)
liIdentityType = lens _liIdentityType (\s a -> s { _liIdentityType = a })
{-# INLINE liIdentityType #-}

-- | The token to use for pagination.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s { _liNextToken = a })
{-# INLINE liNextToken #-}

-- | The maximum number of identities per page. Possible values are 1-100
-- inclusive.
liMaxItems :: Lens' ListIdentities (Maybe Integer)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })
{-# INLINE liMaxItems #-}

instance ToQuery ListIdentities where
    toQuery = genericQuery def

-- | Represents a list of all verified identities for the AWS Account.
data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirsIdentities :: [Text]
    , _lirsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of identities.
lirsIdentities :: Lens' ListIdentitiesResponse [Text]
lirsIdentities = lens _lirsIdentities (\s a -> s { _lirsIdentities = a })
{-# INLINE lirsIdentities #-}

-- | The token used for pagination.
lirsNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\s a -> s { _lirsNextToken = a })
{-# INLINE lirsNextToken #-}

instance FromXML ListIdentitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListIdentities where
    type Sv ListIdentities = SES
    type Rs ListIdentities = ListIdentitiesResponse

    request = post "ListIdentities"
    response _ = xmlResponse

instance AWSPager ListIdentities where
    next rq rs = (\x -> rq { _liNextToken = Just x })
        <$> (_lirsNextToken rs)
