{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.ListIdentities
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
module Network.AWS.SES.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liIdentityType
    , liNextToken
    , liMaxItems

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , listIdentitiesResponse
    -- ** Response lenses
    , lirIdentities
    , lirNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to list all identities for the
-- AWS Account.
data ListIdentities = ListIdentities
    { _liIdentityType :: Maybe IdentityType
    , _liNextToken :: Maybe Text
    , _liMaxItems :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentities' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityType ::@ @Maybe IdentityType@
--
-- * @NextToken ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Integer@
--
listIdentities :: ListIdentities
listIdentities = ListIdentities
    { _liIdentityType = Nothing
    , _liNextToken = Nothing
    , _liMaxItems = Nothing
    }

-- | The type of the identities to list. Possible values are "EmailAddress" and
-- "Domain". If this parameter is omitted, then all identities will be listed.
liIdentityType :: Lens' ListIdentities (Maybe IdentityType)
liIdentityType = lens _liIdentityType (\s a -> s { _liIdentityType = a })

-- | The token to use for pagination.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s { _liNextToken = a })

-- | The maximum number of identities per page. Possible values are 1-100
-- inclusive.
liMaxItems :: Lens' ListIdentities (Maybe Integer)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })

instance ToQuery ListIdentities where
    toQuery = genericQuery def

-- | Represents a list of all verified identities for the AWS Account.
data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities :: [Text]
    , _lirNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentitiesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Identities ::@ @[Text]@
--
-- * @NextToken ::@ @Maybe Text@
--
listIdentitiesResponse :: [Text] -- ^ 'lirIdentities'
                       -> ListIdentitiesResponse
listIdentitiesResponse p1 = ListIdentitiesResponse
    { _lirIdentities = p1
    , _lirNextToken = Nothing
    }

-- | A list of identities.
lirIdentities :: Lens' ListIdentitiesResponse [Text]
lirIdentities = lens _lirIdentities (\s a -> s { _lirIdentities = a })

-- | The token used for pagination.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\s a -> s { _lirNextToken = a })

instance FromXML ListIdentitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListIdentities where
    type Sv ListIdentities = SES
    type Rs ListIdentities = ListIdentitiesResponse

    request = post "ListIdentities"
    response _ = xmlResponse

instance AWSPager ListIdentities where
    next rq rs = (\x -> rq & liNextToken ?~ x)
        <$> (rs ^. lirNextToken)
