{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.ListIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the identities in a pool. ListIdentities The following are examples
-- of a request and a response for the ListIdentities action. {
-- "IdentityPoolId": "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE",
-- "MaxResults": 10 } { "Identities": [ { "IdentityId":
-- "us-east-1:2345a6b7-8cc3-4a60-8aeb-e11bEXAMPLE4" }, { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3" }, { "IdentityId":
-- "us-east-1:921a3843-2dd6-46b8-ab2f-c679EXAMPLE5" } ], "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE" }.
module Network.AWS.CognitoIdentity
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , mkListIdentities
    -- ** Request lenses
    , liIdentityPoolId
    , liMaxResults
    , liNextToken

    -- * Response
    , ListIdentitiesResponse
    -- ** Response constructor
    , mkListIdentitiesResponse
    -- ** Response lenses
    , lirIdentityPoolId
    , lirIdentities
    , lirNextToken
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the ListIdentities action.
data ListIdentities = ListIdentities
    { _liIdentityPoolId :: !Text
    , _liMaxResults :: !Integer
    , _liNextToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentities' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Text@
--
-- * @MaxResults ::@ @Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
mkListIdentities :: Text -- ^ 'liIdentityPoolId'
                 -> Integer -- ^ 'liMaxResults'
                 -> ListIdentities
mkListIdentities p1 p2 = ListIdentities
    { _liIdentityPoolId = p1
    , _liMaxResults = p2
    , _liNextToken = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
liIdentityPoolId :: Lens' ListIdentities Text
liIdentityPoolId =
    lens _liIdentityPoolId (\s a -> s { _liIdentityPoolId = a })

-- | The maximum number of identities to return.
liMaxResults :: Lens' ListIdentities Integer
liMaxResults = lens _liMaxResults (\s a -> s { _liMaxResults = a })

-- | A pagination token.
liNextToken :: Lens' ListIdentities (Maybe Text)
liNextToken = lens _liNextToken (\s a -> s { _liNextToken = a })

instance ToPath ListIdentities

instance ToQuery ListIdentities

instance ToHeaders ListIdentities

instance ToJSON ListIdentities

-- | The response to a ListIdentities request.
data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentityPoolId :: !(Maybe Text)
    , _lirIdentities :: [IdentityDescription]
    , _lirNextToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentitiesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Maybe Text@
--
-- * @Identities ::@ @[IdentityDescription]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkListIdentitiesResponse :: ListIdentitiesResponse
mkListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentityPoolId = Nothing
    , _lirIdentities = mempty
    , _lirNextToken = Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
lirIdentityPoolId :: Lens' ListIdentitiesResponse (Maybe Text)
lirIdentityPoolId =
    lens _lirIdentityPoolId (\s a -> s { _lirIdentityPoolId = a })

-- | An object containing a set of identities and associated mappings.
lirIdentities :: Lens' ListIdentitiesResponse [IdentityDescription]
lirIdentities = lens _lirIdentities (\s a -> s { _lirIdentities = a })

-- | A pagination token.
lirNextToken :: Lens' ListIdentitiesResponse (Maybe Text)
lirNextToken = lens _lirNextToken (\s a -> s { _lirNextToken = a })

instance FromJSON ListIdentitiesResponse

instance AWSRequest ListIdentities where
    type Sv ListIdentities = CognitoIdentity
    type Rs ListIdentities = ListIdentitiesResponse

    request = get
    response _ = jsonResponse
