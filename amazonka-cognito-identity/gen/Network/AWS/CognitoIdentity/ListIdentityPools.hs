{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.ListIdentityPools
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all of the Cognito identity pools registered for your account.
-- ListIdentityPools The following example shows a request and a response for
-- a ListIdentityPools operation. { "MaxResults": 10 } { "IdentityPools": [ {
-- "IdentityPoolId": "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1",
-- "IdentityPoolName": "MyPool" }, { "IdentityPoolId":
-- "us-east-1:f212b602-a526-4557-af13-8eedEXAMPLE2", "IdentityPoolName":
-- "MyPool2" } ] }.
module Network.AWS.CognitoIdentity.ListIdentityPools
    (
    -- * Request
      ListIdentityPools
    -- ** Request constructor
    , mkListIdentityPools
    -- ** Request lenses
    , lipMaxResults
    , lipNextToken

    -- * Response
    , ListIdentityPoolsResponse
    -- ** Response constructor
    , mkListIdentityPoolsResponse
    -- ** Response lenses
    , liprIdentityPools
    , liprNextToken
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Input to the ListIdentityPools action.
data ListIdentityPools = ListIdentityPools
    { _lipMaxResults :: !Integer
    , _lipNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentityPools' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MaxResults ::@ @Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
mkListIdentityPools :: Integer -- ^ 'lipMaxResults'
                    -> ListIdentityPools
mkListIdentityPools p1 = ListIdentityPools
    { _lipMaxResults = p1
    , _lipNextToken = Nothing
    }

-- | The maximum number of identities to return.
lipMaxResults :: Lens' ListIdentityPools Integer
lipMaxResults = lens _lipMaxResults (\s a -> s { _lipMaxResults = a })

-- | A pagination token.
lipNextToken :: Lens' ListIdentityPools (Maybe Text)
lipNextToken = lens _lipNextToken (\s a -> s { _lipNextToken = a })

instance ToPath ListIdentityPools

instance ToQuery ListIdentityPools

instance ToHeaders ListIdentityPools

instance ToJSON ListIdentityPools

-- | The result of a successful ListIdentityPools action.
data ListIdentityPoolsResponse = ListIdentityPoolsResponse
    { _liprIdentityPools :: [IdentityPoolShortDescription]
    , _liprNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentityPoolsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPools ::@ @[IdentityPoolShortDescription]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkListIdentityPoolsResponse :: ListIdentityPoolsResponse
mkListIdentityPoolsResponse = ListIdentityPoolsResponse
    { _liprIdentityPools = mempty
    , _liprNextToken = Nothing
    }

-- | The identity pools returned by the ListIdentityPools action.
liprIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprIdentityPools =
    lens _liprIdentityPools (\s a -> s { _liprIdentityPools = a })

-- | A pagination token.
liprNextToken :: Lens' ListIdentityPoolsResponse (Maybe Text)
liprNextToken = lens _liprNextToken (\s a -> s { _liprNextToken = a })

instance FromJSON ListIdentityPoolsResponse

instance AWSRequest ListIdentityPools where
    type Sv ListIdentityPools = CognitoIdentity
    type Rs ListIdentityPools = ListIdentityPoolsResponse

    request = get
    response _ = jsonResponse
