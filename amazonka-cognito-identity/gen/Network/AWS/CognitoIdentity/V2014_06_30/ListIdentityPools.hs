{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.ListIdentityPools
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
module Network.AWS.CognitoIdentity.V2014_06_30.ListIdentityPools
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
    -- ** Response lenses
    , liprsIdentityPools
    , liprsNextToken
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Input to the ListIdentityPools action.
data ListIdentityPools = ListIdentityPools
    { _lipMaxResults :: Integer
    , _lipNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListIdentityPools' request.
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
    { _liprsIdentityPools :: [IdentityPoolShortDescription]
    , _liprsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | The identity pools returned by the ListIdentityPools action.
liprsIdentityPools :: Lens' ListIdentityPoolsResponse [IdentityPoolShortDescription]
liprsIdentityPools =
    lens _liprsIdentityPools (\s a -> s { _liprsIdentityPools = a })

-- | A pagination token.
liprsNextToken :: Lens' ListIdentityPoolsResponse (Maybe Text)
liprsNextToken = lens _liprsNextToken (\s a -> s { _liprsNextToken = a })

instance FromJSON ListIdentityPoolsResponse

instance AWSRequest ListIdentityPools where
    type Sv ListIdentityPools = CognitoIdentity
    type Rs ListIdentityPools = ListIdentityPoolsResponse

    request = get
    response _ = jsonResponse
