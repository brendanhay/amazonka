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
    , listIdentityPools
    -- ** Request lenses
    , lipiMaxResults
    , lipiNextToken

    -- * Response
    , ListIdentityPoolsResponse
    -- ** Response lenses
    , liprIdentityPools
    , liprNextToken
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListIdentityPools' request.
listIdentityPools :: Integer -- ^ 'lipiMaxResults'
                  -> ListIdentityPools
listIdentityPools p1 = ListIdentityPools
    { _lipiMaxResults = p1
    , _lipiNextToken = Nothing
    }

data ListIdentityPools = ListIdentityPools
    { _lipiMaxResults :: Integer
      -- ^ The maximum number of identities to return.
    , _lipiNextToken :: Maybe Text
      -- ^ A pagination token.
    } deriving (Show, Generic)

-- | The maximum number of identities to return.
lipiMaxResults
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ListIdentityPools
    -> f ListIdentityPools
lipiMaxResults f x =
    (\y -> x { _lipiMaxResults = y })
       <$> f (_lipiMaxResults x)
{-# INLINE lipiMaxResults #-}

-- | A pagination token.
lipiNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentityPools
    -> f ListIdentityPools
lipiNextToken f x =
    (\y -> x { _lipiNextToken = y })
       <$> f (_lipiNextToken x)
{-# INLINE lipiNextToken #-}

instance ToPath ListIdentityPools

instance ToQuery ListIdentityPools

instance ToHeaders ListIdentityPools

instance ToJSON ListIdentityPools

data ListIdentityPoolsResponse = ListIdentityPoolsResponse
    { _liprIdentityPools :: [IdentityPoolShortDescription]
      -- ^ The identity pools returned by the ListIdentityPools action.
    , _liprNextToken :: Maybe Text
      -- ^ A pagination token.
    } deriving (Show, Generic)

-- | The identity pools returned by the ListIdentityPools action.
liprIdentityPools
    :: Functor f
    => ([IdentityPoolShortDescription]
    -> f ([IdentityPoolShortDescription]))
    -> ListIdentityPoolsResponse
    -> f ListIdentityPoolsResponse
liprIdentityPools f x =
    (\y -> x { _liprIdentityPools = y })
       <$> f (_liprIdentityPools x)
{-# INLINE liprIdentityPools #-}

-- | A pagination token.
liprNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentityPoolsResponse
    -> f ListIdentityPoolsResponse
liprNextToken f x =
    (\y -> x { _liprNextToken = y })
       <$> f (_liprNextToken x)
{-# INLINE liprNextToken #-}

instance FromJSON ListIdentityPoolsResponse

instance AWSRequest ListIdentityPools where
    type Sv ListIdentityPools = CognitoIdentity
    type Rs ListIdentityPools = ListIdentityPoolsResponse

    request = get
    response _ = jsonResponse
