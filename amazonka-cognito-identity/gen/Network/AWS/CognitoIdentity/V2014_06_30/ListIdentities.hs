{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.ListIdentities
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
module Network.AWS.CognitoIdentity.V2014_06_30.ListIdentities
    (
    -- * Request
      ListIdentities
    -- ** Request constructor
    , listIdentities
    -- ** Request lenses
    , liiIdentityPoolId
    , liiMaxResults
    , liiNextToken

    -- * Response
    , ListIdentitiesResponse
    -- ** Response lenses
    , lirIdentities
    , lirIdentityPoolId
    , lirNextToken
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListIdentities' request.
listIdentities :: Text -- ^ 'liiIdentityPoolId'
               -> Integer -- ^ 'liiMaxResults'
               -> ListIdentities
listIdentities p1 p2 = ListIdentities
    { _liiIdentityPoolId = p1
    , _liiMaxResults = p2
    , _liiNextToken = Nothing
    }

data ListIdentities = ListIdentities
    { _liiIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _liiMaxResults :: Integer
      -- ^ The maximum number of identities to return.
    , _liiNextToken :: Maybe Text
      -- ^ A pagination token.
    } deriving (Show, Generic)

-- | An identity pool ID in the format REGION:GUID.
liiIdentityPoolId
    :: Functor f
    => (Text
    -> f (Text))
    -> ListIdentities
    -> f ListIdentities
liiIdentityPoolId f x =
    (\y -> x { _liiIdentityPoolId = y })
       <$> f (_liiIdentityPoolId x)
{-# INLINE liiIdentityPoolId #-}

-- | The maximum number of identities to return.
liiMaxResults
    :: Functor f
    => (Integer
    -> f (Integer))
    -> ListIdentities
    -> f ListIdentities
liiMaxResults f x =
    (\y -> x { _liiMaxResults = y })
       <$> f (_liiMaxResults x)
{-# INLINE liiMaxResults #-}

-- | A pagination token.
liiNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentities
    -> f ListIdentities
liiNextToken f x =
    (\y -> x { _liiNextToken = y })
       <$> f (_liiNextToken x)
{-# INLINE liiNextToken #-}

instance ToPath ListIdentities

instance ToQuery ListIdentities

instance ToHeaders ListIdentities

instance ToJSON ListIdentities

data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities :: [IdentityDescription]
      -- ^ An object containing a set of identities and associated mappings.
    , _lirIdentityPoolId :: Maybe Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _lirNextToken :: Maybe Text
      -- ^ A pagination token.
    } deriving (Show, Generic)

-- | An object containing a set of identities and associated mappings.
lirIdentities
    :: Functor f
    => ([IdentityDescription]
    -> f ([IdentityDescription]))
    -> ListIdentitiesResponse
    -> f ListIdentitiesResponse
lirIdentities f x =
    (\y -> x { _lirIdentities = y })
       <$> f (_lirIdentities x)
{-# INLINE lirIdentities #-}

-- | An identity pool ID in the format REGION:GUID.
lirIdentityPoolId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentitiesResponse
    -> f ListIdentitiesResponse
lirIdentityPoolId f x =
    (\y -> x { _lirIdentityPoolId = y })
       <$> f (_lirIdentityPoolId x)
{-# INLINE lirIdentityPoolId #-}

-- | A pagination token.
lirNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListIdentitiesResponse
    -> f ListIdentitiesResponse
lirNextToken f x =
    (\y -> x { _lirNextToken = y })
       <$> f (_lirNextToken x)
{-# INLINE lirNextToken #-}

instance FromJSON ListIdentitiesResponse

instance AWSRequest ListIdentities where
    type Sv ListIdentities = CognitoIdentity
    type Rs ListIdentities = ListIdentitiesResponse

    request = get
    response _ = jsonResponse
