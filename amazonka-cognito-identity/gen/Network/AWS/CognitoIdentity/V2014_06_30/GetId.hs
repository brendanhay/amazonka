{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.GetId
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates (or retrieves) a Cognito ID. Supplying multiple logins will
-- create an implicit linked account. GetId The following example shows a
-- GetId request for an unauthenticated identity. { "AccountId":
-- "123456789012;", "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1" } { "IdentityId":
-- "us-east-1:852d4250-9eec-4006-8f84-4e82EXAMPLE3" }.
module Network.AWS.CognitoIdentity.V2014_06_30.GetId
    (
    -- * Request
      GetId
    -- ** Request constructor
    , getId
    -- ** Request lenses
    , giiAccountId
    , giiIdentityPoolId
    , giiLogins

    -- * Response
    , GetIdResponse
    -- ** Response lenses
    , girIdentityId
    ) where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'GetId' request.
getId :: Text -- ^ 'giiAccountId'
      -> Text -- ^ 'giiIdentityPoolId'
      -> GetId
getId p1 p2 = GetId
    { _giiAccountId = p1
    , _giiIdentityPoolId = p2
    , _giiLogins = mempty
    }

data GetId = GetId
    { _giiAccountId :: Text
      -- ^ A standard AWS account ID (9+ digits).
    , _giiIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _giiLogins :: Map Text Text
      -- ^ A set of optional name/value pairs that map provider names to
      -- provider tokens.
    } deriving (Show, Generic)

-- | A standard AWS account ID (9+ digits).
giiAccountId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetId
    -> f GetId
giiAccountId f x =
    (\y -> x { _giiAccountId = y })
       <$> f (_giiAccountId x)
{-# INLINE giiAccountId #-}

-- | An identity pool ID in the format REGION:GUID.
giiIdentityPoolId
    :: Functor f
    => (Text
    -> f (Text))
    -> GetId
    -> f GetId
giiIdentityPoolId f x =
    (\y -> x { _giiIdentityPoolId = y })
       <$> f (_giiIdentityPoolId x)
{-# INLINE giiIdentityPoolId #-}

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
giiLogins
    :: Functor f
    => (Map Text Text
    -> f (Map Text Text))
    -> GetId
    -> f GetId
giiLogins f x =
    (\y -> x { _giiLogins = y })
       <$> f (_giiLogins x)
{-# INLINE giiLogins #-}

instance ToPath GetId

instance ToQuery GetId

instance ToHeaders GetId

instance ToJSON GetId

data GetIdResponse = GetIdResponse
    { _girIdentityId :: Maybe Text
      -- ^ A unique identifier in the format REGION:GUID.
    } deriving (Show, Generic)

-- | A unique identifier in the format REGION:GUID.
girIdentityId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> GetIdResponse
    -> f GetIdResponse
girIdentityId f x =
    (\y -> x { _girIdentityId = y })
       <$> f (_girIdentityId x)
{-# INLINE girIdentityId #-}

instance FromJSON GetIdResponse

instance AWSRequest GetId where
    type Sv GetId = CognitoIdentity
    type Rs GetId = GetIdResponse

    request = get
    response _ = jsonResponse
