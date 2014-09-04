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
    , mkGetIdInput
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetId' request.
mkGetIdInput :: Text -- ^ 'giiAccountId'
             -> Text -- ^ 'giiIdentityPoolId'
             -> GetId
mkGetIdInput p1 p2 = GetId
    { _giiAccountId = p1
    , _giiIdentityPoolId = p2
    , _giiLogins = mempty
    }
{-# INLINE mkGetIdInput #-}

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
giiAccountId :: Lens' GetId (Text)
giiAccountId = lens _giiAccountId (\s a -> s { _giiAccountId = a })
{-# INLINE giiAccountId #-}

-- | An identity pool ID in the format REGION:GUID.
giiIdentityPoolId :: Lens' GetId (Text)
giiIdentityPoolId = lens _giiIdentityPoolId (\s a -> s { _giiIdentityPoolId = a })
{-# INLINE giiIdentityPoolId #-}

-- | A set of optional name/value pairs that map provider names to provider
-- tokens.
giiLogins :: Lens' GetId (Map Text Text)
giiLogins = lens _giiLogins (\s a -> s { _giiLogins = a })
{-# INLINE giiLogins #-}

instance ToPath GetId

instance ToQuery GetId

instance ToHeaders GetId

instance ToJSON GetId

newtype GetIdResponse = GetIdResponse
    { _girIdentityId :: Maybe Text
      -- ^ A unique identifier in the format REGION:GUID.
    } deriving (Show, Generic)

-- | A unique identifier in the format REGION:GUID.
girIdentityId :: Lens' GetIdResponse (Maybe Text)
girIdentityId = lens _girIdentityId (\s a -> s { _girIdentityId = a })
{-# INLINE girIdentityId #-}

instance FromJSON GetIdResponse

instance AWSRequest GetId where
    type Sv GetId = CognitoIdentity
    type Rs GetId = GetIdResponse

    request = get
    response _ = jsonResponse
