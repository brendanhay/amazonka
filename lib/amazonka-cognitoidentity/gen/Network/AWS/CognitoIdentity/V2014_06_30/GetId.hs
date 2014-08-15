{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.CognitoIdentity.V2014_06_30.GetId where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'GetId' request.
getId :: Text -- ^ '_giiAccountId'
      -> Text -- ^ '_giiIdentityPoolId'
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

makeLenses ''GetId

instance ToPath GetId

instance ToQuery GetId

instance ToHeaders GetId

instance ToJSON GetId

data GetIdResponse = GetIdResponse
    { _girIdentityId :: Maybe Text
      -- ^ A unique identifier in the format REGION:GUID.
    } deriving (Show, Generic)

makeLenses ''GetIdResponse

instance FromJSON GetIdResponse

instance AWSRequest GetId where
    type Sv GetId = CognitoIdentity
    type Rs GetId = GetIdResponse

    request = get
    response _ = jsonResponse
