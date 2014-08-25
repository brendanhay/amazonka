{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets details about a particular identity pool, including the pool name, ID
-- description, creation date, and current number of users.
-- DescribeIdentityPool The following are an example request and response for
-- the DescribeIdentityPool operation. { "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1" } {
-- "IdentityPoolDescription": "My identity pool", "IdentityPoolId":
-- "us-east-1:af4311ca-835e-4b49-814c-2290EXAMPLE1", "IdentityPoolName":
-- "MyIdentityPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "Unauthenticated": true }.
module Network.AWS.CognitoIdentity.V2014_06_30.DescribeIdentityPool where

import           Network.AWS.CognitoIdentity.V2014_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data DescribeIdentityPool = DescribeIdentityPool
    { _dipiIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    } deriving (Show, Generic)

makeLenses ''DescribeIdentityPool

instance ToPath DescribeIdentityPool

instance ToQuery DescribeIdentityPool

instance ToHeaders DescribeIdentityPool

instance ToJSON DescribeIdentityPool

data DescribeIdentityPoolResponse = DescribeIdentityPoolResponse
    { _ipIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    , _ipIdentityPoolName :: Text
      -- ^ A string that you provide.
    , _ipAllowUnauthenticatedIdentities :: Bool
      -- ^ TRUE if the identity pool supports unauthenticated logins.
    , _ipSupportedLoginProviders :: Map Text Text
      -- ^ Optional key:value pairs mapping provider names to provider app
      -- IDs.
    } deriving (Show, Generic)

makeLenses ''DescribeIdentityPoolResponse

instance FromJSON DescribeIdentityPoolResponse

instance AWSRequest DescribeIdentityPool where
    type Sv DescribeIdentityPool = CognitoIdentity
    type Rs DescribeIdentityPool = DescribeIdentityPoolResponse

    request = get
    response _ = jsonResponse
