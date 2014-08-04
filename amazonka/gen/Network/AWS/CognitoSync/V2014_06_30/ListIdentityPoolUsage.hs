{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoSync.V2014_06_30.ListIdentityPoolUsage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets a list of identity pools registered with Cognito.
module Network.AWS.CognitoSync.V2014_06_30.ListIdentityPoolUsage where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListIdentityPoolUsage' request.
listIdentityPoolUsage :: ListIdentityPoolUsage
listIdentityPoolUsage = ListIdentityPoolUsage
    { _lipurMaxResults = Nothing
    , _lipurNextToken = Nothing
    }

data ListIdentityPoolUsage = ListIdentityPoolUsage
    { _lipurMaxResults :: Maybe Integer
      -- ^ The maximum number of results to be returned.
    , _lipurNextToken :: Maybe Text
      -- ^ A pagination token for obtaining the next page of results.
    } deriving (Generic)

makeLenses ''ListIdentityPoolUsage

instance ToPath ListIdentityPoolUsage where
    toPath = const "/identitypools"

instance ToQuery ListIdentityPoolUsage where
    toQuery ListIdentityPoolUsage{..} = mconcat
        [ "maxResults" =? _lipurMaxResults
        , "nextToken" =? _lipurNextToken
        ]

instance ToHeaders ListIdentityPoolUsage

instance ToJSON ListIdentityPoolUsage

data ListIdentityPoolUsageResponse = ListIdentityPoolUsageResponse
    { _lipusIdentityPoolUsages :: [IdentityPoolUsage]
      -- ^ Usage information for the identity pools.
    , _lipusCount :: Maybe Integer
      -- ^ Total number of identities for the identity pool.
    , _lipusMaxResults :: Maybe Integer
      -- ^ The maximum number of results to be returned.
    , _lipusNextToken :: Maybe Text
      -- ^ A pagination token for obtaining the next page of results.
    } deriving (Generic)

makeLenses ''ListIdentityPoolUsageResponse

instance FromJSON ListIdentityPoolUsageResponse

instance AWSRequest ListIdentityPoolUsage where
    type Sv ListIdentityPoolUsage = CognitoSync
    type Rs ListIdentityPoolUsage = ListIdentityPoolUsageResponse

    request = get
    response _ = jsonResponse
