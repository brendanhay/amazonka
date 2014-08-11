{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.ListIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.ListIdentities where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListIdentities' request.
listIdentities :: Text -- ^ '_liiIdentityPoolId'
               -> ListIdentities
listIdentities p1 = ListIdentities
    { _liiIdentityPoolId = p1
    , _liiNextToken = Nothing
    , _liiMaxResults = Nothing
    }

data ListIdentities = ListIdentities
    { _liiIdentityPoolId :: Text
    , _liiNextToken :: Maybe Text
    , _liiMaxResults :: Maybe Integer
    } deriving (Show, Generic)

makeLenses ''ListIdentities

instance ToPath ListIdentities

instance ToQuery ListIdentities

instance ToHeaders ListIdentities

instance ToJSON ListIdentities

data ListIdentitiesResponse = ListIdentitiesResponse
    { _lirIdentities :: [IdentityDescription]
    , _lirIdentityPoolId :: Maybe Text
    , _lirNextToken :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''ListIdentitiesResponse

instance FromJSON ListIdentitiesResponse

instance AWSRequest ListIdentities where
    type Sv ListIdentities = CognitoIdentity
    type Rs ListIdentities = ListIdentitiesResponse

    request = get
    response _ = jsonResponse
