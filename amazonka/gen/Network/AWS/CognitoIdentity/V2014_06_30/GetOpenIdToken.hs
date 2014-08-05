{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.GetOpenIdToken
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.GetOpenIdToken where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetOpenIdToken' request.
getOpenIdToken :: Text -- ^ '_goitiIdentityId'
               -> GetOpenIdToken
getOpenIdToken p1 = GetOpenIdToken
    { _goitiIdentityId = p1
    , _goitiLogins = mempty
    }

data GetOpenIdToken = GetOpenIdToken
    { _goitiIdentityId :: Text
    , _goitiLogins :: HashMap Text Text
    } deriving (Show, Generic)

makeLenses ''GetOpenIdToken

instance ToPath GetOpenIdToken

instance ToQuery GetOpenIdToken

instance ToHeaders GetOpenIdToken

instance ToJSON GetOpenIdToken

data GetOpenIdTokenResponse = GetOpenIdTokenResponse
    { _goitrIdentityId :: Maybe Text
    , _goitrToken :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''GetOpenIdTokenResponse

instance FromJSON GetOpenIdTokenResponse

instance AWSRequest GetOpenIdToken where
    type Sv GetOpenIdToken = CognitoIdentity
    type Rs GetOpenIdToken = GetOpenIdTokenResponse

    request = get
    response _ = jsonResponse
