{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.GetId
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.GetId where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

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
    , _giiIdentityPoolId :: Text
    , _giiLogins :: HashMap Text Text
    } deriving (Show, Generic)

makeLenses ''GetId

instance ToPath GetId

instance ToQuery GetId

instance ToHeaders GetId

instance ToJSON GetId

data GetIdResponse = GetIdResponse
    { _girIdentityId :: Maybe Text
    } deriving (Show, Generic)

makeLenses ''GetIdResponse

instance FromJSON GetIdResponse

instance AWSRequest GetId where
    type Sv GetId = CognitoIdentity
    type Rs GetId = GetIdResponse

    request = get
    response _ = jsonResponse
