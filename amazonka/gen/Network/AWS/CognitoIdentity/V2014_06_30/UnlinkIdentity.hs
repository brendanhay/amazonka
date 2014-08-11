{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.UnlinkIdentity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.UnlinkIdentity where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

data UnlinkIdentity = UnlinkIdentity
    { _uiiIdentityId :: Text
    , _uiiLoginsToRemove :: [Text]
    , _uiiLogins :: HashMap Text Text
    } deriving (Show, Generic)

makeLenses ''UnlinkIdentity

instance ToPath UnlinkIdentity

instance ToQuery UnlinkIdentity

instance ToHeaders UnlinkIdentity

instance ToJSON UnlinkIdentity

data UnlinkIdentityResponse = UnlinkIdentityResponse
    deriving (Eq, Show, Generic)

makeLenses ''UnlinkIdentityResponse

instance AWSRequest UnlinkIdentity where
    type Sv UnlinkIdentity = CognitoIdentity
    type Rs UnlinkIdentity = UnlinkIdentityResponse

    request = get
    response _ = nullaryResponse UnlinkIdentityResponse
