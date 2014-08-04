{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.DeleteIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CognitoIdentity.V2014_06_30.DeleteIdentityPool where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude

data DeleteIdentityPool = DeleteIdentityPool
    { _dipjIdentityPoolId :: Text
    } deriving (Generic)

makeLenses ''DeleteIdentityPool

instance ToPath DeleteIdentityPool

instance ToQuery DeleteIdentityPool

instance ToHeaders DeleteIdentityPool

instance ToJSON DeleteIdentityPool

data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteIdentityPoolResponse

instance AWSRequest DeleteIdentityPool where
    type Sv DeleteIdentityPool = CognitoIdentity
    type Rs DeleteIdentityPool = DeleteIdentityPoolResponse

    request = get
    response _ _ = return (Right DeleteIdentityPoolResponse)
