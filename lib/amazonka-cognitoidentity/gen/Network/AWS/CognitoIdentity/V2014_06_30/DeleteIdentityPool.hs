{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.CognitoIdentity.V2014_06_30.DeleteIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a user pool. Once a pool is deleted, users will not be able to
-- authenticate with the pool. DeleteIdentityPool The following is an example
-- of a DeleteIdentityPool request. { "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE" }.
module Network.AWS.CognitoIdentity.V2014_06_30.DeleteIdentityPool where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.V2014_06_30.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DeleteIdentityPool = DeleteIdentityPool
    { _dipjIdentityPoolId :: Text
      -- ^ An identity pool ID in the format REGION:GUID.
    } deriving (Show, Generic)

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
    response _ = nullaryResponse DeleteIdentityPoolResponse
