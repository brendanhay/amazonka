{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateMyUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a user's SSH public key. Required Permissions: To use this action,
-- an IAM user must have self-management enabled or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateMyUserProfile where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateMyUserProfile' request.
updateMyUserProfile :: UpdateMyUserProfile
updateMyUserProfile = UpdateMyUserProfile
    { _umuprSshPublicKey = Nothing
    }

data UpdateMyUserProfile = UpdateMyUserProfile
    { _umuprSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    } deriving (Show, Generic)

makeLenses ''UpdateMyUserProfile

instance ToPath UpdateMyUserProfile

instance ToQuery UpdateMyUserProfile

instance ToHeaders UpdateMyUserProfile

instance ToJSON UpdateMyUserProfile

data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateMyUserProfileResponse

instance AWSRequest UpdateMyUserProfile where
    type Sv UpdateMyUserProfile = OpsWorks
    type Rs UpdateMyUserProfile = UpdateMyUserProfileResponse

    request = get
    response _ = nullaryResponse UpdateMyUserProfileResponse
