{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.UpdateMyUserProfile
    (
    -- * Request
      UpdateMyUserProfile
    -- ** Request constructor
    , mkUpdateMyUserProfileRequest
    -- ** Request lenses
    , umuprSshPublicKey

    -- * Response
    , UpdateMyUserProfileResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateMyUserProfile' request.
mkUpdateMyUserProfileRequest :: UpdateMyUserProfile
mkUpdateMyUserProfileRequest = UpdateMyUserProfile
    { _umuprSshPublicKey = Nothing
    }
{-# INLINE mkUpdateMyUserProfileRequest #-}

newtype UpdateMyUserProfile = UpdateMyUserProfile
    { _umuprSshPublicKey :: Maybe Text
      -- ^ The user's SSH public key.
    } deriving (Show, Generic)

-- | The user's SSH public key.
umuprSshPublicKey :: Lens' UpdateMyUserProfile (Maybe Text)
umuprSshPublicKey = lens _umuprSshPublicKey (\s a -> s { _umuprSshPublicKey = a })
{-# INLINE umuprSshPublicKey #-}

instance ToPath UpdateMyUserProfile

instance ToQuery UpdateMyUserProfile

instance ToHeaders UpdateMyUserProfile

instance ToJSON UpdateMyUserProfile

data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateMyUserProfile where
    type Sv UpdateMyUserProfile = OpsWorks
    type Rs UpdateMyUserProfile = UpdateMyUserProfileResponse

    request = get
    response _ = nullaryResponse UpdateMyUserProfileResponse
