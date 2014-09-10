{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks
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
module Network.AWS.OpsWorks
    (
    -- * Request
      UpdateMyUserProfile
    -- ** Request constructor
    , mkUpdateMyUserProfile
    -- ** Request lenses
    , umupSshPublicKey

    -- * Response
    , UpdateMyUserProfileResponse
    -- ** Response constructor
    , mkUpdateMyUserProfileResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype UpdateMyUserProfile = UpdateMyUserProfile
    { _umupSshPublicKey :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateMyUserProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SshPublicKey ::@ @Maybe Text@
--
mkUpdateMyUserProfile :: UpdateMyUserProfile
mkUpdateMyUserProfile = UpdateMyUserProfile
    { _umupSshPublicKey = Nothing
    }

-- | The user's SSH public key.
umupSshPublicKey :: Lens' UpdateMyUserProfile (Maybe Text)
umupSshPublicKey =
    lens _umupSshPublicKey (\s a -> s { _umupSshPublicKey = a })

instance ToPath UpdateMyUserProfile

instance ToQuery UpdateMyUserProfile

instance ToHeaders UpdateMyUserProfile

instance ToJSON UpdateMyUserProfile

data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateMyUserProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateMyUserProfileResponse :: UpdateMyUserProfileResponse
mkUpdateMyUserProfileResponse = UpdateMyUserProfileResponse

instance AWSRequest UpdateMyUserProfile where
    type Sv UpdateMyUserProfile = OpsWorks
    type Rs UpdateMyUserProfile = UpdateMyUserProfileResponse

    request = get
    response _ = nullaryResponse UpdateMyUserProfileResponse
