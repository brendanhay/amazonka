{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.OpsWorks.UpdateMyUserProfile
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
module Network.AWS.OpsWorks.UpdateMyUserProfile
    (
    -- * Request
      UpdateMyUserProfile
    -- ** Request constructor
    , updateMyUserProfile
    -- ** Request lenses
    , umupSshPublicKey

    -- * Response
    , UpdateMyUserProfileResponse
    -- ** Response constructor
    , updateMyUserProfileResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype UpdateMyUserProfile = UpdateMyUserProfile
    { _umupSshPublicKey :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'UpdateMyUserProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'umupSshPublicKey' @::@ 'Maybe' 'Text'
--
updateMyUserProfile :: UpdateMyUserProfile
updateMyUserProfile = UpdateMyUserProfile
    { _umupSshPublicKey = Nothing
    }

-- | The user's SSH public key.
umupSshPublicKey :: Lens' UpdateMyUserProfile (Maybe Text)
umupSshPublicKey = lens _umupSshPublicKey (\s a -> s { _umupSshPublicKey = a })

instance ToPath UpdateMyUserProfile where
    toPath = const "/"

instance ToQuery UpdateMyUserProfile where
    toQuery = const mempty

instance ToHeaders UpdateMyUserProfile

instance ToBody UpdateMyUserProfile where
    toBody = toBody . encode . _umupSshPublicKey

data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateMyUserProfileResponse' constructor.
updateMyUserProfileResponse :: UpdateMyUserProfileResponse
updateMyUserProfileResponse = UpdateMyUserProfileResponse

-- FromJSON

instance AWSRequest UpdateMyUserProfile where
    type Sv UpdateMyUserProfile = OpsWorks
    type Rs UpdateMyUserProfile = UpdateMyUserProfileResponse

    request  = post'
    response = nullaryResponse UpdateMyUserProfileResponse
