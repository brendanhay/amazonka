{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeleteUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a user profile. Required Permissions: To use this action, an IAM
-- user must have an attached policy that explicitly grants permissions. For
-- more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeleteUserProfile
    (
    -- * Request
      DeleteUserProfile
    -- ** Request constructor
    , mkDeleteUserProfile
    -- ** Request lenses
    , dupIamUserArn

    -- * Response
    , DeleteUserProfileResponse
    -- ** Response constructor
    , mkDeleteUserProfileResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeleteUserProfile = DeleteUserProfile
    { _dupIamUserArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUserProfile' request.
mkDeleteUserProfile :: Text -- ^ 'dupIamUserArn'
                    -> DeleteUserProfile
mkDeleteUserProfile p1 = DeleteUserProfile
    { _dupIamUserArn = p1
    }

-- | The user's IAM ARN.
dupIamUserArn :: Lens' DeleteUserProfile Text
dupIamUserArn = lens _dupIamUserArn (\s a -> s { _dupIamUserArn = a })

instance ToPath DeleteUserProfile

instance ToQuery DeleteUserProfile

instance ToHeaders DeleteUserProfile

instance ToJSON DeleteUserProfile

data DeleteUserProfileResponse = DeleteUserProfileResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteUserProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteUserProfileResponse :: DeleteUserProfileResponse
mkDeleteUserProfileResponse = DeleteUserProfileResponse

instance AWSRequest DeleteUserProfile where
    type Sv DeleteUserProfile = OpsWorks
    type Rs DeleteUserProfile = DeleteUserProfileResponse

    request = get
    response _ = nullaryResponse DeleteUserProfileResponse
