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

-- Module      : Network.AWS.OpsWorks.DeleteUserProfile
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
module Network.AWS.OpsWorks.DeleteUserProfile
    (
    -- * Request
      DeleteUserProfile
    -- ** Request constructor
    , deleteUserProfile
    -- ** Request lenses
    , dupIamUserArn

    -- * Response
    , DeleteUserProfileResponse
    -- ** Response constructor
    , deleteUserProfileResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype DeleteUserProfile = DeleteUserProfile
    { _dupIamUserArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteUserProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dupIamUserArn' @::@ 'Text'
--
deleteUserProfile :: Text -- ^ 'dupIamUserArn'
                  -> DeleteUserProfile
deleteUserProfile p1 = DeleteUserProfile
    { _dupIamUserArn = p1
    }

-- | The user's IAM ARN.
dupIamUserArn :: Lens' DeleteUserProfile Text
dupIamUserArn = lens _dupIamUserArn (\s a -> s { _dupIamUserArn = a })

instance ToPath DeleteUserProfile where
    toPath = const "/"

instance ToQuery DeleteUserProfile where
    toQuery = const mempty

instance ToHeaders DeleteUserProfile

instance ToBody DeleteUserProfile where
    toBody = toBody . encode . _dupIamUserArn

data DeleteUserProfileResponse = DeleteUserProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteUserProfileResponse' constructor.
deleteUserProfileResponse :: DeleteUserProfileResponse
deleteUserProfileResponse = DeleteUserProfileResponse

-- FromJSON

instance AWSRequest DeleteUserProfile where
    type Sv DeleteUserProfile = OpsWorks
    type Rs DeleteUserProfile = DeleteUserProfileResponse

    request  = post'
    response = nullaryResponse DeleteUserProfileResponse
