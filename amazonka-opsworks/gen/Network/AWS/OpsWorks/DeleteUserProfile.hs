{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- more information on user permissions, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html
-- Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteUserProfile.html>
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
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeleteUserProfile = DeleteUserProfile
    { _dupIamUserArn :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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

data DeleteUserProfileResponse = DeleteUserProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteUserProfileResponse' constructor.
deleteUserProfileResponse :: DeleteUserProfileResponse
deleteUserProfileResponse = DeleteUserProfileResponse

instance ToPath DeleteUserProfile where
    toPath = const "/"

instance ToQuery DeleteUserProfile where
    toQuery = const mempty

instance ToHeaders DeleteUserProfile

instance ToJSON DeleteUserProfile where
    toJSON DeleteUserProfile{..} = object
        [ "IamUserArn" .= _dupIamUserArn
        ]

instance AWSRequest DeleteUserProfile where
    type Sv DeleteUserProfile = OpsWorks
    type Rs DeleteUserProfile = DeleteUserProfileResponse

    request  = post "DeleteUserProfile"
    response = nullResponse DeleteUserProfileResponse
