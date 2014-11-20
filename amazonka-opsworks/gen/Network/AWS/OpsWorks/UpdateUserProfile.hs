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

-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a specified user profile. Required Permissions: To use this action,
-- an IAM user must have an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateUserProfile.html>
module Network.AWS.OpsWorks.UpdateUserProfile
    (
    -- * Request
      UpdateUserProfile
    -- ** Request constructor
    , updateUserProfile
    -- ** Request lenses
    , uupAllowSelfManagement
    , uupIamUserArn
    , uupSshPublicKey
    , uupSshUsername

    -- * Response
    , UpdateUserProfileResponse
    -- ** Response constructor
    , updateUserProfileResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateUserProfile = UpdateUserProfile
    { _uupAllowSelfManagement :: Maybe Bool
    , _uupIamUserArn          :: Text
    , _uupSshPublicKey        :: Maybe Text
    , _uupSshUsername         :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'UpdateUserProfile' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uupAllowSelfManagement' @::@ 'Maybe' 'Bool'
--
-- * 'uupIamUserArn' @::@ 'Text'
--
-- * 'uupSshPublicKey' @::@ 'Maybe' 'Text'
--
-- * 'uupSshUsername' @::@ 'Maybe' 'Text'
--
updateUserProfile :: Text -- ^ 'uupIamUserArn'
                  -> UpdateUserProfile
updateUserProfile p1 = UpdateUserProfile
    { _uupIamUserArn          = p1
    , _uupSshUsername         = Nothing
    , _uupSshPublicKey        = Nothing
    , _uupAllowSelfManagement = Nothing
    }

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see Managing User Permissions.
uupAllowSelfManagement :: Lens' UpdateUserProfile (Maybe Bool)
uupAllowSelfManagement =
    lens _uupAllowSelfManagement (\s a -> s { _uupAllowSelfManagement = a })

-- | The user IAM ARN.
uupIamUserArn :: Lens' UpdateUserProfile Text
uupIamUserArn = lens _uupIamUserArn (\s a -> s { _uupIamUserArn = a })

-- | The user's new SSH public key.
uupSshPublicKey :: Lens' UpdateUserProfile (Maybe Text)
uupSshPublicKey = lens _uupSshPublicKey (\s a -> s { _uupSshPublicKey = a })

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z],
-- [0-9], '-', and '_'. If the specified name includes other punctuation
-- marks, AWS OpsWorks removes them. For example, my.name will be changed to
-- myname. If you do not specify an SSH user name, AWS OpsWorks generates
-- one from the IAM user name.
uupSshUsername :: Lens' UpdateUserProfile (Maybe Text)
uupSshUsername = lens _uupSshUsername (\s a -> s { _uupSshUsername = a })

data UpdateUserProfileResponse = UpdateUserProfileResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateUserProfileResponse' constructor.
updateUserProfileResponse :: UpdateUserProfileResponse
updateUserProfileResponse = UpdateUserProfileResponse

instance ToPath UpdateUserProfile where
    toPath = const "/"

instance ToQuery UpdateUserProfile where
    toQuery = const mempty

instance ToHeaders UpdateUserProfile

instance ToJSON UpdateUserProfile where
    toJSON UpdateUserProfile{..} = object
        [ "IamUserArn"          .= _uupIamUserArn
        , "SshUsername"         .= _uupSshUsername
        , "SshPublicKey"        .= _uupSshPublicKey
        , "AllowSelfManagement" .= _uupAllowSelfManagement
        ]

instance AWSRequest UpdateUserProfile where
    type Sv UpdateUserProfile = OpsWorks
    type Rs UpdateUserProfile = UpdateUserProfileResponse

    request  = post "UpdateUserProfile"
    response = nullResponse UpdateUserProfileResponse


Some kind of operator / class to check the types whether to continue?
