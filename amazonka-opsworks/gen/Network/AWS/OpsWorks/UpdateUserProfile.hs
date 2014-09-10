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

-- | Updates a specified user profile. Required Permissions: To use this action,
-- an IAM user must have an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks
    (
    -- * Request
      UpdateUserProfile
    -- ** Request constructor
    , mkUpdateUserProfile
    -- ** Request lenses
    , uupIamUserArn
    , uupSshUsername
    , uupSshPublicKey
    , uupAllowSelfManagement

    -- * Response
    , UpdateUserProfileResponse
    -- ** Response constructor
    , mkUpdateUserProfileResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateUserProfile = UpdateUserProfile
    { _uupIamUserArn :: Text
    , _uupSshUsername :: Maybe Text
    , _uupSshPublicKey :: Maybe Text
    , _uupAllowSelfManagement :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateUserProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IamUserArn ::@ @Text@
--
-- * @SshUsername ::@ @Maybe Text@
--
-- * @SshPublicKey ::@ @Maybe Text@
--
-- * @AllowSelfManagement ::@ @Maybe Bool@
--
mkUpdateUserProfile :: Text -- ^ 'uupIamUserArn'
                    -> UpdateUserProfile
mkUpdateUserProfile p1 = UpdateUserProfile
    { _uupIamUserArn = p1
    , _uupSshUsername = Nothing
    , _uupSshPublicKey = Nothing
    , _uupAllowSelfManagement = Nothing
    }

-- | The user IAM ARN.
uupIamUserArn :: Lens' UpdateUserProfile Text
uupIamUserArn = lens _uupIamUserArn (\s a -> s { _uupIamUserArn = a })

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9],
-- '-', and '_'. If the specified name includes other punctuation marks, AWS
-- OpsWorks removes them. For example, my.name will be changed to myname. If
-- you do not specify an SSH user name, AWS OpsWorks generates one from the
-- IAM user name.
uupSshUsername :: Lens' UpdateUserProfile (Maybe Text)
uupSshUsername = lens _uupSshUsername (\s a -> s { _uupSshUsername = a })

-- | The user's new SSH public key.
uupSshPublicKey :: Lens' UpdateUserProfile (Maybe Text)
uupSshPublicKey = lens _uupSshPublicKey (\s a -> s { _uupSshPublicKey = a })

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Managing User Permissions.
uupAllowSelfManagement :: Lens' UpdateUserProfile (Maybe Bool)
uupAllowSelfManagement =
    lens _uupAllowSelfManagement (\s a -> s { _uupAllowSelfManagement = a })

instance ToPath UpdateUserProfile

instance ToQuery UpdateUserProfile

instance ToHeaders UpdateUserProfile

instance ToJSON UpdateUserProfile

data UpdateUserProfileResponse = UpdateUserProfileResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateUserProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateUserProfileResponse :: UpdateUserProfileResponse
mkUpdateUserProfileResponse = UpdateUserProfileResponse

instance AWSRequest UpdateUserProfile where
    type Sv UpdateUserProfile = OpsWorks
    type Rs UpdateUserProfile = UpdateUserProfileResponse

    request = get
    response _ = nullaryResponse UpdateUserProfileResponse
