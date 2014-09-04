{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateUserProfile
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
module Network.AWS.OpsWorks.V2013_02_18.UpdateUserProfile
    (
    -- * Request
      UpdateUserProfile
    -- ** Request constructor
    , mkUpdateUserProfileRequest
    -- ** Request lenses
    , uuprIamUserArn
    , uuprSshUsername
    , uuprSshPublicKey
    , uuprAllowSelfManagement

    -- * Response
    , UpdateUserProfileResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateUserProfile' request.
mkUpdateUserProfileRequest :: Text -- ^ 'uuprIamUserArn'
                           -> UpdateUserProfile
mkUpdateUserProfileRequest p1 = UpdateUserProfile
    { _uuprIamUserArn = p1
    , _uuprSshUsername = Nothing
    , _uuprSshPublicKey = Nothing
    , _uuprAllowSelfManagement = Nothing
    }
{-# INLINE mkUpdateUserProfileRequest #-}

data UpdateUserProfile = UpdateUserProfile
    { _uuprIamUserArn :: Text
      -- ^ The user IAM ARN.
    , _uuprSshUsername :: Maybe Text
      -- ^ The user's SSH user name. The allowable characters are [a-z],
      -- [A-Z], [0-9], '-', and '_'. If the specified name includes other
      -- punctuation marks, AWS OpsWorks removes them. For example,
      -- my.name will be changed to myname. If you do not specify an SSH
      -- user name, AWS OpsWorks generates one from the IAM user name.
    , _uuprSshPublicKey :: Maybe Text
      -- ^ The user's new SSH public key.
    , _uuprAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My
      -- Settings page. For more information, see Managing User
      -- Permissions.
    } deriving (Show, Generic)

-- | The user IAM ARN.
uuprIamUserArn :: Lens' UpdateUserProfile (Text)
uuprIamUserArn = lens _uuprIamUserArn (\s a -> s { _uuprIamUserArn = a })
{-# INLINE uuprIamUserArn #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9],
-- '-', and '_'. If the specified name includes other punctuation marks, AWS
-- OpsWorks removes them. For example, my.name will be changed to myname. If
-- you do not specify an SSH user name, AWS OpsWorks generates one from the
-- IAM user name.
uuprSshUsername :: Lens' UpdateUserProfile (Maybe Text)
uuprSshUsername = lens _uuprSshUsername (\s a -> s { _uuprSshUsername = a })
{-# INLINE uuprSshUsername #-}

-- | The user's new SSH public key.
uuprSshPublicKey :: Lens' UpdateUserProfile (Maybe Text)
uuprSshPublicKey = lens _uuprSshPublicKey (\s a -> s { _uuprSshPublicKey = a })
{-# INLINE uuprSshPublicKey #-}

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Managing User Permissions.
uuprAllowSelfManagement :: Lens' UpdateUserProfile (Maybe Bool)
uuprAllowSelfManagement = lens _uuprAllowSelfManagement (\s a -> s { _uuprAllowSelfManagement = a })
{-# INLINE uuprAllowSelfManagement #-}

instance ToPath UpdateUserProfile

instance ToQuery UpdateUserProfile

instance ToHeaders UpdateUserProfile

instance ToJSON UpdateUserProfile

data UpdateUserProfileResponse = UpdateUserProfileResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateUserProfile where
    type Sv UpdateUserProfile = OpsWorks
    type Rs UpdateUserProfile = UpdateUserProfileResponse

    request = get
    response _ = nullaryResponse UpdateUserProfileResponse
