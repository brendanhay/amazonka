{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.CreateUserProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new user profile. Required Permissions: To use this action, an
-- IAM user must have an attached policy that explicitly grants permissions.
-- For more information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.CreateUserProfile
    (
    -- * Request
      CreateUserProfile
    -- ** Request constructor
    , mkCreateUserProfileRequest
    -- ** Request lenses
    , cuprIamUserArn
    , cuprSshUsername
    , cuprSshPublicKey
    , cuprAllowSelfManagement

    -- * Response
    , CreateUserProfileResponse
    -- ** Response lenses
    , cupsIamUserArn
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateUserProfile' request.
mkCreateUserProfileRequest :: Text -- ^ 'cuprIamUserArn'
                           -> CreateUserProfile
mkCreateUserProfileRequest p1 = CreateUserProfile
    { _cuprIamUserArn = p1
    , _cuprSshUsername = Nothing
    , _cuprSshPublicKey = Nothing
    , _cuprAllowSelfManagement = Nothing
    }
{-# INLINE mkCreateUserProfileRequest #-}

data CreateUserProfile = CreateUserProfile
    { _cuprIamUserArn :: Text
      -- ^ The user's IAM ARN.
    , _cuprSshUsername :: Maybe Text
      -- ^ The user's SSH user name. The allowable characters are [a-z],
      -- [A-Z], [0-9], '-', and '_'. If the specified name includes other
      -- punctuation marks, AWS OpsWorks removes them. For example,
      -- my.name will be changed to myname. If you do not specify an SSH
      -- user name, AWS OpsWorks generates one from the IAM user name.
    , _cuprSshPublicKey :: Maybe Text
      -- ^ The user's public SSH key.
    , _cuprAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My
      -- Settings page. For more information, see Setting an IAM User's
      -- Public SSH Key.
    } deriving (Show, Generic)

-- | The user's IAM ARN.
cuprIamUserArn :: Lens' CreateUserProfile (Text)
cuprIamUserArn = lens _cuprIamUserArn (\s a -> s { _cuprIamUserArn = a })
{-# INLINE cuprIamUserArn #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9],
-- '-', and '_'. If the specified name includes other punctuation marks, AWS
-- OpsWorks removes them. For example, my.name will be changed to myname. If
-- you do not specify an SSH user name, AWS OpsWorks generates one from the
-- IAM user name.
cuprSshUsername :: Lens' CreateUserProfile (Maybe Text)
cuprSshUsername = lens _cuprSshUsername (\s a -> s { _cuprSshUsername = a })
{-# INLINE cuprSshUsername #-}

-- | The user's public SSH key.
cuprSshPublicKey :: Lens' CreateUserProfile (Maybe Text)
cuprSshPublicKey = lens _cuprSshPublicKey (\s a -> s { _cuprSshPublicKey = a })
{-# INLINE cuprSshPublicKey #-}

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Setting an IAM User's Public SSH Key.
cuprAllowSelfManagement :: Lens' CreateUserProfile (Maybe Bool)
cuprAllowSelfManagement = lens _cuprAllowSelfManagement (\s a -> s { _cuprAllowSelfManagement = a })
{-# INLINE cuprAllowSelfManagement #-}

instance ToPath CreateUserProfile

instance ToQuery CreateUserProfile

instance ToHeaders CreateUserProfile

instance ToJSON CreateUserProfile

newtype CreateUserProfileResponse = CreateUserProfileResponse
    { _cupsIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    } deriving (Show, Generic)

-- | The user's IAM ARN.
cupsIamUserArn :: Lens' CreateUserProfileResponse (Maybe Text)
cupsIamUserArn = lens _cupsIamUserArn (\s a -> s { _cupsIamUserArn = a })
{-# INLINE cupsIamUserArn #-}

instance FromJSON CreateUserProfileResponse

instance AWSRequest CreateUserProfile where
    type Sv CreateUserProfile = OpsWorks
    type Rs CreateUserProfile = CreateUserProfileResponse

    request = get
    response _ = jsonResponse
