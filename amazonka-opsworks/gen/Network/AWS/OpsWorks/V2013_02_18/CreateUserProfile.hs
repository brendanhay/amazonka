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
    , createUserProfile
    -- ** Request lenses
    , cuprIamUserArn
    , cuprAllowSelfManagement
    , cuprSshUsername
    , cuprSshPublicKey

    -- * Response
    , CreateUserProfileResponse
    -- ** Response lenses
    , cupsIamUserArn
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateUserProfile' request.
createUserProfile :: Text -- ^ 'cuprIamUserArn'
                  -> CreateUserProfile
createUserProfile p1 = CreateUserProfile
    { _cuprIamUserArn = p1
    , _cuprAllowSelfManagement = Nothing
    , _cuprSshUsername = Nothing
    , _cuprSshPublicKey = Nothing
    }

data CreateUserProfile = CreateUserProfile
    { _cuprIamUserArn :: Text
      -- ^ The user's IAM ARN.
    , _cuprAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My
      -- Settings page. For more information, see Setting an IAM User's
      -- Public SSH Key.
    , _cuprSshUsername :: Maybe Text
      -- ^ The user's SSH user name. The allowable characters are [a-z],
      -- [A-Z], [0-9], '-', and '_'. If the specified name includes other
      -- punctuation marks, AWS OpsWorks removes them. For example,
      -- my.name will be changed to myname. If you do not specify an SSH
      -- user name, AWS OpsWorks generates one from the IAM user name.
    , _cuprSshPublicKey :: Maybe Text
      -- ^ The user's public SSH key.
    } deriving (Show, Generic)

-- | The user's IAM ARN.
cuprIamUserArn
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateUserProfile
    -> f CreateUserProfile
cuprIamUserArn f x =
    (\y -> x { _cuprIamUserArn = y })
       <$> f (_cuprIamUserArn x)
{-# INLINE cuprIamUserArn #-}

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Setting an IAM User's Public SSH Key.
cuprAllowSelfManagement
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CreateUserProfile
    -> f CreateUserProfile
cuprAllowSelfManagement f x =
    (\y -> x { _cuprAllowSelfManagement = y })
       <$> f (_cuprAllowSelfManagement x)
{-# INLINE cuprAllowSelfManagement #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9],
-- '-', and '_'. If the specified name includes other punctuation marks, AWS
-- OpsWorks removes them. For example, my.name will be changed to myname. If
-- you do not specify an SSH user name, AWS OpsWorks generates one from the
-- IAM user name.
cuprSshUsername
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateUserProfile
    -> f CreateUserProfile
cuprSshUsername f x =
    (\y -> x { _cuprSshUsername = y })
       <$> f (_cuprSshUsername x)
{-# INLINE cuprSshUsername #-}

-- | The user's public SSH key.
cuprSshPublicKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateUserProfile
    -> f CreateUserProfile
cuprSshPublicKey f x =
    (\y -> x { _cuprSshPublicKey = y })
       <$> f (_cuprSshPublicKey x)
{-# INLINE cuprSshPublicKey #-}

instance ToPath CreateUserProfile

instance ToQuery CreateUserProfile

instance ToHeaders CreateUserProfile

instance ToJSON CreateUserProfile

data CreateUserProfileResponse = CreateUserProfileResponse
    { _cupsIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    } deriving (Show, Generic)

-- | The user's IAM ARN.
cupsIamUserArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CreateUserProfileResponse
    -> f CreateUserProfileResponse
cupsIamUserArn f x =
    (\y -> x { _cupsIamUserArn = y })
       <$> f (_cupsIamUserArn x)
{-# INLINE cupsIamUserArn #-}

instance FromJSON CreateUserProfileResponse

instance AWSRequest CreateUserProfile where
    type Sv CreateUserProfile = OpsWorks
    type Rs CreateUserProfile = CreateUserProfileResponse

    request = get
    response _ = jsonResponse
