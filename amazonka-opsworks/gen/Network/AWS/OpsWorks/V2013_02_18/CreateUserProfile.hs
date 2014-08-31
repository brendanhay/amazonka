{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.CreateUserProfile where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'CreateUserProfile' request.
createUserProfile :: Text -- ^ '_cuprIamUserArn'
                  -> CreateUserProfile
createUserProfile p1 = CreateUserProfile
    { _cuprIamUserArn = p1
    , _cuprAllowSelfManagement = Nothing
    , _cuprSshPublicKey = Nothing
    , _cuprSshUsername = Nothing
    }

data CreateUserProfile = CreateUserProfile
    { _cuprIamUserArn :: Text
      -- ^ The user's IAM ARN.
    , _cuprAllowSelfManagement :: Maybe Bool
      -- ^ Whether users can specify their own SSH public key through the My
      -- Settings page. For more information, see Setting an IAM User's
      -- Public SSH Key.
    , _cuprSshPublicKey :: Maybe Text
      -- ^ The user's public SSH key.
    , _cuprSshUsername :: Maybe Text
      -- ^ The user's SSH user name. The allowable characters are [a-z],
      -- [A-Z], [0-9], '-', and '_'. If the specified name includes other
      -- punctuation marks, AWS OpsWorks removes them. For example,
      -- my.name will be changed to myname. If you do not specify an SSH
      -- user name, AWS OpsWorks generates one from the IAM user name.
    } deriving (Show, Generic)

makeLenses ''CreateUserProfile

instance ToPath CreateUserProfile

instance ToQuery CreateUserProfile

instance ToHeaders CreateUserProfile

instance ToJSON CreateUserProfile

data CreateUserProfileResponse = CreateUserProfileResponse
    { _cupsIamUserArn :: Maybe Text
      -- ^ The user's IAM ARN.
    } deriving (Show, Generic)

makeLenses ''CreateUserProfileResponse

instance FromJSON CreateUserProfileResponse

instance AWSRequest CreateUserProfile where
    type Sv CreateUserProfile = OpsWorks
    type Rs CreateUserProfile = CreateUserProfileResponse

    request = get
    response _ = jsonResponse
