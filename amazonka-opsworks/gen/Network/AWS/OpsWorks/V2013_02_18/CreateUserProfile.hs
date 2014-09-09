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
    , mkCreateUserProfile
    -- ** Request lenses
    , cupIamUserArn
    , cupSshUsername
    , cupSshPublicKey
    , cupAllowSelfManagement

    -- * Response
    , CreateUserProfileResponse
    -- ** Response constructor
    , mkCreateUserProfileResponse
    -- ** Response lenses
    , cuprIamUserArn
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data CreateUserProfile = CreateUserProfile
    { _cupIamUserArn :: Text
    , _cupSshUsername :: Maybe Text
    , _cupSshPublicKey :: Maybe Text
    , _cupAllowSelfManagement :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateUserProfile' request.
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
mkCreateUserProfile :: Text -- ^ 'cupIamUserArn'
                    -> CreateUserProfile
mkCreateUserProfile p1 = CreateUserProfile
    { _cupIamUserArn = p1
    , _cupSshUsername = Nothing
    , _cupSshPublicKey = Nothing
    , _cupAllowSelfManagement = Nothing
    }

-- | The user's IAM ARN.
cupIamUserArn :: Lens' CreateUserProfile Text
cupIamUserArn = lens _cupIamUserArn (\s a -> s { _cupIamUserArn = a })

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9],
-- '-', and '_'. If the specified name includes other punctuation marks, AWS
-- OpsWorks removes them. For example, my.name will be changed to myname. If
-- you do not specify an SSH user name, AWS OpsWorks generates one from the
-- IAM user name.
cupSshUsername :: Lens' CreateUserProfile (Maybe Text)
cupSshUsername = lens _cupSshUsername (\s a -> s { _cupSshUsername = a })

-- | The user's public SSH key.
cupSshPublicKey :: Lens' CreateUserProfile (Maybe Text)
cupSshPublicKey = lens _cupSshPublicKey (\s a -> s { _cupSshPublicKey = a })

-- | Whether users can specify their own SSH public key through the My Settings
-- page. For more information, see Setting an IAM User's Public SSH Key.
cupAllowSelfManagement :: Lens' CreateUserProfile (Maybe Bool)
cupAllowSelfManagement =
    lens _cupAllowSelfManagement (\s a -> s { _cupAllowSelfManagement = a })

instance ToPath CreateUserProfile

instance ToQuery CreateUserProfile

instance ToHeaders CreateUserProfile

instance ToJSON CreateUserProfile

-- | Contains the response to a CreateUserProfile request.
newtype CreateUserProfileResponse = CreateUserProfileResponse
    { _cuprIamUserArn :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateUserProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IamUserArn ::@ @Maybe Text@
--
mkCreateUserProfileResponse :: CreateUserProfileResponse
mkCreateUserProfileResponse = CreateUserProfileResponse
    { _cuprIamUserArn = Nothing
    }

-- | The user's IAM ARN.
cuprIamUserArn :: Lens' CreateUserProfileResponse (Maybe Text)
cuprIamUserArn = lens _cuprIamUserArn (\s a -> s { _cuprIamUserArn = a })

instance FromJSON CreateUserProfileResponse

instance AWSRequest CreateUserProfile where
    type Sv CreateUserProfile = OpsWorks
    type Rs CreateUserProfile = CreateUserProfileResponse

    request = get
    response _ = jsonResponse
