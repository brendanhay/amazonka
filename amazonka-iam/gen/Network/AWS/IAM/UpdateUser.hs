{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified user. You should
-- understand the implications of changing a user's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide. To
-- change a user name the requester must have appropriate permissions on both
-- the source object and the target object. For example, to change Bob to
-- Robert, the entity making the request must have permission on Bob and
-- Robert, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateUser &UserName=Bob &NewUserName=Robert &Version=2010-05-08
-- &AUTHPARAMS /division_abc/subdivision_xyz/ Robert AIDACKCEVSQ6C2EXAMPLE
-- arn:aws::123456789012:user/division_abc/subdivision_xyz/Robert
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.UpdateUser
    (
    -- * Request
      UpdateUser
    -- ** Request constructor
    , mkUpdateUser
    -- ** Request lenses
    , uuUserName
    , uuNewPath
    , uuNewUserName

    -- * Response
    , UpdateUserResponse
    -- ** Response constructor
    , mkUpdateUserResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateUser = UpdateUser
    { _uuUserName :: !Text
    , _uuNewPath :: !(Maybe Text)
    , _uuNewUserName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateUser' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @NewPath ::@ @Maybe Text@
--
-- * @NewUserName ::@ @Maybe Text@
--
mkUpdateUser :: Text -- ^ 'uuUserName'
             -> UpdateUser
mkUpdateUser p1 = UpdateUser
    { _uuUserName = p1
    , _uuNewPath = Nothing
    , _uuNewUserName = Nothing
    }

-- | Name of the user to update. If you're changing the name of the user, this
-- is the original user name.
uuUserName :: Lens' UpdateUser Text
uuUserName = lens _uuUserName (\s a -> s { _uuUserName = a })

-- | New path for the user. Include this parameter only if you're changing the
-- user's path.
uuNewPath :: Lens' UpdateUser (Maybe Text)
uuNewPath = lens _uuNewPath (\s a -> s { _uuNewPath = a })

-- | New name for the user. Include this parameter only if you're changing the
-- user's name.
uuNewUserName :: Lens' UpdateUser (Maybe Text)
uuNewUserName = lens _uuNewUserName (\s a -> s { _uuNewUserName = a })

instance ToQuery UpdateUser where
    toQuery = genericQuery def

data UpdateUserResponse = UpdateUserResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateUserResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateUserResponse :: UpdateUserResponse
mkUpdateUserResponse = UpdateUserResponse

instance AWSRequest UpdateUser where
    type Sv UpdateUser = IAM
    type Rs UpdateUser = UpdateUserResponse

    request = post "UpdateUser"
    response _ = nullaryResponse UpdateUserResponse
