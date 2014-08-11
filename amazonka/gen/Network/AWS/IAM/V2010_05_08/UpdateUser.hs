{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateUser
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
module Network.AWS.IAM.V2010_05_08.UpdateUser where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateUser' request.
updateUser :: Text -- ^ '_uurUserName'
           -> UpdateUser
updateUser p1 = UpdateUser
    { _uurUserName = p1
    , _uurNewPath = Nothing
    , _uurNewUserName = Nothing
    }

data UpdateUser = UpdateUser
    { _uurUserName :: Text
      -- ^ Name of the user to update. If you're changing the name of the
      -- user, this is the original user name.
    , _uurNewPath :: Maybe Text
      -- ^ New path for the user. Include this parameter only if you're
      -- changing the user's path.
    , _uurNewUserName :: Maybe Text
      -- ^ New name for the user. Include this parameter only if you're
      -- changing the user's name.
    } deriving (Show, Generic)

makeLenses ''UpdateUser

instance ToQuery UpdateUser where
    toQuery = genericToQuery def

data UpdateUserResponse = UpdateUserResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateUserResponse

instance AWSRequest UpdateUser where
    type Sv UpdateUser = IAM
    type Rs UpdateUser = UpdateUserResponse

    request = post "UpdateUser"
    response _ = nullaryResponse UpdateUserResponse
