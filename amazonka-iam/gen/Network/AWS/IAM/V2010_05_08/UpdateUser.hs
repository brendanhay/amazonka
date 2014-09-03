{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.UpdateUser
    (
    -- * Request
      UpdateUser
    -- ** Request constructor
    , updateUser
    -- ** Request lenses
    , uuvUserName
    , uuvNewPath
    , uuvNewUserName

    -- * Response
    , UpdateUserResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateUser' request.
updateUser :: Text -- ^ 'uuvUserName'
           -> UpdateUser
updateUser p1 = UpdateUser
    { _uuvUserName = p1
    , _uuvNewPath = Nothing
    , _uuvNewUserName = Nothing
    }

data UpdateUser = UpdateUser
    { _uuvUserName :: Text
      -- ^ Name of the user to update. If you're changing the name of the
      -- user, this is the original user name.
    , _uuvNewPath :: Maybe Text
      -- ^ New path for the user. Include this parameter only if you're
      -- changing the user's path.
    , _uuvNewUserName :: Maybe Text
      -- ^ New name for the user. Include this parameter only if you're
      -- changing the user's name.
    } deriving (Show, Generic)

-- | Name of the user to update. If you're changing the name of the user, this
-- is the original user name.
uuvUserName
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateUser
    -> f UpdateUser
uuvUserName f x =
    (\y -> x { _uuvUserName = y })
       <$> f (_uuvUserName x)
{-# INLINE uuvUserName #-}

-- | New path for the user. Include this parameter only if you're changing the
-- user's path.
uuvNewPath
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateUser
    -> f UpdateUser
uuvNewPath f x =
    (\y -> x { _uuvNewPath = y })
       <$> f (_uuvNewPath x)
{-# INLINE uuvNewPath #-}

-- | New name for the user. Include this parameter only if you're changing the
-- user's name.
uuvNewUserName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> UpdateUser
    -> f UpdateUser
uuvNewUserName f x =
    (\y -> x { _uuvNewUserName = y })
       <$> f (_uuvNewUserName x)
{-# INLINE uuvNewUserName #-}

instance ToQuery UpdateUser where
    toQuery = genericQuery def

data UpdateUserResponse = UpdateUserResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateUser where
    type Sv UpdateUser = IAM
    type Rs UpdateUser = UpdateUserResponse

    request = post "UpdateUser"
    response _ = nullaryResponse UpdateUserResponse
