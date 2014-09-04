{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the name and/or the path of the specified group. You should
-- understand the implications of changing a group's path or name. For more
-- information, see Renaming Users and Groups in the Using IAM guide. To
-- change a group name the requester must have appropriate permissions on both
-- the source object and the target object. For example, to change Managers to
-- MGRs, the entity making the request must have permission on Managers and
-- MGRs, or must have permission on all (*). For more information about
-- permissions, see Permissions and Policies. https://iam.amazonaws.com/
-- ?Action=UpdateGroup &GroupName=Test &NewGroupName=Test_1
-- &Version=2010-05-08 &AUTHPARAMS
-- /division_abc/subdivision_xyz/product_1234/engineering/ Test_1
-- AGP2MAB8DPLSRHEXAMPLE
-- arn:aws:iam::123456789012:group/division_abc/subdivision_xyz/
-- product_1234/engineering/Test_1 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UpdateGroup
    (
    -- * Request
      UpdateGroup
    -- ** Request constructor
    , mkUpdateGroupRequest
    -- ** Request lenses
    , ugrGroupName
    , ugrNewPath
    , ugrNewGroupName

    -- * Response
    , UpdateGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateGroup' request.
mkUpdateGroupRequest :: Text -- ^ 'ugrGroupName'
                     -> UpdateGroup
mkUpdateGroupRequest p1 = UpdateGroup
    { _ugrGroupName = p1
    , _ugrNewPath = Nothing
    , _ugrNewGroupName = Nothing
    }
{-# INLINE mkUpdateGroupRequest #-}

data UpdateGroup = UpdateGroup
    { _ugrGroupName :: Text
      -- ^ Name of the group to update. If you're changing the name of the
      -- group, this is the original name.
    , _ugrNewPath :: Maybe Text
      -- ^ New path for the group. Only include this if changing the group's
      -- path.
    , _ugrNewGroupName :: Maybe Text
      -- ^ New name for the group. Only include this if changing the group's
      -- name.
    } deriving (Show, Generic)

-- | Name of the group to update. If you're changing the name of the group, this
-- is the original name.
ugrGroupName :: Lens' UpdateGroup (Text)
ugrGroupName = lens _ugrGroupName (\s a -> s { _ugrGroupName = a })
{-# INLINE ugrGroupName #-}

-- | New path for the group. Only include this if changing the group's path.
ugrNewPath :: Lens' UpdateGroup (Maybe Text)
ugrNewPath = lens _ugrNewPath (\s a -> s { _ugrNewPath = a })
{-# INLINE ugrNewPath #-}

-- | New name for the group. Only include this if changing the group's name.
ugrNewGroupName :: Lens' UpdateGroup (Maybe Text)
ugrNewGroupName = lens _ugrNewGroupName (\s a -> s { _ugrNewGroupName = a })
{-# INLINE ugrNewGroupName #-}

instance ToQuery UpdateGroup where
    toQuery = genericQuery def

data UpdateGroupResponse = UpdateGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateGroup where
    type Sv UpdateGroup = IAM
    type Rs UpdateGroup = UpdateGroupResponse

    request = post "UpdateGroup"
    response _ = nullaryResponse UpdateGroupResponse
