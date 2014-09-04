{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.AddUserToGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds the specified user to the specified group. https://iam.amazonaws.com/
-- ?Action=AddUserToGroup &GroupName=Managers &UserName=Bob &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.AddUserToGroup
    (
    -- * Request
      AddUserToGroup
    -- ** Request constructor
    , mkAddUserToGroupRequest
    -- ** Request lenses
    , autgrGroupName
    , autgrUserName

    -- * Response
    , AddUserToGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddUserToGroup' request.
mkAddUserToGroupRequest :: Text -- ^ 'autgrGroupName'
                        -> Text -- ^ 'autgrUserName'
                        -> AddUserToGroup
mkAddUserToGroupRequest p1 p2 = AddUserToGroup
    { _autgrGroupName = p1
    , _autgrUserName = p2
    }
{-# INLINE mkAddUserToGroupRequest #-}

data AddUserToGroup = AddUserToGroup
    { _autgrGroupName :: Text
      -- ^ Name of the group to update.
    , _autgrUserName :: Text
      -- ^ Name of the user to add.
    } deriving (Show, Generic)

-- | Name of the group to update.
autgrGroupName :: Lens' AddUserToGroup (Text)
autgrGroupName = lens _autgrGroupName (\s a -> s { _autgrGroupName = a })
{-# INLINE autgrGroupName #-}

-- | Name of the user to add.
autgrUserName :: Lens' AddUserToGroup (Text)
autgrUserName = lens _autgrUserName (\s a -> s { _autgrUserName = a })
{-# INLINE autgrUserName #-}

instance ToQuery AddUserToGroup where
    toQuery = genericQuery def

data AddUserToGroupResponse = AddUserToGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AddUserToGroup where
    type Sv AddUserToGroup = IAM
    type Rs AddUserToGroup = AddUserToGroupResponse

    request = post "AddUserToGroup"
    response _ = nullaryResponse AddUserToGroupResponse
