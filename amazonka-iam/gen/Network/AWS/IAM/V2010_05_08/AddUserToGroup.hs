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
    , addUserToGroup
    -- ** Request lenses
    , autgrUserName
    , autgrGroupName

    -- * Response
    , AddUserToGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AddUserToGroup' request.
addUserToGroup :: Text -- ^ 'autgrUserName'
               -> Text -- ^ 'autgrGroupName'
               -> AddUserToGroup
addUserToGroup p1 p2 = AddUserToGroup
    { _autgrUserName = p1
    , _autgrGroupName = p2
    }
{-# INLINE addUserToGroup #-}

data AddUserToGroup = AddUserToGroup
    { _autgrUserName :: Text
      -- ^ Name of the user to add.
    , _autgrGroupName :: Text
      -- ^ Name of the group to update.
    } deriving (Show, Generic)

-- | Name of the user to add.
autgrUserName :: Lens' AddUserToGroup (Text)
autgrUserName f x =
    f (_autgrUserName x)
        <&> \y -> x { _autgrUserName = y }
{-# INLINE autgrUserName #-}

-- | Name of the group to update.
autgrGroupName :: Lens' AddUserToGroup (Text)
autgrGroupName f x =
    f (_autgrGroupName x)
        <&> \y -> x { _autgrGroupName = y }
{-# INLINE autgrGroupName #-}

instance ToQuery AddUserToGroup where
    toQuery = genericQuery def

data AddUserToGroupResponse = AddUserToGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest AddUserToGroup where
    type Sv AddUserToGroup = IAM
    type Rs AddUserToGroup = AddUserToGroupResponse

    request = post "AddUserToGroup"
    response _ = nullaryResponse AddUserToGroupResponse
