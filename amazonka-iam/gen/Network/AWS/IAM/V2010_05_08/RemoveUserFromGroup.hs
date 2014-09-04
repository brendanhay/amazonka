{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.RemoveUserFromGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified user from the specified group.
-- https://iam.amazonaws.com/ ?Action=RemoveUserFromGroup &GroupName=Managers
-- &UserName=Bob &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.RemoveUserFromGroup
    (
    -- * Request
      RemoveUserFromGroup
    -- ** Request constructor
    , removeUserFromGroup
    -- ** Request lenses
    , rufgrUserName
    , rufgrGroupName

    -- * Response
    , RemoveUserFromGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RemoveUserFromGroup' request.
removeUserFromGroup :: Text -- ^ 'rufgrUserName'
                    -> Text -- ^ 'rufgrGroupName'
                    -> RemoveUserFromGroup
removeUserFromGroup p1 p2 = RemoveUserFromGroup
    { _rufgrUserName = p1
    , _rufgrGroupName = p2
    }
{-# INLINE removeUserFromGroup #-}

data RemoveUserFromGroup = RemoveUserFromGroup
    { _rufgrUserName :: Text
      -- ^ Name of the user to remove.
    , _rufgrGroupName :: Text
      -- ^ Name of the group to update.
    } deriving (Show, Generic)

-- | Name of the user to remove.
rufgrUserName :: Lens' RemoveUserFromGroup (Text)
rufgrUserName f x =
    f (_rufgrUserName x)
        <&> \y -> x { _rufgrUserName = y }
{-# INLINE rufgrUserName #-}

-- | Name of the group to update.
rufgrGroupName :: Lens' RemoveUserFromGroup (Text)
rufgrGroupName f x =
    f (_rufgrGroupName x)
        <&> \y -> x { _rufgrGroupName = y }
{-# INLINE rufgrGroupName #-}

instance ToQuery RemoveUserFromGroup where
    toQuery = genericQuery def

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RemoveUserFromGroup where
    type Sv RemoveUserFromGroup = IAM
    type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse

    request = post "RemoveUserFromGroup"
    response _ = nullaryResponse RemoveUserFromGroupResponse
