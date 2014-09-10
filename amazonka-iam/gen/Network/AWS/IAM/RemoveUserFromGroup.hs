{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM
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
module Network.AWS.IAM
    (
    -- * Request
      RemoveUserFromGroup
    -- ** Request constructor
    , mkRemoveUserFromGroup
    -- ** Request lenses
    , rufgGroupName
    , rufgUserName

    -- * Response
    , RemoveUserFromGroupResponse
    -- ** Response constructor
    , mkRemoveUserFromGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data RemoveUserFromGroup = RemoveUserFromGroup
    { _rufgGroupName :: !Text
    , _rufgUserName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveUserFromGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @UserName ::@ @Text@
--
mkRemoveUserFromGroup :: Text -- ^ 'rufgGroupName'
                      -> Text -- ^ 'rufgUserName'
                      -> RemoveUserFromGroup
mkRemoveUserFromGroup p1 p2 = RemoveUserFromGroup
    { _rufgGroupName = p1
    , _rufgUserName = p2
    }

-- | Name of the group to update.
rufgGroupName :: Lens' RemoveUserFromGroup Text
rufgGroupName = lens _rufgGroupName (\s a -> s { _rufgGroupName = a })

-- | Name of the user to remove.
rufgUserName :: Lens' RemoveUserFromGroup Text
rufgUserName = lens _rufgUserName (\s a -> s { _rufgUserName = a })

instance ToQuery RemoveUserFromGroup where
    toQuery = genericQuery def

data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveUserFromGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRemoveUserFromGroupResponse :: RemoveUserFromGroupResponse
mkRemoveUserFromGroupResponse = RemoveUserFromGroupResponse

instance AWSRequest RemoveUserFromGroup where
    type Sv RemoveUserFromGroup = IAM
    type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse

    request = post "RemoveUserFromGroup"
    response _ = nullaryResponse RemoveUserFromGroupResponse
