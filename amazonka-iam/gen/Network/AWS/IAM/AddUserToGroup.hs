{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds the specified user to the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddUserToGroup.html>
module Network.AWS.IAM.AddUserToGroup
    (
    -- * Request
      AddUserToGroup
    -- ** Request constructor
    , addUserToGroup
    -- ** Request lenses
    , autgGroupName
    , autgUserName

    -- * Response
    , AddUserToGroupResponse
    -- ** Response constructor
    , addUserToGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data AddUserToGroup = AddUserToGroup
    { _autgGroupName :: Text
    , _autgUserName  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AddUserToGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'autgGroupName' @::@ 'Text'
--
-- * 'autgUserName' @::@ 'Text'
--
addUserToGroup :: Text -- ^ 'autgGroupName'
               -> Text -- ^ 'autgUserName'
               -> AddUserToGroup
addUserToGroup p1 p2 = AddUserToGroup
    { _autgGroupName = p1
    , _autgUserName  = p2
    }

-- | The name of the group to update.
autgGroupName :: Lens' AddUserToGroup Text
autgGroupName = lens _autgGroupName (\s a -> s { _autgGroupName = a })

-- | The name of the user to add.
autgUserName :: Lens' AddUserToGroup Text
autgUserName = lens _autgUserName (\s a -> s { _autgUserName = a })

data AddUserToGroupResponse = AddUserToGroupResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AddUserToGroupResponse' constructor.
addUserToGroupResponse :: AddUserToGroupResponse
addUserToGroupResponse = AddUserToGroupResponse

instance ToPath AddUserToGroup where
    toPath = const "/"

instance ToQuery AddUserToGroup where
    toQuery AddUserToGroup{..} = mconcat
        [ "GroupName" =? _autgGroupName
        , "UserName"  =? _autgUserName
        ]

instance ToHeaders AddUserToGroup

instance AWSRequest AddUserToGroup where
    type Sv AddUserToGroup = IAM
    type Rs AddUserToGroup = AddUserToGroupResponse

    request  = post "AddUserToGroup"
    response = nullResponse AddUserToGroupResponse
