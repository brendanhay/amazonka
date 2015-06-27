{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addUserToGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'autgGroupName'
--
-- * 'autgUserName'
data AddUserToGroup = AddUserToGroup'
    { _autgGroupName :: Text
    , _autgUserName  :: Text
    } deriving (Eq,Read,Show)

-- | 'AddUserToGroup' smart constructor.
addUserToGroup :: Text -> Text -> AddUserToGroup
addUserToGroup pGroupName pUserName =
    AddUserToGroup'
    { _autgGroupName = pGroupName
    , _autgUserName = pUserName
    }

-- | The name of the group to update.
autgGroupName :: Lens' AddUserToGroup Text
autgGroupName = lens _autgGroupName (\ s a -> s{_autgGroupName = a});

-- | The name of the user to add.
autgUserName :: Lens' AddUserToGroup Text
autgUserName = lens _autgUserName (\ s a -> s{_autgUserName = a});

instance AWSRequest AddUserToGroup where
        type Sv AddUserToGroup = IAM
        type Rs AddUserToGroup = AddUserToGroupResponse
        request = post
        response = receiveNull AddUserToGroupResponse'

instance ToHeaders AddUserToGroup where
        toHeaders = const mempty

instance ToPath AddUserToGroup where
        toPath = const "/"

instance ToQuery AddUserToGroup where
        toQuery AddUserToGroup'{..}
          = mconcat
              ["Action" =: ("AddUserToGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _autgGroupName,
               "UserName" =: _autgUserName]

-- | /See:/ 'addUserToGroupResponse' smart constructor.
data AddUserToGroupResponse =
    AddUserToGroupResponse'
    deriving (Eq,Read,Show)

-- | 'AddUserToGroupResponse' smart constructor.
addUserToGroupResponse :: AddUserToGroupResponse
addUserToGroupResponse = AddUserToGroupResponse'
