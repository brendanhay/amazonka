{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.RemoveUserFromGroup
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

-- | Removes the specified user from the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_RemoveUserFromGroup.html>
module Network.AWS.IAM.RemoveUserFromGroup
    (
    -- * Request
      RemoveUserFromGroup
    -- ** Request constructor
    , removeUserFromGroup
    -- ** Request lenses
    , rufgGroupName
    , rufgUserName

    -- * Response
    , RemoveUserFromGroupResponse
    -- ** Response constructor
    , removeUserFromGroupResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'removeUserFromGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rufgGroupName'
--
-- * 'rufgUserName'
data RemoveUserFromGroup = RemoveUserFromGroup'{_rufgGroupName :: Text, _rufgUserName :: Text} deriving (Eq, Read, Show)

-- | 'RemoveUserFromGroup' smart constructor.
removeUserFromGroup :: Text -> Text -> RemoveUserFromGroup
removeUserFromGroup pGroupName pUserName = RemoveUserFromGroup'{_rufgGroupName = pGroupName, _rufgUserName = pUserName};

-- | The name of the group to update.
rufgGroupName :: Lens' RemoveUserFromGroup Text
rufgGroupName = lens _rufgGroupName (\ s a -> s{_rufgGroupName = a});

-- | The name of the user to remove.
rufgUserName :: Lens' RemoveUserFromGroup Text
rufgUserName = lens _rufgUserName (\ s a -> s{_rufgUserName = a});

instance AWSRequest RemoveUserFromGroup where
        type Sv RemoveUserFromGroup = IAM
        type Rs RemoveUserFromGroup =
             RemoveUserFromGroupResponse
        request = post
        response = receiveNull RemoveUserFromGroupResponse'

instance ToHeaders RemoveUserFromGroup where
        toHeaders = const mempty

instance ToPath RemoveUserFromGroup where
        toPath = const "/"

instance ToQuery RemoveUserFromGroup where
        toQuery RemoveUserFromGroup'{..}
          = mconcat
              ["Action" =: ("RemoveUserFromGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _rufgGroupName,
               "UserName" =: _rufgUserName]

-- | /See:/ 'removeUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse' deriving (Eq, Read, Show)

-- | 'RemoveUserFromGroupResponse' smart constructor.
removeUserFromGroupResponse :: RemoveUserFromGroupResponse
removeUserFromGroupResponse = RemoveUserFromGroupResponse';
