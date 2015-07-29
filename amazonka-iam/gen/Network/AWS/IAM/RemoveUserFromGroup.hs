{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
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

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removeUserFromGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rufgGroupName'
--
-- * 'rufgUserName'
data RemoveUserFromGroup = RemoveUserFromGroup'
    { _rufgGroupName :: !Text
    , _rufgUserName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveUserFromGroup' smart constructor.
removeUserFromGroup :: Text -> Text -> RemoveUserFromGroup
removeUserFromGroup pGroupName_ pUserName_ =
    RemoveUserFromGroup'
    { _rufgGroupName = pGroupName_
    , _rufgUserName = pUserName_
    }

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
        request = postQuery
        response = receiveNull RemoveUserFromGroupResponse'

instance ToHeaders RemoveUserFromGroup where
        toHeaders = const mempty

instance ToPath RemoveUserFromGroup where
        toPath = const mempty

instance ToQuery RemoveUserFromGroup where
        toQuery RemoveUserFromGroup'{..}
          = mconcat
              ["Action" =: ("RemoveUserFromGroup" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "GroupName" =: _rufgGroupName,
               "UserName" =: _rufgUserName]

-- | /See:/ 'removeUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse =
    RemoveUserFromGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveUserFromGroupResponse' smart constructor.
removeUserFromGroupResponse :: RemoveUserFromGroupResponse
removeUserFromGroupResponse = RemoveUserFromGroupResponse'
