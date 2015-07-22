{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_AddUserToGroup.html>
module Network.AWS.IAM.AddUserToGroup
    (
    -- * Request
      AddUserToGroup
    -- ** Request constructor
    , addUserToGroup
    -- ** Request lenses
    , autgrqGroupName
    , autgrqUserName

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
-- * 'autgrqGroupName'
--
-- * 'autgrqUserName'
data AddUserToGroup = AddUserToGroup'
    { _autgrqGroupName :: !Text
    , _autgrqUserName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddUserToGroup' smart constructor.
addUserToGroup :: Text -> Text -> AddUserToGroup
addUserToGroup pGroupName pUserName =
    AddUserToGroup'
    { _autgrqGroupName = pGroupName
    , _autgrqUserName = pUserName
    }

-- | The name of the group to update.
autgrqGroupName :: Lens' AddUserToGroup Text
autgrqGroupName = lens _autgrqGroupName (\ s a -> s{_autgrqGroupName = a});

-- | The name of the user to add.
autgrqUserName :: Lens' AddUserToGroup Text
autgrqUserName = lens _autgrqUserName (\ s a -> s{_autgrqUserName = a});

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
               "GroupName" =: _autgrqGroupName,
               "UserName" =: _autgrqUserName]

-- | /See:/ 'addUserToGroupResponse' smart constructor.
data AddUserToGroupResponse =
    AddUserToGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddUserToGroupResponse' smart constructor.
addUserToGroupResponse :: AddUserToGroupResponse
addUserToGroupResponse = AddUserToGroupResponse'
