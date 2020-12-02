{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the list of users that belong to the user group.
module Network.AWS.ElastiCache.ModifyUserGroup
  ( -- * Creating a Request
    modifyUserGroup,
    ModifyUserGroup,

    -- * Request Lenses
    mugUserIdsToAdd,
    mugUserIdsToRemove,
    mugUserGroupId,

    -- * Destructuring the Response
    userGroup,
    UserGroup,

    -- * Response Lenses
    ugStatus,
    ugUserIds,
    ugARN,
    ugUserGroupId,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyUserGroup' smart constructor.
data ModifyUserGroup = ModifyUserGroup'
  { _mugUserIdsToAdd ::
      !(Maybe (List1 Text)),
    _mugUserIdsToRemove :: !(Maybe (List1 Text)),
    _mugUserGroupId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyUserGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mugUserIdsToAdd' - The list of user IDs to add to the user group.
--
-- * 'mugUserIdsToRemove' - The list of user IDs to remove from the user group.
--
-- * 'mugUserGroupId' - The ID of the user group.
modifyUserGroup ::
  -- | 'mugUserGroupId'
  Text ->
  ModifyUserGroup
modifyUserGroup pUserGroupId_ =
  ModifyUserGroup'
    { _mugUserIdsToAdd = Nothing,
      _mugUserIdsToRemove = Nothing,
      _mugUserGroupId = pUserGroupId_
    }

-- | The list of user IDs to add to the user group.
mugUserIdsToAdd :: Lens' ModifyUserGroup (Maybe (NonEmpty Text))
mugUserIdsToAdd = lens _mugUserIdsToAdd (\s a -> s {_mugUserIdsToAdd = a}) . mapping _List1

-- | The list of user IDs to remove from the user group.
mugUserIdsToRemove :: Lens' ModifyUserGroup (Maybe (NonEmpty Text))
mugUserIdsToRemove = lens _mugUserIdsToRemove (\s a -> s {_mugUserIdsToRemove = a}) . mapping _List1

-- | The ID of the user group.
mugUserGroupId :: Lens' ModifyUserGroup Text
mugUserGroupId = lens _mugUserGroupId (\s a -> s {_mugUserGroupId = a})

instance AWSRequest ModifyUserGroup where
  type Rs ModifyUserGroup = UserGroup
  request = postQuery elastiCache
  response =
    receiveXMLWrapper "ModifyUserGroupResult" (\s h x -> parseXML x)

instance Hashable ModifyUserGroup

instance NFData ModifyUserGroup

instance ToHeaders ModifyUserGroup where
  toHeaders = const mempty

instance ToPath ModifyUserGroup where
  toPath = const "/"

instance ToQuery ModifyUserGroup where
  toQuery ModifyUserGroup' {..} =
    mconcat
      [ "Action" =: ("ModifyUserGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "UserIdsToAdd"
          =: toQuery (toQueryList "member" <$> _mugUserIdsToAdd),
        "UserIdsToRemove"
          =: toQuery (toQueryList "member" <$> _mugUserIdsToRemove),
        "UserGroupId" =: _mugUserGroupId
      ]
