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
-- Module      : Network.AWS.ElastiCache.CreateUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user group. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>
module Network.AWS.ElastiCache.CreateUserGroup
  ( -- * Creating a Request
    createUserGroup,
    CreateUserGroup,

    -- * Request Lenses
    cugUserIds,
    cugUserGroupId,
    cugEngine,

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

-- | /See:/ 'createUserGroup' smart constructor.
data CreateUserGroup = CreateUserGroup'
  { _cugUserIds ::
      !(Maybe (List1 Text)),
    _cugUserGroupId :: !Text,
    _cugEngine :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cugUserIds' - The list of user IDs that belong to the user group.
--
-- * 'cugUserGroupId' - The ID of the user group.
--
-- * 'cugEngine' - Must be Redis.
createUserGroup ::
  -- | 'cugUserGroupId'
  Text ->
  -- | 'cugEngine'
  Text ->
  CreateUserGroup
createUserGroup pUserGroupId_ pEngine_ =
  CreateUserGroup'
    { _cugUserIds = Nothing,
      _cugUserGroupId = pUserGroupId_,
      _cugEngine = pEngine_
    }

-- | The list of user IDs that belong to the user group.
cugUserIds :: Lens' CreateUserGroup (Maybe (NonEmpty Text))
cugUserIds = lens _cugUserIds (\s a -> s {_cugUserIds = a}) . mapping _List1

-- | The ID of the user group.
cugUserGroupId :: Lens' CreateUserGroup Text
cugUserGroupId = lens _cugUserGroupId (\s a -> s {_cugUserGroupId = a})

-- | Must be Redis.
cugEngine :: Lens' CreateUserGroup Text
cugEngine = lens _cugEngine (\s a -> s {_cugEngine = a})

instance AWSRequest CreateUserGroup where
  type Rs CreateUserGroup = UserGroup
  request = postQuery elastiCache
  response =
    receiveXMLWrapper "CreateUserGroupResult" (\s h x -> parseXML x)

instance Hashable CreateUserGroup

instance NFData CreateUserGroup

instance ToHeaders CreateUserGroup where
  toHeaders = const mempty

instance ToPath CreateUserGroup where
  toPath = const "/"

instance ToQuery CreateUserGroup where
  toQuery CreateUserGroup' {..} =
    mconcat
      [ "Action" =: ("CreateUserGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "UserIds" =: toQuery (toQueryList "member" <$> _cugUserIds),
        "UserGroupId" =: _cugUserGroupId,
        "Engine" =: _cugEngine
      ]
