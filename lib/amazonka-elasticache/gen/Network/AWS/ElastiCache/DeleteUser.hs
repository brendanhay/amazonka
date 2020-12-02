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
-- Module      : Network.AWS.ElastiCache.DeleteUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a user. The user will be removed from all user groups and in turn removed from all replication groups. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.DeleteUser
  ( -- * Creating a Request
    deleteUser,
    DeleteUser,

    -- * Request Lenses
    dUserId,

    -- * Destructuring the Response
    user,
    User,

    -- * Response Lenses
    uStatus,
    uARN,
    uUserGroupIds,
    uAuthentication,
    uEngine,
    uUserName,
    uAccessString,
    uUserId,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUser' smart constructor.
newtype DeleteUser = DeleteUser' {_dUserId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dUserId' - The ID of the user.
deleteUser ::
  -- | 'dUserId'
  Text ->
  DeleteUser
deleteUser pUserId_ = DeleteUser' {_dUserId = pUserId_}

-- | The ID of the user.
dUserId :: Lens' DeleteUser Text
dUserId = lens _dUserId (\s a -> s {_dUserId = a})

instance AWSRequest DeleteUser where
  type Rs DeleteUser = User
  request = postQuery elastiCache
  response =
    receiveXMLWrapper "DeleteUserResult" (\s h x -> parseXML x)

instance Hashable DeleteUser

instance NFData DeleteUser

instance ToHeaders DeleteUser where
  toHeaders = const mempty

instance ToPath DeleteUser where
  toPath = const "/"

instance ToQuery DeleteUser where
  toQuery DeleteUser' {..} =
    mconcat
      [ "Action" =: ("DeleteUser" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "UserId" =: _dUserId
      ]
