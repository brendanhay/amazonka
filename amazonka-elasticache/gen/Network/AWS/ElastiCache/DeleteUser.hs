{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a user. The user will be
-- removed from all user groups and in turn removed from all replication
-- groups. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>.
module Network.AWS.ElastiCache.DeleteUser
  ( -- * Creating a Request
    DeleteUser (..),
    newDeleteUser,

    -- * Request Lenses
    deleteUser_userId,

    -- * Destructuring the Response
    User (..),
    newUser,

    -- * Response Lenses
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUser' smart constructor.
data DeleteUser = DeleteUser'
  { -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'deleteUser_userId' - The ID of the user.
newDeleteUser ::
  -- | 'userId'
  Prelude.Text ->
  DeleteUser
newDeleteUser pUserId_ =
  DeleteUser' {userId = pUserId_}

-- | The ID of the user.
deleteUser_userId :: Lens.Lens' DeleteUser Prelude.Text
deleteUser_userId = Lens.lens (\DeleteUser' {userId} -> userId) (\s@DeleteUser' {} a -> s {userId = a} :: DeleteUser)

instance Prelude.AWSRequest DeleteUser where
  type Rs DeleteUser = User
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteUserResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable DeleteUser

instance Prelude.NFData DeleteUser

instance Prelude.ToHeaders DeleteUser where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteUser where
  toQuery DeleteUser' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteUser" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "UserId" Prelude.=: userId
      ]
