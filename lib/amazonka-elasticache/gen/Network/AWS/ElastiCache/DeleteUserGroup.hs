{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a ser group. The user group must first be disassociated from the replcation group before it can be deleted. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.DeleteUserGroup
  ( -- * Creating a request
    DeleteUserGroup (..),
    mkDeleteUserGroup,

    -- ** Request lenses
    dUserGroupId,

    -- * Destructuring the response
    UserGroup (..),
    mkUserGroup,

    -- ** Response lenses
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserGroup' smart constructor.
newtype DeleteUserGroup = DeleteUserGroup'
  { -- | The ID of the user group.
    userGroupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserGroup' with the minimum fields required to make a request.
--
-- * 'userGroupId' - The ID of the user group.
mkDeleteUserGroup ::
  -- | 'userGroupId'
  Lude.Text ->
  DeleteUserGroup
mkDeleteUserGroup pUserGroupId_ =
  DeleteUserGroup' {userGroupId = pUserGroupId_}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dUserGroupId :: Lens.Lens' DeleteUserGroup Lude.Text
dUserGroupId = Lens.lens (userGroupId :: DeleteUserGroup -> Lude.Text) (\s a -> s {userGroupId = a} :: DeleteUserGroup)
{-# DEPRECATED dUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

instance Lude.AWSRequest DeleteUserGroup where
  type Rs DeleteUserGroup = UserGroup
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DeleteUserGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DeleteUserGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteUserGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserGroup where
  toQuery DeleteUserGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteUserGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "UserGroupId" Lude.=: userGroupId
      ]
