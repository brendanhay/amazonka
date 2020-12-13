{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    CreateUserGroup (..),
    mkCreateUserGroup,

    -- ** Request lenses
    cugUserIds,
    cugUserGroupId,
    cugEngine,

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

-- | /See:/ 'mkCreateUserGroup' smart constructor.
data CreateUserGroup = CreateUserGroup'
  { -- | The list of user IDs that belong to the user group.
    userIds :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | The ID of the user group.
    userGroupId :: Lude.Text,
    -- | Must be Redis.
    engine :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserGroup' with the minimum fields required to make a request.
--
-- * 'userIds' - The list of user IDs that belong to the user group.
-- * 'userGroupId' - The ID of the user group.
-- * 'engine' - Must be Redis.
mkCreateUserGroup ::
  -- | 'userGroupId'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  CreateUserGroup
mkCreateUserGroup pUserGroupId_ pEngine_ =
  CreateUserGroup'
    { userIds = Lude.Nothing,
      userGroupId = pUserGroupId_,
      engine = pEngine_
    }

-- | The list of user IDs that belong to the user group.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugUserIds :: Lens.Lens' CreateUserGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
cugUserIds = Lens.lens (userIds :: CreateUserGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {userIds = a} :: CreateUserGroup)
{-# DEPRECATED cugUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugUserGroupId :: Lens.Lens' CreateUserGroup Lude.Text
cugUserGroupId = Lens.lens (userGroupId :: CreateUserGroup -> Lude.Text) (\s a -> s {userGroupId = a} :: CreateUserGroup)
{-# DEPRECATED cugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cugEngine :: Lens.Lens' CreateUserGroup Lude.Text
cugEngine = Lens.lens (engine :: CreateUserGroup -> Lude.Text) (\s a -> s {engine = a} :: CreateUserGroup)
{-# DEPRECATED cugEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

instance Lude.AWSRequest CreateUserGroup where
  type Rs CreateUserGroup = UserGroup
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateUserGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateUserGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateUserGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserGroup where
  toQuery CreateUserGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateUserGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "UserIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> userIds),
        "UserGroupId" Lude.=: userGroupId,
        "Engine" Lude.=: engine
      ]
