{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    ModifyUserGroup (..),
    mkModifyUserGroup,

    -- ** Request lenses
    mugUserIdsToAdd,
    mugUserIdsToRemove,
    mugUserGroupId,

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

-- | /See:/ 'mkModifyUserGroup' smart constructor.
data ModifyUserGroup = ModifyUserGroup'
  { userIdsToAdd ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    userIdsToRemove :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    userGroupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyUserGroup' with the minimum fields required to make a request.
--
-- * 'userGroupId' - The ID of the user group.
-- * 'userIdsToAdd' - The list of user IDs to add to the user group.
-- * 'userIdsToRemove' - The list of user IDs to remove from the user group.
mkModifyUserGroup ::
  -- | 'userGroupId'
  Lude.Text ->
  ModifyUserGroup
mkModifyUserGroup pUserGroupId_ =
  ModifyUserGroup'
    { userIdsToAdd = Lude.Nothing,
      userIdsToRemove = Lude.Nothing,
      userGroupId = pUserGroupId_
    }

-- | The list of user IDs to add to the user group.
--
-- /Note:/ Consider using 'userIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mugUserIdsToAdd :: Lens.Lens' ModifyUserGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
mugUserIdsToAdd = Lens.lens (userIdsToAdd :: ModifyUserGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {userIdsToAdd = a} :: ModifyUserGroup)
{-# DEPRECATED mugUserIdsToAdd "Use generic-lens or generic-optics with 'userIdsToAdd' instead." #-}

-- | The list of user IDs to remove from the user group.
--
-- /Note:/ Consider using 'userIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mugUserIdsToRemove :: Lens.Lens' ModifyUserGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
mugUserIdsToRemove = Lens.lens (userIdsToRemove :: ModifyUserGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {userIdsToRemove = a} :: ModifyUserGroup)
{-# DEPRECATED mugUserIdsToRemove "Use generic-lens or generic-optics with 'userIdsToRemove' instead." #-}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'userGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mugUserGroupId :: Lens.Lens' ModifyUserGroup Lude.Text
mugUserGroupId = Lens.lens (userGroupId :: ModifyUserGroup -> Lude.Text) (\s a -> s {userGroupId = a} :: ModifyUserGroup)
{-# DEPRECATED mugUserGroupId "Use generic-lens or generic-optics with 'userGroupId' instead." #-}

instance Lude.AWSRequest ModifyUserGroup where
  type Rs ModifyUserGroup = UserGroup
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyUserGroupResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ModifyUserGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyUserGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyUserGroup where
  toQuery ModifyUserGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyUserGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "UserIdsToAdd"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> userIdsToAdd),
        "UserIdsToRemove"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> userIdsToRemove),
        "UserGroupId" Lude.=: userGroupId
      ]
