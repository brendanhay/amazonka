{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
module Network.AWS.IAM.RemoveUserFromGroup
  ( -- * Creating a request
    RemoveUserFromGroup (..),
    mkRemoveUserFromGroup,

    -- ** Request lenses
    rufgGroupName,
    rufgUserName,

    -- * Destructuring the response
    RemoveUserFromGroupResponse (..),
    mkRemoveUserFromGroupResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveUserFromGroup' smart constructor.
data RemoveUserFromGroup = RemoveUserFromGroup'
  { groupName ::
      Lude.Text,
    userName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveUserFromGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'userName' - The name of the user to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkRemoveUserFromGroup ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'userName'
  Lude.Text ->
  RemoveUserFromGroup
mkRemoveUserFromGroup pGroupName_ pUserName_ =
  RemoveUserFromGroup'
    { groupName = pGroupName_,
      userName = pUserName_
    }

-- | The name of the group to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rufgGroupName :: Lens.Lens' RemoveUserFromGroup Lude.Text
rufgGroupName = Lens.lens (groupName :: RemoveUserFromGroup -> Lude.Text) (\s a -> s {groupName = a} :: RemoveUserFromGroup)
{-# DEPRECATED rufgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The name of the user to remove.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rufgUserName :: Lens.Lens' RemoveUserFromGroup Lude.Text
rufgUserName = Lens.lens (userName :: RemoveUserFromGroup -> Lude.Text) (\s a -> s {userName = a} :: RemoveUserFromGroup)
{-# DEPRECATED rufgUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest RemoveUserFromGroup where
  type Rs RemoveUserFromGroup = RemoveUserFromGroupResponse
  request = Req.postQuery iamService
  response = Res.receiveNull RemoveUserFromGroupResponse'

instance Lude.ToHeaders RemoveUserFromGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveUserFromGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveUserFromGroup where
  toQuery RemoveUserFromGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveUserFromGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "GroupName" Lude.=: groupName,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkRemoveUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveUserFromGroupResponse' with the minimum fields required to make a request.
mkRemoveUserFromGroupResponse ::
  RemoveUserFromGroupResponse
mkRemoveUserFromGroupResponse = RemoveUserFromGroupResponse'
