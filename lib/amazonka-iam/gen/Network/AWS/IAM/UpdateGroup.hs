{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified IAM group.
--
-- /Important:/ You should understand the implications of changing a group's path or name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming Users and Groups> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateGroup
  ( -- * Creating a request
    UpdateGroup (..),
    mkUpdateGroup,

    -- ** Request lenses
    ugNewGroupName,
    ugNewPath,
    ugGroupName,

    -- * Destructuring the response
    UpdateGroupResponse (..),
    mkUpdateGroupResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { newGroupName ::
      Lude.Maybe Lude.Text,
    newPath :: Lude.Maybe Lude.Text,
    groupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - Name of the IAM group to update. If you're changing the name of the group, this is the original name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'newGroupName' - New name for the IAM group. Only include this if changing the group's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
-- * 'newPath' - New path for the IAM group. Only include this if changing the group's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
mkUpdateGroup ::
  -- | 'groupName'
  Lude.Text ->
  UpdateGroup
mkUpdateGroup pGroupName_ =
  UpdateGroup'
    { newGroupName = Lude.Nothing,
      newPath = Lude.Nothing,
      groupName = pGroupName_
    }

-- | New name for the IAM group. Only include this if changing the group's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'newGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugNewGroupName :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugNewGroupName = Lens.lens (newGroupName :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {newGroupName = a} :: UpdateGroup)
{-# DEPRECATED ugNewGroupName "Use generic-lens or generic-optics with 'newGroupName' instead." #-}

-- | New path for the IAM group. Only include this if changing the group's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'newPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugNewPath :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugNewPath = Lens.lens (newPath :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {newPath = a} :: UpdateGroup)
{-# DEPRECATED ugNewPath "Use generic-lens or generic-optics with 'newPath' instead." #-}

-- | Name of the IAM group to update. If you're changing the name of the group, this is the original name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup Lude.Text
ugGroupName = Lens.lens (groupName :: UpdateGroup -> Lude.Text) (\s a -> s {groupName = a} :: UpdateGroup)
{-# DEPRECATED ugGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateGroupResponse'

instance Lude.ToHeaders UpdateGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGroup where
  toQuery UpdateGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "NewGroupName" Lude.=: newGroupName,
        "NewPath" Lude.=: newPath,
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
mkUpdateGroupResponse ::
  UpdateGroupResponse
mkUpdateGroupResponse = UpdateGroupResponse'
