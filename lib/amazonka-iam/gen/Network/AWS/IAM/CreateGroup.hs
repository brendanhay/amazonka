{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group.
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgPath,
    cgGroupName,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrsResponseStatus,
    cgrsGroup,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { path :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group to create. Do not include the path in this value.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
-- * 'path' - The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
mkCreateGroup ::
  -- | 'groupName'
  Lude.Text ->
  CreateGroup
mkCreateGroup pGroupName_ =
  CreateGroup' {path = Lude.Nothing, groupName = pGroupName_}

-- | The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgPath :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgPath = Lens.lens (path :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CreateGroup)
{-# DEPRECATED cgPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the group to create. Do not include the path in this value.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGroupName :: Lens.Lens' CreateGroup Lude.Text
cgGroupName = Lens.lens (groupName :: CreateGroup -> Lude.Text) (\s a -> s {groupName = a} :: CreateGroup)
{-# DEPRECATED cgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateGroupResult"
      ( \s h x ->
          CreateGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "Group")
      )

instance Lude.ToHeaders CreateGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGroup where
  toQuery CreateGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Path" Lude.=: path,
        "GroupName" Lude.=: groupName
      ]

-- | Contains the response to a successful 'CreateGroup' request.
--
-- /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { responseStatus ::
      Lude.Int,
    group :: Group
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - A structure containing details about the new group.
-- * 'responseStatus' - The response status code.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'group'
  Group ->
  CreateGroupResponse
mkCreateGroupResponse pResponseStatus_ pGroup_ =
  CreateGroupResponse'
    { responseStatus = pResponseStatus_,
      group = pGroup_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGroupResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing details about the new group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGroup :: Lens.Lens' CreateGroupResponse Group
cgrsGroup = Lens.lens (group :: CreateGroupResponse -> Group) (\s a -> s {group = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}
