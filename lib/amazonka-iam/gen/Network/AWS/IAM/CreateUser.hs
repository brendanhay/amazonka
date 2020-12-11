{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IAM user for your AWS account.
--
-- The number and size of IAM resources in an AWS account are limited. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS Quotas> in the /IAM User Guide/ .
module Network.AWS.IAM.CreateUser
  ( -- * Creating a request
    CreateUser (..),
    mkCreateUser,

    -- ** Request lenses
    cuPath,
    cuPermissionsBoundary,
    cuTags,
    cuUserName,

    -- * Destructuring the response
    CreateUserResponse (..),
    mkCreateUserResponse,

    -- ** Response lenses
    cursUser,
    cursResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUser' smart constructor.
data CreateUser = CreateUser'
  { path :: Lude.Maybe Lude.Text,
    permissionsBoundary :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- * 'path' - The path for the user name. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the user.
-- * 'tags' - A list of tags that you want to attach to the newly created user. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
-- * 'userName' - The name of the user to create.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
mkCreateUser ::
  -- | 'userName'
  Lude.Text ->
  CreateUser
mkCreateUser pUserName_ =
  CreateUser'
    { path = Lude.Nothing,
      permissionsBoundary = Lude.Nothing,
      tags = Lude.Nothing,
      userName = pUserName_
    }

-- | The path for the user name. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/).
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPath :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuPath = Lens.lens (path :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: CreateUser)
{-# DEPRECATED cuPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The ARN of the policy that is used to set the permissions boundary for the user.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuPermissionsBoundary :: Lens.Lens' CreateUser (Lude.Maybe Lude.Text)
cuPermissionsBoundary = Lens.lens (permissionsBoundary :: CreateUser -> Lude.Maybe Lude.Text) (\s a -> s {permissionsBoundary = a} :: CreateUser)
{-# DEPRECATED cuPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

-- | A list of tags that you want to attach to the newly created user. Each tag consists of a key name and an associated value. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuTags :: Lens.Lens' CreateUser (Lude.Maybe [Tag])
cuTags = Lens.lens (tags :: CreateUser -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateUser)
{-# DEPRECATED cuTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the user to create.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuUserName :: Lens.Lens' CreateUser Lude.Text
cuUserName = Lens.lens (userName :: CreateUser -> Lude.Text) (\s a -> s {userName = a} :: CreateUser)
{-# DEPRECATED cuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest CreateUser where
  type Rs CreateUser = CreateUserResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "CreateUserResult"
      ( \s h x ->
          CreateUserResponse'
            Lude.<$> (x Lude..@? "User") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUser where
  toQuery CreateUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Path" Lude.=: path,
        "PermissionsBoundary" Lude.=: permissionsBoundary,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "UserName" Lude.=: userName
      ]

-- | Contains the response to a successful 'CreateUser' request.
--
-- /See:/ 'mkCreateUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { user ::
      Lude.Maybe User,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'user' - A structure with details about the new IAM user.
mkCreateUserResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserResponse
mkCreateUserResponse pResponseStatus_ =
  CreateUserResponse'
    { user = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure with details about the new IAM user.
--
-- /Note:/ Consider using 'user' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursUser :: Lens.Lens' CreateUserResponse (Lude.Maybe User)
cursUser = Lens.lens (user :: CreateUserResponse -> Lude.Maybe User) (\s a -> s {user = a} :: CreateUserResponse)
{-# DEPRECATED cursUser "Use generic-lens or generic-optics with 'user' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursResponseStatus :: Lens.Lens' CreateUserResponse Lude.Int
cursResponseStatus = Lens.lens (responseStatus :: CreateUserResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserResponse)
{-# DEPRECATED cursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
