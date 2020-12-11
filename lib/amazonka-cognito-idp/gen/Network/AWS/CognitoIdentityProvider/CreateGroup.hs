{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new group in the specified user pool.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.CreateGroup
  ( -- * Creating a request
    CreateGroup (..),
    mkCreateGroup,

    -- ** Request lenses
    cgPrecedence,
    cgDescription,
    cgRoleARN,
    cgGroupName,
    cgUserPoolId,

    -- * Destructuring the response
    CreateGroupResponse (..),
    mkCreateGroupResponse,

    -- ** Response lenses
    cgrsGroup,
    cgrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { precedence ::
      Lude.Maybe Lude.Natural,
    description :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    groupName :: Lude.Text,
    userPoolId :: Lude.Text
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
-- * 'description' - A string containing the description of the group.
-- * 'groupName' - The name of the group. Must be unique.
-- * 'precedence' - A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. Zero is the highest precedence value. Groups with lower @Precedence@ values take precedence over groups with higher or null @Precedence@ values. If a user belongs to two or more groups, it is the group with the lowest precedence value whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
-- * 'roleARN' - The role ARN for the group.
-- * 'userPoolId' - The user pool ID for the user pool.
mkCreateGroup ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  CreateGroup
mkCreateGroup pGroupName_ pUserPoolId_ =
  CreateGroup'
    { precedence = Lude.Nothing,
      description = Lude.Nothing,
      roleARN = Lude.Nothing,
      groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. Zero is the highest precedence value. Groups with lower @Precedence@ values take precedence over groups with higher or null @Precedence@ values. If a user belongs to two or more groups, it is the group with the lowest precedence value whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgPrecedence :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Natural)
cgPrecedence = Lens.lens (precedence :: CreateGroup -> Lude.Maybe Lude.Natural) (\s a -> s {precedence = a} :: CreateGroup)
{-# DEPRECATED cgPrecedence "Use generic-lens or generic-optics with 'precedence' instead." #-}

-- | A string containing the description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgDescription :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgDescription = Lens.lens (description :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateGroup)
{-# DEPRECATED cgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The role ARN for the group.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgRoleARN :: Lens.Lens' CreateGroup (Lude.Maybe Lude.Text)
cgRoleARN = Lens.lens (roleARN :: CreateGroup -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CreateGroup)
{-# DEPRECATED cgRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name of the group. Must be unique.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgGroupName :: Lens.Lens' CreateGroup Lude.Text
cgGroupName = Lens.lens (groupName :: CreateGroup -> Lude.Text) (\s a -> s {groupName = a} :: CreateGroup)
{-# DEPRECATED cgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgUserPoolId :: Lens.Lens' CreateGroup Lude.Text
cgUserPoolId = Lens.lens (userPoolId :: CreateGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: CreateGroup)
{-# DEPRECATED cgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest CreateGroup where
  type Rs CreateGroup = CreateGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Precedence" Lude..=) Lude.<$> precedence,
            ("Description" Lude..=) Lude.<$> description,
            ("RoleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("GroupName" Lude..= groupName),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath CreateGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { group ::
      Lude.Maybe GroupType,
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

-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The group object for the group.
-- * 'responseStatus' - The response status code.
mkCreateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateGroupResponse
mkCreateGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsGroup :: Lens.Lens' CreateGroupResponse (Lude.Maybe GroupType)
cgrsGroup = Lens.lens (group :: CreateGroupResponse -> Lude.Maybe GroupType) (\s a -> s {group = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrsResponseStatus :: Lens.Lens' CreateGroupResponse Lude.Int
cgrsResponseStatus = Lens.lens (responseStatus :: CreateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateGroupResponse)
{-# DEPRECATED cgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
