{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified group with the specified attributes.
--
-- Calling this action requires developer credentials.
-- /Important:/ If you don't provide a value for an attribute, it will be set to the default value.
module Network.AWS.CognitoIdentityProvider.UpdateGroup
  ( -- * Creating a request
    UpdateGroup (..),
    mkUpdateGroup,

    -- ** Request lenses
    ugUserPoolId,
    ugPrecedence,
    ugGroupName,
    ugDescription,
    ugRoleARN,

    -- * Destructuring the response
    UpdateGroupResponse (..),
    mkUpdateGroupResponse,

    -- ** Response lenses
    ugrsGroup,
    ugrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Lude.Text,
    -- | The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
    precedence :: Lude.Maybe Lude.Natural,
    -- | The name of the group.
    groupName :: Lude.Text,
    -- | A string containing the new description of the group.
    description :: Lude.Maybe Lude.Text,
    -- | The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool.
-- * 'precedence' - The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
-- * 'groupName' - The name of the group.
-- * 'description' - A string containing the new description of the group.
-- * 'roleARN' - The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
mkUpdateGroup ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  UpdateGroup
mkUpdateGroup pUserPoolId_ pGroupName_ =
  UpdateGroup'
    { userPoolId = pUserPoolId_,
      precedence = Lude.Nothing,
      groupName = pGroupName_,
      description = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserPoolId :: Lens.Lens' UpdateGroup Lude.Text
ugUserPoolId = Lens.lens (userPoolId :: UpdateGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateGroup)
{-# DEPRECATED ugUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPrecedence :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Natural)
ugPrecedence = Lens.lens (precedence :: UpdateGroup -> Lude.Maybe Lude.Natural) (\s a -> s {precedence = a} :: UpdateGroup)
{-# DEPRECATED ugPrecedence "Use generic-lens or generic-optics with 'precedence' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup Lude.Text
ugGroupName = Lens.lens (groupName :: UpdateGroup -> Lude.Text) (\s a -> s {groupName = a} :: UpdateGroup)
{-# DEPRECATED ugGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A string containing the new description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugDescription :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugDescription = Lens.lens (description :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateGroup)
{-# DEPRECATED ugDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugRoleARN :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Text)
ugRoleARN = Lens.lens (roleARN :: UpdateGroup -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateGroup)
{-# DEPRECATED ugRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateGroupResponse'
            Lude.<$> (x Lude..?> "Group") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateGroup where
  toJSON UpdateGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            ("Precedence" Lude..=) Lude.<$> precedence,
            Lude.Just ("GroupName" Lude..= groupName),
            ("Description" Lude..=) Lude.<$> description,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  { -- | The group object for the group.
    group :: Lude.Maybe GroupType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGroupResponse' with the minimum fields required to make a request.
--
-- * 'group' - The group object for the group.
-- * 'responseStatus' - The response status code.
mkUpdateGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateGroupResponse
mkUpdateGroupResponse pResponseStatus_ =
  UpdateGroupResponse'
    { group = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The group object for the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsGroup :: Lens.Lens' UpdateGroupResponse (Lude.Maybe GroupType)
ugrsGroup = Lens.lens (group :: UpdateGroupResponse -> Lude.Maybe GroupType) (\s a -> s {group = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugrsResponseStatus :: Lens.Lens' UpdateGroupResponse Lude.Int
ugrsResponseStatus = Lens.lens (responseStatus :: UpdateGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateGroupResponse)
{-# DEPRECATED ugrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
