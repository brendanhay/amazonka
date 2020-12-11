{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ugPrecedence,
    ugDescription,
    ugRoleARN,
    ugGroupName,
    ugUserPoolId,

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

-- | Creates a value of 'UpdateGroup' with the minimum fields required to make a request.
--
-- * 'description' - A string containing the new description of the group.
-- * 'groupName' - The name of the group.
-- * 'precedence' - The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
-- * 'roleARN' - The new role ARN for the group. This is used for setting the @cognito:roles@ and @cognito:preferred_role@ claims in the token.
-- * 'userPoolId' - The user pool ID for the user pool.
mkUpdateGroup ::
  -- | 'groupName'
  Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  UpdateGroup
mkUpdateGroup pGroupName_ pUserPoolId_ =
  UpdateGroup'
    { precedence = Lude.Nothing,
      description = Lude.Nothing,
      roleARN = Lude.Nothing,
      groupName = pGroupName_,
      userPoolId = pUserPoolId_
    }

-- | The new precedence value for the group. For more information about this parameter, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_CreateGroup.html CreateGroup> .
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugPrecedence :: Lens.Lens' UpdateGroup (Lude.Maybe Lude.Natural)
ugPrecedence = Lens.lens (precedence :: UpdateGroup -> Lude.Maybe Lude.Natural) (\s a -> s {precedence = a} :: UpdateGroup)
{-# DEPRECATED ugPrecedence "Use generic-lens or generic-optics with 'precedence' instead." #-}

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

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugGroupName :: Lens.Lens' UpdateGroup Lude.Text
ugGroupName = Lens.lens (groupName :: UpdateGroup -> Lude.Text) (\s a -> s {groupName = a} :: UpdateGroup)
{-# DEPRECATED ugGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugUserPoolId :: Lens.Lens' UpdateGroup Lude.Text
ugUserPoolId = Lens.lens (userPoolId :: UpdateGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateGroup)
{-# DEPRECATED ugUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

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
          [ ("Precedence" Lude..=) Lude.<$> precedence,
            ("Description" Lude..=) Lude.<$> description,
            ("RoleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("GroupName" Lude..= groupName),
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath UpdateGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
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
