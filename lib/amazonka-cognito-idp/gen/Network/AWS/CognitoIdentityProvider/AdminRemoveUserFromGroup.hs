{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminRemoveUserFromGroup
  ( -- * Creating a request
    AdminRemoveUserFromGroup (..),
    mkAdminRemoveUserFromGroup,

    -- ** Request lenses
    arufgUserPoolId,
    arufgUsername,
    arufgGroupName,

    -- * Destructuring the response
    AdminRemoveUserFromGroupResponse (..),
    mkAdminRemoveUserFromGroupResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminRemoveUserFromGroup' smart constructor.
data AdminRemoveUserFromGroup = AdminRemoveUserFromGroup'
  { userPoolId ::
      Lude.Text,
    username :: Lude.Sensitive Lude.Text,
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminRemoveUserFromGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The group name.
-- * 'userPoolId' - The user pool ID for the user pool.
-- * 'username' - The username for the user.
mkAdminRemoveUserFromGroup ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  AdminRemoveUserFromGroup
mkAdminRemoveUserFromGroup pUserPoolId_ pUsername_ pGroupName_ =
  AdminRemoveUserFromGroup'
    { userPoolId = pUserPoolId_,
      username = pUsername_,
      groupName = pGroupName_
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arufgUserPoolId :: Lens.Lens' AdminRemoveUserFromGroup Lude.Text
arufgUserPoolId = Lens.lens (userPoolId :: AdminRemoveUserFromGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminRemoveUserFromGroup)
{-# DEPRECATED arufgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The username for the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arufgUsername :: Lens.Lens' AdminRemoveUserFromGroup (Lude.Sensitive Lude.Text)
arufgUsername = Lens.lens (username :: AdminRemoveUserFromGroup -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminRemoveUserFromGroup)
{-# DEPRECATED arufgUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The group name.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arufgGroupName :: Lens.Lens' AdminRemoveUserFromGroup Lude.Text
arufgGroupName = Lens.lens (groupName :: AdminRemoveUserFromGroup -> Lude.Text) (\s a -> s {groupName = a} :: AdminRemoveUserFromGroup)
{-# DEPRECATED arufgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest AdminRemoveUserFromGroup where
  type Rs AdminRemoveUserFromGroup = AdminRemoveUserFromGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull AdminRemoveUserFromGroupResponse'

instance Lude.ToHeaders AdminRemoveUserFromGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminRemoveUserFromGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminRemoveUserFromGroup where
  toJSON AdminRemoveUserFromGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("GroupName" Lude..= groupName)
          ]
      )

instance Lude.ToPath AdminRemoveUserFromGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminRemoveUserFromGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminRemoveUserFromGroupResponse' smart constructor.
data AdminRemoveUserFromGroupResponse = AdminRemoveUserFromGroupResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminRemoveUserFromGroupResponse' with the minimum fields required to make a request.
mkAdminRemoveUserFromGroupResponse ::
  AdminRemoveUserFromGroupResponse
mkAdminRemoveUserFromGroupResponse =
  AdminRemoveUserFromGroupResponse'
