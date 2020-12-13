{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminAddUserToGroup
  ( -- * Creating a request
    AdminAddUserToGroup (..),
    mkAdminAddUserToGroup,

    -- ** Request lenses
    aautgUserPoolId,
    aautgUsername,
    aautgGroupName,

    -- * Destructuring the response
    AdminAddUserToGroupResponse (..),
    mkAdminAddUserToGroupResponse,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAdminAddUserToGroup' smart constructor.
data AdminAddUserToGroup = AdminAddUserToGroup'
  { -- | The user pool ID for the user pool.
    userPoolId :: Lude.Text,
    -- | The username for the user.
    username :: Lude.Sensitive Lude.Text,
    -- | The group name.
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminAddUserToGroup' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool.
-- * 'username' - The username for the user.
-- * 'groupName' - The group name.
mkAdminAddUserToGroup ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  AdminAddUserToGroup
mkAdminAddUserToGroup pUserPoolId_ pUsername_ pGroupName_ =
  AdminAddUserToGroup'
    { userPoolId = pUserPoolId_,
      username = pUsername_,
      groupName = pGroupName_
    }

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aautgUserPoolId :: Lens.Lens' AdminAddUserToGroup Lude.Text
aautgUserPoolId = Lens.lens (userPoolId :: AdminAddUserToGroup -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminAddUserToGroup)
{-# DEPRECATED aautgUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The username for the user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aautgUsername :: Lens.Lens' AdminAddUserToGroup (Lude.Sensitive Lude.Text)
aautgUsername = Lens.lens (username :: AdminAddUserToGroup -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminAddUserToGroup)
{-# DEPRECATED aautgUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The group name.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aautgGroupName :: Lens.Lens' AdminAddUserToGroup Lude.Text
aautgGroupName = Lens.lens (groupName :: AdminAddUserToGroup -> Lude.Text) (\s a -> s {groupName = a} :: AdminAddUserToGroup)
{-# DEPRECATED aautgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest AdminAddUserToGroup where
  type Rs AdminAddUserToGroup = AdminAddUserToGroupResponse
  request = Req.postJSON cognitoIdentityProviderService
  response = Res.receiveNull AdminAddUserToGroupResponse'

instance Lude.ToHeaders AdminAddUserToGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminAddUserToGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminAddUserToGroup where
  toJSON AdminAddUserToGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("GroupName" Lude..= groupName)
          ]
      )

instance Lude.ToPath AdminAddUserToGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminAddUserToGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAdminAddUserToGroupResponse' smart constructor.
data AdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminAddUserToGroupResponse' with the minimum fields required to make a request.
mkAdminAddUserToGroupResponse ::
  AdminAddUserToGroupResponse
mkAdminAddUserToGroupResponse = AdminAddUserToGroupResponse'
