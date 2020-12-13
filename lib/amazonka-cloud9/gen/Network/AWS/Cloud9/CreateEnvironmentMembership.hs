{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.CreateEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an environment member to an AWS Cloud9 development environment.
module Network.AWS.Cloud9.CreateEnvironmentMembership
  ( -- * Creating a request
    CreateEnvironmentMembership (..),
    mkCreateEnvironmentMembership,

    -- ** Request lenses
    cemUserARN,
    cemPermissions,
    cemEnvironmentId,

    -- * Destructuring the response
    CreateEnvironmentMembershipResponse (..),
    mkCreateEnvironmentMembershipResponse,

    -- ** Response lenses
    cemrsMembership,
    cemrsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEnvironmentMembership' smart constructor.
data CreateEnvironmentMembership = CreateEnvironmentMembership'
  { -- | The Amazon Resource Name (ARN) of the environment member you want to add.
    userARN :: Lude.Text,
    -- | The type of environment member permissions you want to associate with this environment member. Available values include:
    --
    --
    --     * @read-only@ : Has read-only access to the environment.
    --
    --
    --     * @read-write@ : Has read-write access to the environment.
    permissions :: MemberPermissions,
    -- | The ID of the environment that contains the environment member you want to add.
    environmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEnvironmentMembership' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the environment member you want to add.
-- * 'permissions' - The type of environment member permissions you want to associate with this environment member. Available values include:
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
-- * 'environmentId' - The ID of the environment that contains the environment member you want to add.
mkCreateEnvironmentMembership ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'permissions'
  MemberPermissions ->
  -- | 'environmentId'
  Lude.Text ->
  CreateEnvironmentMembership
mkCreateEnvironmentMembership
  pUserARN_
  pPermissions_
  pEnvironmentId_ =
    CreateEnvironmentMembership'
      { userARN = pUserARN_,
        permissions = pPermissions_,
        environmentId = pEnvironmentId_
      }

-- | The Amazon Resource Name (ARN) of the environment member you want to add.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemUserARN :: Lens.Lens' CreateEnvironmentMembership Lude.Text
cemUserARN = Lens.lens (userARN :: CreateEnvironmentMembership -> Lude.Text) (\s a -> s {userARN = a} :: CreateEnvironmentMembership)
{-# DEPRECATED cemUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The type of environment member permissions you want to associate with this environment member. Available values include:
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemPermissions :: Lens.Lens' CreateEnvironmentMembership MemberPermissions
cemPermissions = Lens.lens (permissions :: CreateEnvironmentMembership -> MemberPermissions) (\s a -> s {permissions = a} :: CreateEnvironmentMembership)
{-# DEPRECATED cemPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The ID of the environment that contains the environment member you want to add.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemEnvironmentId :: Lens.Lens' CreateEnvironmentMembership Lude.Text
cemEnvironmentId = Lens.lens (environmentId :: CreateEnvironmentMembership -> Lude.Text) (\s a -> s {environmentId = a} :: CreateEnvironmentMembership)
{-# DEPRECATED cemEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

instance Lude.AWSRequest CreateEnvironmentMembership where
  type
    Rs CreateEnvironmentMembership =
      CreateEnvironmentMembershipResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateEnvironmentMembershipResponse'
            Lude.<$> (x Lude..?> "membership") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateEnvironmentMembership where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.CreateEnvironmentMembership" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateEnvironmentMembership where
  toJSON CreateEnvironmentMembership' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("userArn" Lude..= userARN),
            Lude.Just ("permissions" Lude..= permissions),
            Lude.Just ("environmentId" Lude..= environmentId)
          ]
      )

instance Lude.ToPath CreateEnvironmentMembership where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateEnvironmentMembership where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateEnvironmentMembershipResponse' smart constructor.
data CreateEnvironmentMembershipResponse = CreateEnvironmentMembershipResponse'
  { -- | Information about the environment member that was added.
    membership :: Lude.Maybe EnvironmentMember,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEnvironmentMembershipResponse' with the minimum fields required to make a request.
--
-- * 'membership' - Information about the environment member that was added.
-- * 'responseStatus' - The response status code.
mkCreateEnvironmentMembershipResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateEnvironmentMembershipResponse
mkCreateEnvironmentMembershipResponse pResponseStatus_ =
  CreateEnvironmentMembershipResponse'
    { membership = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the environment member that was added.
--
-- /Note:/ Consider using 'membership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemrsMembership :: Lens.Lens' CreateEnvironmentMembershipResponse (Lude.Maybe EnvironmentMember)
cemrsMembership = Lens.lens (membership :: CreateEnvironmentMembershipResponse -> Lude.Maybe EnvironmentMember) (\s a -> s {membership = a} :: CreateEnvironmentMembershipResponse)
{-# DEPRECATED cemrsMembership "Use generic-lens or generic-optics with 'membership' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemrsResponseStatus :: Lens.Lens' CreateEnvironmentMembershipResponse Lude.Int
cemrsResponseStatus = Lens.lens (responseStatus :: CreateEnvironmentMembershipResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateEnvironmentMembershipResponse)
{-# DEPRECATED cemrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
