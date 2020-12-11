{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.UpdateEnvironmentMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing environment member for an AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironmentMembership
  ( -- * Creating a request
    UpdateEnvironmentMembership (..),
    mkUpdateEnvironmentMembership,

    -- ** Request lenses
    uemEnvironmentId,
    uemUserARN,
    uemPermissions,

    -- * Destructuring the response
    UpdateEnvironmentMembershipResponse (..),
    mkUpdateEnvironmentMembershipResponse,

    -- ** Response lenses
    uemrsMembership,
    uemrsResponseStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateEnvironmentMembership' smart constructor.
data UpdateEnvironmentMembership = UpdateEnvironmentMembership'
  { environmentId ::
      Lude.Text,
    userARN :: Lude.Text,
    permissions :: MemberPermissions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEnvironmentMembership' with the minimum fields required to make a request.
--
-- * 'environmentId' - The ID of the environment for the environment member whose settings you want to change.
-- * 'permissions' - The replacement type of environment member permissions you want to associate with this environment member. Available values include:
--
--
--     * @read-only@ : Has read-only access to the environment.
--
--
--     * @read-write@ : Has read-write access to the environment.
--
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the environment member whose settings you want to change.
mkUpdateEnvironmentMembership ::
  -- | 'environmentId'
  Lude.Text ->
  -- | 'userARN'
  Lude.Text ->
  -- | 'permissions'
  MemberPermissions ->
  UpdateEnvironmentMembership
mkUpdateEnvironmentMembership
  pEnvironmentId_
  pUserARN_
  pPermissions_ =
    UpdateEnvironmentMembership'
      { environmentId = pEnvironmentId_,
        userARN = pUserARN_,
        permissions = pPermissions_
      }

-- | The ID of the environment for the environment member whose settings you want to change.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemEnvironmentId :: Lens.Lens' UpdateEnvironmentMembership Lude.Text
uemEnvironmentId = Lens.lens (environmentId :: UpdateEnvironmentMembership -> Lude.Text) (\s a -> s {environmentId = a} :: UpdateEnvironmentMembership)
{-# DEPRECATED uemEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The Amazon Resource Name (ARN) of the environment member whose settings you want to change.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemUserARN :: Lens.Lens' UpdateEnvironmentMembership Lude.Text
uemUserARN = Lens.lens (userARN :: UpdateEnvironmentMembership -> Lude.Text) (\s a -> s {userARN = a} :: UpdateEnvironmentMembership)
{-# DEPRECATED uemUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The replacement type of environment member permissions you want to associate with this environment member. Available values include:
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
uemPermissions :: Lens.Lens' UpdateEnvironmentMembership MemberPermissions
uemPermissions = Lens.lens (permissions :: UpdateEnvironmentMembership -> MemberPermissions) (\s a -> s {permissions = a} :: UpdateEnvironmentMembership)
{-# DEPRECATED uemPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

instance Lude.AWSRequest UpdateEnvironmentMembership where
  type
    Rs UpdateEnvironmentMembership =
      UpdateEnvironmentMembershipResponse
  request = Req.postJSON cloud9Service
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateEnvironmentMembershipResponse'
            Lude.<$> (x Lude..?> "membership") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateEnvironmentMembership where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCloud9WorkspaceManagementService.UpdateEnvironmentMembership" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateEnvironmentMembership where
  toJSON UpdateEnvironmentMembership' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("environmentId" Lude..= environmentId),
            Lude.Just ("userArn" Lude..= userARN),
            Lude.Just ("permissions" Lude..= permissions)
          ]
      )

instance Lude.ToPath UpdateEnvironmentMembership where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateEnvironmentMembership where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateEnvironmentMembershipResponse' smart constructor.
data UpdateEnvironmentMembershipResponse = UpdateEnvironmentMembershipResponse'
  { membership ::
      Lude.Maybe
        EnvironmentMember,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEnvironmentMembershipResponse' with the minimum fields required to make a request.
--
-- * 'membership' - Information about the environment member whose settings were changed.
-- * 'responseStatus' - The response status code.
mkUpdateEnvironmentMembershipResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateEnvironmentMembershipResponse
mkUpdateEnvironmentMembershipResponse pResponseStatus_ =
  UpdateEnvironmentMembershipResponse'
    { membership = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the environment member whose settings were changed.
--
-- /Note:/ Consider using 'membership' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemrsMembership :: Lens.Lens' UpdateEnvironmentMembershipResponse (Lude.Maybe EnvironmentMember)
uemrsMembership = Lens.lens (membership :: UpdateEnvironmentMembershipResponse -> Lude.Maybe EnvironmentMember) (\s a -> s {membership = a} :: UpdateEnvironmentMembershipResponse)
{-# DEPRECATED uemrsMembership "Use generic-lens or generic-optics with 'membership' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uemrsResponseStatus :: Lens.Lens' UpdateEnvironmentMembershipResponse Lude.Int
uemrsResponseStatus = Lens.lens (responseStatus :: UpdateEnvironmentMembershipResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateEnvironmentMembershipResponse)
{-# DEPRECATED uemrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
