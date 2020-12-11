{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserSecurityProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified security profiles to the specified user.
module Network.AWS.Connect.UpdateUserSecurityProfiles
  ( -- * Creating a request
    UpdateUserSecurityProfiles (..),
    mkUpdateUserSecurityProfiles,

    -- ** Request lenses
    uuspSecurityProfileIds,
    uuspUserId,
    uuspInstanceId,

    -- * Destructuring the response
    UpdateUserSecurityProfilesResponse (..),
    mkUpdateUserSecurityProfilesResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserSecurityProfiles' smart constructor.
data UpdateUserSecurityProfiles = UpdateUserSecurityProfiles'
  { securityProfileIds ::
      Lude.NonEmpty Lude.Text,
    userId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserSecurityProfiles' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'securityProfileIds' - The identifiers of the security profiles for the user.
-- * 'userId' - The identifier of the user account.
mkUpdateUserSecurityProfiles ::
  -- | 'securityProfileIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  UpdateUserSecurityProfiles
mkUpdateUserSecurityProfiles
  pSecurityProfileIds_
  pUserId_
  pInstanceId_ =
    UpdateUserSecurityProfiles'
      { securityProfileIds =
          pSecurityProfileIds_,
        userId = pUserId_,
        instanceId = pInstanceId_
      }

-- | The identifiers of the security profiles for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspSecurityProfileIds :: Lens.Lens' UpdateUserSecurityProfiles (Lude.NonEmpty Lude.Text)
uuspSecurityProfileIds = Lens.lens (securityProfileIds :: UpdateUserSecurityProfiles -> Lude.NonEmpty Lude.Text) (\s a -> s {securityProfileIds = a} :: UpdateUserSecurityProfiles)
{-# DEPRECATED uuspSecurityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspUserId :: Lens.Lens' UpdateUserSecurityProfiles Lude.Text
uuspUserId = Lens.lens (userId :: UpdateUserSecurityProfiles -> Lude.Text) (\s a -> s {userId = a} :: UpdateUserSecurityProfiles)
{-# DEPRECATED uuspUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuspInstanceId :: Lens.Lens' UpdateUserSecurityProfiles Lude.Text
uuspInstanceId = Lens.lens (instanceId :: UpdateUserSecurityProfiles -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserSecurityProfiles)
{-# DEPRECATED uuspInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest UpdateUserSecurityProfiles where
  type
    Rs UpdateUserSecurityProfiles =
      UpdateUserSecurityProfilesResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserSecurityProfilesResponse'

instance Lude.ToHeaders UpdateUserSecurityProfiles where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserSecurityProfiles where
  toJSON UpdateUserSecurityProfiles' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("SecurityProfileIds" Lude..= securityProfileIds)]
      )

instance Lude.ToPath UpdateUserSecurityProfiles where
  toPath UpdateUserSecurityProfiles' {..} =
    Lude.mconcat
      [ "/users/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS userId,
        "/security-profiles"
      ]

instance Lude.ToQuery UpdateUserSecurityProfiles where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserSecurityProfilesResponse' smart constructor.
data UpdateUserSecurityProfilesResponse = UpdateUserSecurityProfilesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserSecurityProfilesResponse' with the minimum fields required to make a request.
mkUpdateUserSecurityProfilesResponse ::
  UpdateUserSecurityProfilesResponse
mkUpdateUserSecurityProfilesResponse =
  UpdateUserSecurityProfilesResponse'
