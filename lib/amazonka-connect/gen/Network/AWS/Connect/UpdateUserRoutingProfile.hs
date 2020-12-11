{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserRoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the specified routing profile to the specified user.
module Network.AWS.Connect.UpdateUserRoutingProfile
  ( -- * Creating a request
    UpdateUserRoutingProfile (..),
    mkUpdateUserRoutingProfile,

    -- ** Request lenses
    uurpRoutingProfileId,
    uurpUserId,
    uurpInstanceId,

    -- * Destructuring the response
    UpdateUserRoutingProfileResponse (..),
    mkUpdateUserRoutingProfileResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserRoutingProfile' smart constructor.
data UpdateUserRoutingProfile = UpdateUserRoutingProfile'
  { routingProfileId ::
      Lude.Text,
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

-- | Creates a value of 'UpdateUserRoutingProfile' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'routingProfileId' - The identifier of the routing profile for the user.
-- * 'userId' - The identifier of the user account.
mkUpdateUserRoutingProfile ::
  -- | 'routingProfileId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  UpdateUserRoutingProfile
mkUpdateUserRoutingProfile pRoutingProfileId_ pUserId_ pInstanceId_ =
  UpdateUserRoutingProfile'
    { routingProfileId = pRoutingProfileId_,
      userId = pUserId_,
      instanceId = pInstanceId_
    }

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpRoutingProfileId :: Lens.Lens' UpdateUserRoutingProfile Lude.Text
uurpRoutingProfileId = Lens.lens (routingProfileId :: UpdateUserRoutingProfile -> Lude.Text) (\s a -> s {routingProfileId = a} :: UpdateUserRoutingProfile)
{-# DEPRECATED uurpRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpUserId :: Lens.Lens' UpdateUserRoutingProfile Lude.Text
uurpUserId = Lens.lens (userId :: UpdateUserRoutingProfile -> Lude.Text) (\s a -> s {userId = a} :: UpdateUserRoutingProfile)
{-# DEPRECATED uurpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpInstanceId :: Lens.Lens' UpdateUserRoutingProfile Lude.Text
uurpInstanceId = Lens.lens (instanceId :: UpdateUserRoutingProfile -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserRoutingProfile)
{-# DEPRECATED uurpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest UpdateUserRoutingProfile where
  type Rs UpdateUserRoutingProfile = UpdateUserRoutingProfileResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserRoutingProfileResponse'

instance Lude.ToHeaders UpdateUserRoutingProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserRoutingProfile where
  toJSON UpdateUserRoutingProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RoutingProfileId" Lude..= routingProfileId)]
      )

instance Lude.ToPath UpdateUserRoutingProfile where
  toPath UpdateUserRoutingProfile' {..} =
    Lude.mconcat
      [ "/users/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS userId,
        "/routing-profile"
      ]

instance Lude.ToQuery UpdateUserRoutingProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserRoutingProfileResponse' smart constructor.
data UpdateUserRoutingProfileResponse = UpdateUserRoutingProfileResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserRoutingProfileResponse' with the minimum fields required to make a request.
mkUpdateUserRoutingProfileResponse ::
  UpdateUserRoutingProfileResponse
mkUpdateUserRoutingProfileResponse =
  UpdateUserRoutingProfileResponse'
