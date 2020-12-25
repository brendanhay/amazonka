{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserRoutingProfile' smart constructor.
data UpdateUserRoutingProfile = UpdateUserRoutingProfile'
  { -- | The identifier of the routing profile for the user.
    routingProfileId :: Types.RoutingProfileId,
    -- | The identifier of the user account.
    userId :: Types.UserId,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserRoutingProfile' value with any optional fields omitted.
mkUpdateUserRoutingProfile ::
  -- | 'routingProfileId'
  Types.RoutingProfileId ->
  -- | 'userId'
  Types.UserId ->
  -- | 'instanceId'
  Types.InstanceId ->
  UpdateUserRoutingProfile
mkUpdateUserRoutingProfile routingProfileId userId instanceId =
  UpdateUserRoutingProfile' {routingProfileId, userId, instanceId}

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpRoutingProfileId :: Lens.Lens' UpdateUserRoutingProfile Types.RoutingProfileId
uurpRoutingProfileId = Lens.field @"routingProfileId"
{-# DEPRECATED uurpRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpUserId :: Lens.Lens' UpdateUserRoutingProfile Types.UserId
uurpUserId = Lens.field @"userId"
{-# DEPRECATED uurpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpInstanceId :: Lens.Lens' UpdateUserRoutingProfile Types.InstanceId
uurpInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uurpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Core.FromJSON UpdateUserRoutingProfile where
  toJSON UpdateUserRoutingProfile {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RoutingProfileId" Core..= routingProfileId)]
      )

instance Core.AWSRequest UpdateUserRoutingProfile where
  type Rs UpdateUserRoutingProfile = UpdateUserRoutingProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/users/" Core.<> (Core.toText instanceId) Core.<> ("/")
                Core.<> (Core.toText userId)
                Core.<> ("/routing-profile")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateUserRoutingProfileResponse'

-- | /See:/ 'mkUpdateUserRoutingProfileResponse' smart constructor.
data UpdateUserRoutingProfileResponse = UpdateUserRoutingProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserRoutingProfileResponse' value with any optional fields omitted.
mkUpdateUserRoutingProfileResponse ::
  UpdateUserRoutingProfileResponse
mkUpdateUserRoutingProfileResponse =
  UpdateUserRoutingProfileResponse'
