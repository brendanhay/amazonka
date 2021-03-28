{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateUserRoutingProfile (..)
    , mkUpdateUserRoutingProfile
    -- ** Request lenses
    , uurpRoutingProfileId
    , uurpUserId
    , uurpInstanceId

    -- * Destructuring the response
    , UpdateUserRoutingProfileResponse (..)
    , mkUpdateUserRoutingProfileResponse
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserRoutingProfile' smart constructor.
data UpdateUserRoutingProfile = UpdateUserRoutingProfile'
  { routingProfileId :: Types.RoutingProfileId
    -- ^ The identifier of the routing profile for the user.
  , userId :: Types.UserId
    -- ^ The identifier of the user account.
  , instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserRoutingProfile' value with any optional fields omitted.
mkUpdateUserRoutingProfile
    :: Types.RoutingProfileId -- ^ 'routingProfileId'
    -> Types.UserId -- ^ 'userId'
    -> Types.InstanceId -- ^ 'instanceId'
    -> UpdateUserRoutingProfile
mkUpdateUserRoutingProfile routingProfileId userId instanceId
  = UpdateUserRoutingProfile'{routingProfileId, userId, instanceId}

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpRoutingProfileId :: Lens.Lens' UpdateUserRoutingProfile Types.RoutingProfileId
uurpRoutingProfileId = Lens.field @"routingProfileId"
{-# INLINEABLE uurpRoutingProfileId #-}
{-# DEPRECATED routingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead"  #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpUserId :: Lens.Lens' UpdateUserRoutingProfile Types.UserId
uurpUserId = Lens.field @"userId"
{-# INLINEABLE uurpUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uurpInstanceId :: Lens.Lens' UpdateUserRoutingProfile Types.InstanceId
uurpInstanceId = Lens.field @"instanceId"
{-# INLINEABLE uurpInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery UpdateUserRoutingProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserRoutingProfile where
        toHeaders UpdateUserRoutingProfile{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserRoutingProfile where
        toJSON UpdateUserRoutingProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("RoutingProfileId" Core..= routingProfileId)])

instance Core.AWSRequest UpdateUserRoutingProfile where
        type Rs UpdateUserRoutingProfile = UpdateUserRoutingProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/users/" Core.<> Core.toText instanceId Core.<> "/" Core.<>
                             Core.toText userId
                             Core.<> "/routing-profile",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull UpdateUserRoutingProfileResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserRoutingProfileResponse' smart constructor.
data UpdateUserRoutingProfileResponse = UpdateUserRoutingProfileResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserRoutingProfileResponse' value with any optional fields omitted.
mkUpdateUserRoutingProfileResponse
    :: UpdateUserRoutingProfileResponse
mkUpdateUserRoutingProfileResponse
  = UpdateUserRoutingProfileResponse'
