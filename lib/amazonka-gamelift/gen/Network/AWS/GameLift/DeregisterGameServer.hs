{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeregisterGameServer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__ 
--
-- Removes the game server from a game server group. As a result of this operation, the deregistered game server can no longer be claimed and will not be returned in a list of active game servers. 
-- To deregister a game server, specify the game server group and game server ID. If successful, this operation emits a CloudWatch event with termination timestamp and reason.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide> 
-- __Related operations__ 
--
--     * 'RegisterGameServer' 
--
--
--     * 'ListGameServers' 
--
--
--     * 'ClaimGameServer' 
--
--
--     * 'DescribeGameServer' 
--
--
--     * 'UpdateGameServer' 
--
--
--     * 'DeregisterGameServer' 
--
--
module Network.AWS.GameLift.DeregisterGameServer
    (
    -- * Creating a request
      DeregisterGameServer (..)
    , mkDeregisterGameServer
    -- ** Request lenses
    , dgsGameServerGroupName
    , dgsGameServerId

    -- * Destructuring the response
    , DeregisterGameServerResponse (..)
    , mkDeregisterGameServerResponse
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterGameServer' smart constructor.
data DeregisterGameServer = DeregisterGameServer'
  { gameServerGroupName :: Types.GameServerGroupNameOrArn
    -- ^ A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
  , gameServerId :: Types.GameServerId
    -- ^ A custom string that uniquely identifies the game server to deregister.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterGameServer' value with any optional fields omitted.
mkDeregisterGameServer
    :: Types.GameServerGroupNameOrArn -- ^ 'gameServerGroupName'
    -> Types.GameServerId -- ^ 'gameServerId'
    -> DeregisterGameServer
mkDeregisterGameServer gameServerGroupName gameServerId
  = DeregisterGameServer'{gameServerGroupName, gameServerId}

-- | A unique identifier for the game server group where the game server is running. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsGameServerGroupName :: Lens.Lens' DeregisterGameServer Types.GameServerGroupNameOrArn
dgsGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE dgsGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | A custom string that uniquely identifies the game server to deregister.
--
-- /Note:/ Consider using 'gameServerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsGameServerId :: Lens.Lens' DeregisterGameServer Types.GameServerId
dgsGameServerId = Lens.field @"gameServerId"
{-# INLINEABLE dgsGameServerId #-}
{-# DEPRECATED gameServerId "Use generic-lens or generic-optics with 'gameServerId' instead"  #-}

instance Core.ToQuery DeregisterGameServer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterGameServer where
        toHeaders DeregisterGameServer{..}
          = Core.pure ("X-Amz-Target", "GameLift.DeregisterGameServer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterGameServer where
        toJSON DeregisterGameServer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
                  Core.Just ("GameServerId" Core..= gameServerId)])

instance Core.AWSRequest DeregisterGameServer where
        type Rs DeregisterGameServer = DeregisterGameServerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeregisterGameServerResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterGameServerResponse' smart constructor.
data DeregisterGameServerResponse = DeregisterGameServerResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterGameServerResponse' value with any optional fields omitted.
mkDeregisterGameServerResponse
    :: DeregisterGameServerResponse
mkDeregisterGameServerResponse = DeregisterGameServerResponse'
