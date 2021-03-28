{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ResumeGameServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__ 
--
-- Reinstates activity on a game server group after it has been suspended. A game server group might be suspended by the'SuspendGameServerGroup' operation, or it might be suspended involuntarily due to a configuration problem. In the second case, you can manually resume activity on the group once the configuration problem has been resolved. Refer to the game server group status and status reason for more information on why group activity is suspended.
-- To resume activity, specify a game server group ARN and the type of activity to be resumed. If successful, a 'GameServerGroup' object is returned showing that the resumed activity is no longer listed in @SuspendedActions@ . 
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide> 
-- __Related operations__ 
--
--     * 'CreateGameServerGroup' 
--
--
--     * 'ListGameServerGroups' 
--
--
--     * 'DescribeGameServerGroup' 
--
--
--     * 'UpdateGameServerGroup' 
--
--
--     * 'DeleteGameServerGroup' 
--
--
--     * 'ResumeGameServerGroup' 
--
--
--     * 'SuspendGameServerGroup' 
--
--
--     * 'DescribeGameServerInstances' 
--
--
module Network.AWS.GameLift.ResumeGameServerGroup
    (
    -- * Creating a request
      ResumeGameServerGroup (..)
    , mkResumeGameServerGroup
    -- ** Request lenses
    , rgsgGameServerGroupName
    , rgsgResumeActions

    -- * Destructuring the response
    , ResumeGameServerGroupResponse (..)
    , mkResumeGameServerGroupResponse
    -- ** Response lenses
    , rgsgrrsGameServerGroup
    , rgsgrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkResumeGameServerGroup' smart constructor.
data ResumeGameServerGroup = ResumeGameServerGroup'
  { gameServerGroupName :: Types.GameServerGroupName
    -- ^ A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
  , resumeActions :: Core.NonEmpty Types.GameServerGroupAction
    -- ^ The activity to resume for this game server group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeGameServerGroup' value with any optional fields omitted.
mkResumeGameServerGroup
    :: Types.GameServerGroupName -- ^ 'gameServerGroupName'
    -> Core.NonEmpty Types.GameServerGroupAction -- ^ 'resumeActions'
    -> ResumeGameServerGroup
mkResumeGameServerGroup gameServerGroupName resumeActions
  = ResumeGameServerGroup'{gameServerGroupName, resumeActions}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgGameServerGroupName :: Lens.Lens' ResumeGameServerGroup Types.GameServerGroupName
rgsgGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE rgsgGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | The activity to resume for this game server group.
--
-- /Note:/ Consider using 'resumeActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgResumeActions :: Lens.Lens' ResumeGameServerGroup (Core.NonEmpty Types.GameServerGroupAction)
rgsgResumeActions = Lens.field @"resumeActions"
{-# INLINEABLE rgsgResumeActions #-}
{-# DEPRECATED resumeActions "Use generic-lens or generic-optics with 'resumeActions' instead"  #-}

instance Core.ToQuery ResumeGameServerGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ResumeGameServerGroup where
        toHeaders ResumeGameServerGroup{..}
          = Core.pure ("X-Amz-Target", "GameLift.ResumeGameServerGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ResumeGameServerGroup where
        toJSON ResumeGameServerGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
                  Core.Just ("ResumeActions" Core..= resumeActions)])

instance Core.AWSRequest ResumeGameServerGroup where
        type Rs ResumeGameServerGroup = ResumeGameServerGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ResumeGameServerGroupResponse' Core.<$>
                   (x Core..:? "GameServerGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkResumeGameServerGroupResponse' smart constructor.
data ResumeGameServerGroupResponse = ResumeGameServerGroupResponse'
  { gameServerGroup :: Core.Maybe Types.GameServerGroup
    -- ^ An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResumeGameServerGroupResponse' value with any optional fields omitted.
mkResumeGameServerGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ResumeGameServerGroupResponse
mkResumeGameServerGroupResponse responseStatus
  = ResumeGameServerGroupResponse'{gameServerGroup = Core.Nothing,
                                   responseStatus}

-- | An object that describes the game server group resource, with the @SuspendedActions@ property updated to reflect the resumed activity.
--
-- /Note:/ Consider using 'gameServerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgrrsGameServerGroup :: Lens.Lens' ResumeGameServerGroupResponse (Core.Maybe Types.GameServerGroup)
rgsgrrsGameServerGroup = Lens.field @"gameServerGroup"
{-# INLINEABLE rgsgrrsGameServerGroup #-}
{-# DEPRECATED gameServerGroup "Use generic-lens or generic-optics with 'gameServerGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgsgrrsResponseStatus :: Lens.Lens' ResumeGameServerGroupResponse Core.Int
rgsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rgsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
