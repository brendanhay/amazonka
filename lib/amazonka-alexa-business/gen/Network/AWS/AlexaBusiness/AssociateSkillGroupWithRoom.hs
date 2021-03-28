{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill group with a given room. This enables all skills in the associated skill group on all devices in the room.
module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
    (
    -- * Creating a request
      AssociateSkillGroupWithRoom (..)
    , mkAssociateSkillGroupWithRoom
    -- ** Request lenses
    , asgwrRoomArn
    , asgwrSkillGroupArn

    -- * Destructuring the response
    , AssociateSkillGroupWithRoomResponse (..)
    , mkAssociateSkillGroupWithRoomResponse
    -- ** Response lenses
    , asgwrrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSkillGroupWithRoom' smart constructor.
data AssociateSkillGroupWithRoom = AssociateSkillGroupWithRoom'
  { roomArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the room with which to associate the skill group. Required.
  , skillGroupArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the skill group to associate with a room. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillGroupWithRoom' value with any optional fields omitted.
mkAssociateSkillGroupWithRoom
    :: AssociateSkillGroupWithRoom
mkAssociateSkillGroupWithRoom
  = AssociateSkillGroupWithRoom'{roomArn = Core.Nothing,
                                 skillGroupArn = Core.Nothing}

-- | The ARN of the room with which to associate the skill group. Required.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgwrRoomArn :: Lens.Lens' AssociateSkillGroupWithRoom (Core.Maybe Types.Arn)
asgwrRoomArn = Lens.field @"roomArn"
{-# INLINEABLE asgwrRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

-- | The ARN of the skill group to associate with a room. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgwrSkillGroupArn :: Lens.Lens' AssociateSkillGroupWithRoom (Core.Maybe Types.Arn)
asgwrSkillGroupArn = Lens.field @"skillGroupArn"
{-# INLINEABLE asgwrSkillGroupArn #-}
{-# DEPRECATED skillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead"  #-}

instance Core.ToQuery AssociateSkillGroupWithRoom where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateSkillGroupWithRoom where
        toHeaders AssociateSkillGroupWithRoom{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.AssociateSkillGroupWithRoom")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateSkillGroupWithRoom where
        toJSON AssociateSkillGroupWithRoom{..}
          = Core.object
              (Core.catMaybes
                 [("RoomArn" Core..=) Core.<$> roomArn,
                  ("SkillGroupArn" Core..=) Core.<$> skillGroupArn])

instance Core.AWSRequest AssociateSkillGroupWithRoom where
        type Rs AssociateSkillGroupWithRoom =
             AssociateSkillGroupWithRoomResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateSkillGroupWithRoomResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateSkillGroupWithRoomResponse' smart constructor.
newtype AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillGroupWithRoomResponse' value with any optional fields omitted.
mkAssociateSkillGroupWithRoomResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateSkillGroupWithRoomResponse
mkAssociateSkillGroupWithRoomResponse responseStatus
  = AssociateSkillGroupWithRoomResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgwrrrsResponseStatus :: Lens.Lens' AssociateSkillGroupWithRoomResponse Core.Int
asgwrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asgwrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
