{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with a skill group.
module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
    (
    -- * Creating a request
      AssociateSkillWithSkillGroup (..)
    , mkAssociateSkillWithSkillGroup
    -- ** Request lenses
    , aswsgSkillId
    , aswsgSkillGroupArn

    -- * Destructuring the response
    , AssociateSkillWithSkillGroupResponse (..)
    , mkAssociateSkillWithSkillGroupResponse
    -- ** Response lenses
    , aswsgrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSkillWithSkillGroup' smart constructor.
data AssociateSkillWithSkillGroup = AssociateSkillWithSkillGroup'
  { skillId :: Types.SkillId
    -- ^ The unique identifier of the skill.
  , skillGroupArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the skill group to associate the skill to. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillWithSkillGroup' value with any optional fields omitted.
mkAssociateSkillWithSkillGroup
    :: Types.SkillId -- ^ 'skillId'
    -> AssociateSkillWithSkillGroup
mkAssociateSkillWithSkillGroup skillId
  = AssociateSkillWithSkillGroup'{skillId,
                                  skillGroupArn = Core.Nothing}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgSkillId :: Lens.Lens' AssociateSkillWithSkillGroup Types.SkillId
aswsgSkillId = Lens.field @"skillId"
{-# INLINEABLE aswsgSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

-- | The ARN of the skill group to associate the skill to. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgSkillGroupArn :: Lens.Lens' AssociateSkillWithSkillGroup (Core.Maybe Types.Arn)
aswsgSkillGroupArn = Lens.field @"skillGroupArn"
{-# INLINEABLE aswsgSkillGroupArn #-}
{-# DEPRECATED skillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead"  #-}

instance Core.ToQuery AssociateSkillWithSkillGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateSkillWithSkillGroup where
        toHeaders AssociateSkillWithSkillGroup{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.AssociateSkillWithSkillGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateSkillWithSkillGroup where
        toJSON AssociateSkillWithSkillGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SkillId" Core..= skillId),
                  ("SkillGroupArn" Core..=) Core.<$> skillGroupArn])

instance Core.AWSRequest AssociateSkillWithSkillGroup where
        type Rs AssociateSkillWithSkillGroup =
             AssociateSkillWithSkillGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateSkillWithSkillGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateSkillWithSkillGroupResponse' smart constructor.
newtype AssociateSkillWithSkillGroupResponse = AssociateSkillWithSkillGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSkillWithSkillGroupResponse' value with any optional fields omitted.
mkAssociateSkillWithSkillGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateSkillWithSkillGroupResponse
mkAssociateSkillWithSkillGroupResponse responseStatus
  = AssociateSkillWithSkillGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgrrsResponseStatus :: Lens.Lens' AssociateSkillWithSkillGroupResponse Core.Int
aswsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aswsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
