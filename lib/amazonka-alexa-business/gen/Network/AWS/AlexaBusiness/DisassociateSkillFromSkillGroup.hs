{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from a skill group.
module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
    (
    -- * Creating a request
      DisassociateSkillFromSkillGroup (..)
    , mkDisassociateSkillFromSkillGroup
    -- ** Request lenses
    , dsfsgSkillId
    , dsfsgSkillGroupArn

    -- * Destructuring the response
    , DisassociateSkillFromSkillGroupResponse (..)
    , mkDisassociateSkillFromSkillGroupResponse
    -- ** Response lenses
    , dsfsgrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSkillFromSkillGroup' smart constructor.
data DisassociateSkillFromSkillGroup = DisassociateSkillFromSkillGroup'
  { skillId :: Types.SkillId
    -- ^ The ARN of a skill group to associate to a skill.
  , skillGroupArn :: Core.Maybe Types.SkillGroupArn
    -- ^ The unique identifier of a skill. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillFromSkillGroup' value with any optional fields omitted.
mkDisassociateSkillFromSkillGroup
    :: Types.SkillId -- ^ 'skillId'
    -> DisassociateSkillFromSkillGroup
mkDisassociateSkillFromSkillGroup skillId
  = DisassociateSkillFromSkillGroup'{skillId,
                                     skillGroupArn = Core.Nothing}

-- | The ARN of a skill group to associate to a skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfsgSkillId :: Lens.Lens' DisassociateSkillFromSkillGroup Types.SkillId
dsfsgSkillId = Lens.field @"skillId"
{-# INLINEABLE dsfsgSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

-- | The unique identifier of a skill. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfsgSkillGroupArn :: Lens.Lens' DisassociateSkillFromSkillGroup (Core.Maybe Types.SkillGroupArn)
dsfsgSkillGroupArn = Lens.field @"skillGroupArn"
{-# INLINEABLE dsfsgSkillGroupArn #-}
{-# DEPRECATED skillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead"  #-}

instance Core.ToQuery DisassociateSkillFromSkillGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateSkillFromSkillGroup where
        toHeaders DisassociateSkillFromSkillGroup{..}
          = Core.pure
              ("X-Amz-Target",
               "AlexaForBusiness.DisassociateSkillFromSkillGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateSkillFromSkillGroup where
        toJSON DisassociateSkillFromSkillGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SkillId" Core..= skillId),
                  ("SkillGroupArn" Core..=) Core.<$> skillGroupArn])

instance Core.AWSRequest DisassociateSkillFromSkillGroup where
        type Rs DisassociateSkillFromSkillGroup =
             DisassociateSkillFromSkillGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateSkillFromSkillGroupResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateSkillFromSkillGroupResponse' smart constructor.
newtype DisassociateSkillFromSkillGroupResponse = DisassociateSkillFromSkillGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillFromSkillGroupResponse' value with any optional fields omitted.
mkDisassociateSkillFromSkillGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateSkillFromSkillGroupResponse
mkDisassociateSkillFromSkillGroupResponse responseStatus
  = DisassociateSkillFromSkillGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfsgrrsResponseStatus :: Lens.Lens' DisassociateSkillFromSkillGroupResponse Core.Int
dsfsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsfsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
