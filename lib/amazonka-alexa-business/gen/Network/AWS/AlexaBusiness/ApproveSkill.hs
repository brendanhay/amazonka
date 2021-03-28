{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ApproveSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with the organization under the customer's AWS account. If a skill is private, the user implicitly accepts access to this skill during enablement.
module Network.AWS.AlexaBusiness.ApproveSkill
    (
    -- * Creating a request
      ApproveSkill (..)
    , mkApproveSkill
    -- ** Request lenses
    , asSkillId

    -- * Destructuring the response
    , ApproveSkillResponse (..)
    , mkApproveSkillResponse
    -- ** Response lenses
    , asrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkApproveSkill' smart constructor.
newtype ApproveSkill = ApproveSkill'
  { skillId :: Types.SkillId
    -- ^ The unique identifier of the skill.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ApproveSkill' value with any optional fields omitted.
mkApproveSkill
    :: Types.SkillId -- ^ 'skillId'
    -> ApproveSkill
mkApproveSkill skillId = ApproveSkill'{skillId}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSkillId :: Lens.Lens' ApproveSkill Types.SkillId
asSkillId = Lens.field @"skillId"
{-# INLINEABLE asSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

instance Core.ToQuery ApproveSkill where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ApproveSkill where
        toHeaders ApproveSkill{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.ApproveSkill")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ApproveSkill where
        toJSON ApproveSkill{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SkillId" Core..= skillId)])

instance Core.AWSRequest ApproveSkill where
        type Rs ApproveSkill = ApproveSkillResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ApproveSkillResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkApproveSkillResponse' smart constructor.
newtype ApproveSkillResponse = ApproveSkillResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ApproveSkillResponse' value with any optional fields omitted.
mkApproveSkillResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ApproveSkillResponse
mkApproveSkillResponse responseStatus
  = ApproveSkillResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrrsResponseStatus :: Lens.Lens' ApproveSkillResponse Core.Int
asrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
