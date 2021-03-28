{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a skill group by skill group ARN.
module Network.AWS.AlexaBusiness.DeleteSkillGroup
    (
    -- * Creating a request
      DeleteSkillGroup (..)
    , mkDeleteSkillGroup
    -- ** Request lenses
    , dsgSkillGroupArn

    -- * Destructuring the response
    , DeleteSkillGroupResponse (..)
    , mkDeleteSkillGroupResponse
    -- ** Response lenses
    , dsgrrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSkillGroup' smart constructor.
newtype DeleteSkillGroup = DeleteSkillGroup'
  { skillGroupArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the skill group to delete. Required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSkillGroup' value with any optional fields omitted.
mkDeleteSkillGroup
    :: DeleteSkillGroup
mkDeleteSkillGroup
  = DeleteSkillGroup'{skillGroupArn = Core.Nothing}

-- | The ARN of the skill group to delete. Required.
--
-- /Note:/ Consider using 'skillGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgSkillGroupArn :: Lens.Lens' DeleteSkillGroup (Core.Maybe Types.Arn)
dsgSkillGroupArn = Lens.field @"skillGroupArn"
{-# INLINEABLE dsgSkillGroupArn #-}
{-# DEPRECATED skillGroupArn "Use generic-lens or generic-optics with 'skillGroupArn' instead"  #-}

instance Core.ToQuery DeleteSkillGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSkillGroup where
        toHeaders DeleteSkillGroup{..}
          = Core.pure ("X-Amz-Target", "AlexaForBusiness.DeleteSkillGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSkillGroup where
        toJSON DeleteSkillGroup{..}
          = Core.object
              (Core.catMaybes [("SkillGroupArn" Core..=) Core.<$> skillGroupArn])

instance Core.AWSRequest DeleteSkillGroup where
        type Rs DeleteSkillGroup = DeleteSkillGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteSkillGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSkillGroupResponse' smart constructor.
newtype DeleteSkillGroupResponse = DeleteSkillGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSkillGroupResponse' value with any optional fields omitted.
mkDeleteSkillGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSkillGroupResponse
mkDeleteSkillGroupResponse responseStatus
  = DeleteSkillGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsgrrsResponseStatus :: Lens.Lens' DeleteSkillGroupResponse Core.Int
dsgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
