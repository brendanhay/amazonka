{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a third-party account from a skill.
module Network.AWS.AlexaBusiness.DeleteSkillAuthorization
    (
    -- * Creating a request
      DeleteSkillAuthorization (..)
    , mkDeleteSkillAuthorization
    -- ** Request lenses
    , dsaSkillId
    , dsaRoomArn

    -- * Destructuring the response
    , DeleteSkillAuthorizationResponse (..)
    , mkDeleteSkillAuthorizationResponse
    -- ** Response lenses
    , dsarrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSkillAuthorization' smart constructor.
data DeleteSkillAuthorization = DeleteSkillAuthorization'
  { skillId :: Types.SkillId
    -- ^ The unique identifier of a skill.
  , roomArn :: Core.Maybe Types.Arn
    -- ^ The room that the skill is authorized for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSkillAuthorization' value with any optional fields omitted.
mkDeleteSkillAuthorization
    :: Types.SkillId -- ^ 'skillId'
    -> DeleteSkillAuthorization
mkDeleteSkillAuthorization skillId
  = DeleteSkillAuthorization'{skillId, roomArn = Core.Nothing}

-- | The unique identifier of a skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaSkillId :: Lens.Lens' DeleteSkillAuthorization Types.SkillId
dsaSkillId = Lens.field @"skillId"
{-# INLINEABLE dsaSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

-- | The room that the skill is authorized for.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsaRoomArn :: Lens.Lens' DeleteSkillAuthorization (Core.Maybe Types.Arn)
dsaRoomArn = Lens.field @"roomArn"
{-# INLINEABLE dsaRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery DeleteSkillAuthorization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSkillAuthorization where
        toHeaders DeleteSkillAuthorization{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.DeleteSkillAuthorization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSkillAuthorization where
        toJSON DeleteSkillAuthorization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SkillId" Core..= skillId),
                  ("RoomArn" Core..=) Core.<$> roomArn])

instance Core.AWSRequest DeleteSkillAuthorization where
        type Rs DeleteSkillAuthorization = DeleteSkillAuthorizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteSkillAuthorizationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteSkillAuthorizationResponse' smart constructor.
newtype DeleteSkillAuthorizationResponse = DeleteSkillAuthorizationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSkillAuthorizationResponse' value with any optional fields omitted.
mkDeleteSkillAuthorizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSkillAuthorizationResponse
mkDeleteSkillAuthorizationResponse responseStatus
  = DeleteSkillAuthorizationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsarrsResponseStatus :: Lens.Lens' DeleteSkillAuthorizationResponse Core.Int
dsarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
