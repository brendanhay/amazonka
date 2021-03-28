{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes room skill parameter details by room, skill, and parameter key ID.
module Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
    (
    -- * Creating a request
      DeleteRoomSkillParameter (..)
    , mkDeleteRoomSkillParameter
    -- ** Request lenses
    , drspSkillId
    , drspParameterKey
    , drspRoomArn

    -- * Destructuring the response
    , DeleteRoomSkillParameterResponse (..)
    , mkDeleteRoomSkillParameterResponse
    -- ** Response lenses
    , drsprrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRoomSkillParameter' smart constructor.
data DeleteRoomSkillParameter = DeleteRoomSkillParameter'
  { skillId :: Types.SkillId
    -- ^ The ID of the skill from which to remove the room skill parameter details.
  , parameterKey :: Types.ParameterKey
    -- ^ The room skill parameter key for which to remove details.
  , roomArn :: Core.Maybe Types.Arn
    -- ^ The ARN of the room from which to remove the room skill parameter details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoomSkillParameter' value with any optional fields omitted.
mkDeleteRoomSkillParameter
    :: Types.SkillId -- ^ 'skillId'
    -> Types.ParameterKey -- ^ 'parameterKey'
    -> DeleteRoomSkillParameter
mkDeleteRoomSkillParameter skillId parameterKey
  = DeleteRoomSkillParameter'{skillId, parameterKey,
                              roomArn = Core.Nothing}

-- | The ID of the skill from which to remove the room skill parameter details.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drspSkillId :: Lens.Lens' DeleteRoomSkillParameter Types.SkillId
drspSkillId = Lens.field @"skillId"
{-# INLINEABLE drspSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

-- | The room skill parameter key for which to remove details.
--
-- /Note:/ Consider using 'parameterKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drspParameterKey :: Lens.Lens' DeleteRoomSkillParameter Types.ParameterKey
drspParameterKey = Lens.field @"parameterKey"
{-# INLINEABLE drspParameterKey #-}
{-# DEPRECATED parameterKey "Use generic-lens or generic-optics with 'parameterKey' instead"  #-}

-- | The ARN of the room from which to remove the room skill parameter details.
--
-- /Note:/ Consider using 'roomArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drspRoomArn :: Lens.Lens' DeleteRoomSkillParameter (Core.Maybe Types.Arn)
drspRoomArn = Lens.field @"roomArn"
{-# INLINEABLE drspRoomArn #-}
{-# DEPRECATED roomArn "Use generic-lens or generic-optics with 'roomArn' instead"  #-}

instance Core.ToQuery DeleteRoomSkillParameter where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteRoomSkillParameter where
        toHeaders DeleteRoomSkillParameter{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.DeleteRoomSkillParameter")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteRoomSkillParameter where
        toJSON DeleteRoomSkillParameter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SkillId" Core..= skillId),
                  Core.Just ("ParameterKey" Core..= parameterKey),
                  ("RoomArn" Core..=) Core.<$> roomArn])

instance Core.AWSRequest DeleteRoomSkillParameter where
        type Rs DeleteRoomSkillParameter = DeleteRoomSkillParameterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteRoomSkillParameterResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteRoomSkillParameterResponse' smart constructor.
newtype DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRoomSkillParameterResponse' value with any optional fields omitted.
mkDeleteRoomSkillParameterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteRoomSkillParameterResponse
mkDeleteRoomSkillParameterResponse responseStatus
  = DeleteRoomSkillParameterResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsprrsResponseStatus :: Lens.Lens' DeleteRoomSkillParameterResponse Core.Int
drsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
