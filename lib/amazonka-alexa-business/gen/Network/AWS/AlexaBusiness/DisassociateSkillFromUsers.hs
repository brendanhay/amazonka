{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill unavailable for enrolled users and prevents them from enabling it on their devices.
module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
    (
    -- * Creating a request
      DisassociateSkillFromUsers (..)
    , mkDisassociateSkillFromUsers
    -- ** Request lenses
    , dsfuSkillId

    -- * Destructuring the response
    , DisassociateSkillFromUsersResponse (..)
    , mkDisassociateSkillFromUsersResponse
    -- ** Response lenses
    , dsfurrsResponseStatus
    ) where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateSkillFromUsers' smart constructor.
newtype DisassociateSkillFromUsers = DisassociateSkillFromUsers'
  { skillId :: Types.SkillId
    -- ^ The private skill ID you want to make unavailable for enrolled users.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillFromUsers' value with any optional fields omitted.
mkDisassociateSkillFromUsers
    :: Types.SkillId -- ^ 'skillId'
    -> DisassociateSkillFromUsers
mkDisassociateSkillFromUsers skillId
  = DisassociateSkillFromUsers'{skillId}

-- | The private skill ID you want to make unavailable for enrolled users.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfuSkillId :: Lens.Lens' DisassociateSkillFromUsers Types.SkillId
dsfuSkillId = Lens.field @"skillId"
{-# INLINEABLE dsfuSkillId #-}
{-# DEPRECATED skillId "Use generic-lens or generic-optics with 'skillId' instead"  #-}

instance Core.ToQuery DisassociateSkillFromUsers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateSkillFromUsers where
        toHeaders DisassociateSkillFromUsers{..}
          = Core.pure
              ("X-Amz-Target", "AlexaForBusiness.DisassociateSkillFromUsers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateSkillFromUsers where
        toJSON DisassociateSkillFromUsers{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SkillId" Core..= skillId)])

instance Core.AWSRequest DisassociateSkillFromUsers where
        type Rs DisassociateSkillFromUsers =
             DisassociateSkillFromUsersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateSkillFromUsersResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateSkillFromUsersResponse' smart constructor.
newtype DisassociateSkillFromUsersResponse = DisassociateSkillFromUsersResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateSkillFromUsersResponse' value with any optional fields omitted.
mkDisassociateSkillFromUsersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateSkillFromUsersResponse
mkDisassociateSkillFromUsersResponse responseStatus
  = DisassociateSkillFromUsersResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfurrsResponseStatus :: Lens.Lens' DisassociateSkillFromUsersResponse Core.Int
dsfurrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsfurrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
