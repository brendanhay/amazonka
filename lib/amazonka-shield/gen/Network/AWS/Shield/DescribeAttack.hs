{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeAttack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a DDoS attack. 
module Network.AWS.Shield.DescribeAttack
    (
    -- * Creating a request
      DescribeAttack (..)
    , mkDescribeAttack
    -- ** Request lenses
    , daAttackId

    -- * Destructuring the response
    , DescribeAttackResponse (..)
    , mkDescribeAttackResponse
    -- ** Response lenses
    , darrsAttack
    , darrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeAttack' smart constructor.
newtype DescribeAttack = DescribeAttack'
  { attackId :: Types.AttackId
    -- ^ The unique identifier (ID) for the attack that to be described.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAttack' value with any optional fields omitted.
mkDescribeAttack
    :: Types.AttackId -- ^ 'attackId'
    -> DescribeAttack
mkDescribeAttack attackId = DescribeAttack'{attackId}

-- | The unique identifier (ID) for the attack that to be described.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttackId :: Lens.Lens' DescribeAttack Types.AttackId
daAttackId = Lens.field @"attackId"
{-# INLINEABLE daAttackId #-}
{-# DEPRECATED attackId "Use generic-lens or generic-optics with 'attackId' instead"  #-}

instance Core.ToQuery DescribeAttack where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAttack where
        toHeaders DescribeAttack{..}
          = Core.pure ("X-Amz-Target", "AWSShield_20160616.DescribeAttack")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAttack where
        toJSON DescribeAttack{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AttackId" Core..= attackId)])

instance Core.AWSRequest DescribeAttack where
        type Rs DescribeAttack = DescribeAttackResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAttackResponse' Core.<$>
                   (x Core..:? "Attack") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAttackResponse' smart constructor.
data DescribeAttackResponse = DescribeAttackResponse'
  { attack :: Core.Maybe Types.AttackDetail
    -- ^ The attack that is described.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAttackResponse' value with any optional fields omitted.
mkDescribeAttackResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAttackResponse
mkDescribeAttackResponse responseStatus
  = DescribeAttackResponse'{attack = Core.Nothing, responseStatus}

-- | The attack that is described.
--
-- /Note:/ Consider using 'attack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAttack :: Lens.Lens' DescribeAttackResponse (Core.Maybe Types.AttackDetail)
darrsAttack = Lens.field @"attack"
{-# INLINEABLE darrsAttack #-}
{-# DEPRECATED attack "Use generic-lens or generic-optics with 'attack' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAttackResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE darrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
