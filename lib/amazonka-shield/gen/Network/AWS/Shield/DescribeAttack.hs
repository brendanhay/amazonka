{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeAttack (..),
    mkDescribeAttack,

    -- ** Request lenses
    daAttackId,

    -- * Destructuring the response
    DescribeAttackResponse (..),
    mkDescribeAttackResponse,

    -- ** Response lenses
    darrsAttack,
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeAttack' smart constructor.
newtype DescribeAttack = DescribeAttack'
  { -- | The unique identifier (ID) for the attack that to be described.
    attackId :: Types.AttackId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAttack' value with any optional fields omitted.
mkDescribeAttack ::
  -- | 'attackId'
  Types.AttackId ->
  DescribeAttack
mkDescribeAttack attackId = DescribeAttack' {attackId}

-- | The unique identifier (ID) for the attack that to be described.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttackId :: Lens.Lens' DescribeAttack Types.AttackId
daAttackId = Lens.field @"attackId"
{-# DEPRECATED daAttackId "Use generic-lens or generic-optics with 'attackId' instead." #-}

instance Core.FromJSON DescribeAttack where
  toJSON DescribeAttack {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AttackId" Core..= attackId)])

instance Core.AWSRequest DescribeAttack where
  type Rs DescribeAttack = DescribeAttackResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSShield_20160616.DescribeAttack")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAttackResponse'
            Core.<$> (x Core..:? "Attack") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAttackResponse' smart constructor.
data DescribeAttackResponse = DescribeAttackResponse'
  { -- | The attack that is described.
    attack :: Core.Maybe Types.AttackDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAttackResponse' value with any optional fields omitted.
mkDescribeAttackResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAttackResponse
mkDescribeAttackResponse responseStatus =
  DescribeAttackResponse' {attack = Core.Nothing, responseStatus}

-- | The attack that is described.
--
-- /Note:/ Consider using 'attack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsAttack :: Lens.Lens' DescribeAttackResponse (Core.Maybe Types.AttackDetail)
darrsAttack = Lens.field @"attack"
{-# DEPRECATED darrsAttack "Use generic-lens or generic-optics with 'attack' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DescribeAttackResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
