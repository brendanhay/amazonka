{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.UpdateExpirationForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateExpirationForHIT@ operation allows you update the expiration time of a HIT. If you update it to a time in the past, the HIT will be immediately expired.
module Network.AWS.MechanicalTurk.UpdateExpirationForHIT
  ( -- * Creating a request
    UpdateExpirationForHIT (..),
    mkUpdateExpirationForHIT,

    -- ** Request lenses
    uefhitHITId,
    uefhitExpireAt,

    -- * Destructuring the response
    UpdateExpirationForHITResponse (..),
    mkUpdateExpirationForHITResponse,

    -- ** Response lenses
    uefhitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { -- | The HIT to update.
    hITId :: Types.HITId,
    -- | The date and time at which you want the HIT to expire
    expireAt :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateExpirationForHIT' value with any optional fields omitted.
mkUpdateExpirationForHIT ::
  -- | 'hITId'
  Types.HITId ->
  -- | 'expireAt'
  Core.NominalDiffTime ->
  UpdateExpirationForHIT
mkUpdateExpirationForHIT hITId expireAt =
  UpdateExpirationForHIT' {hITId, expireAt}

-- | The HIT to update.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitHITId :: Lens.Lens' UpdateExpirationForHIT Types.HITId
uefhitHITId = Lens.field @"hITId"
{-# DEPRECATED uefhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The date and time at which you want the HIT to expire
--
-- /Note:/ Consider using 'expireAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitExpireAt :: Lens.Lens' UpdateExpirationForHIT Core.NominalDiffTime
uefhitExpireAt = Lens.field @"expireAt"
{-# DEPRECATED uefhitExpireAt "Use generic-lens or generic-optics with 'expireAt' instead." #-}

instance Core.FromJSON UpdateExpirationForHIT where
  toJSON UpdateExpirationForHIT {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HITId" Core..= hITId),
            Core.Just ("ExpireAt" Core..= expireAt)
          ]
      )

instance Core.AWSRequest UpdateExpirationForHIT where
  type Rs UpdateExpirationForHIT = UpdateExpirationForHITResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.UpdateExpirationForHIT"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateExpirationForHITResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateExpirationForHITResponse' smart constructor.
newtype UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateExpirationForHITResponse' value with any optional fields omitted.
mkUpdateExpirationForHITResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateExpirationForHITResponse
mkUpdateExpirationForHITResponse responseStatus =
  UpdateExpirationForHITResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uefhitrrsResponseStatus :: Lens.Lens' UpdateExpirationForHITResponse Core.Int
uefhitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uefhitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
