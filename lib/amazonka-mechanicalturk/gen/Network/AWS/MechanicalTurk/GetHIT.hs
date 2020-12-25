{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetHIT@ operation retrieves the details of the specified HIT.
module Network.AWS.MechanicalTurk.GetHIT
  ( -- * Creating a request
    GetHIT (..),
    mkGetHIT,

    -- ** Request lenses
    ghitHITId,

    -- * Destructuring the response
    GetHITResponse (..),
    mkGetHITResponse,

    -- ** Response lenses
    ghitrrsHIT,
    ghitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetHIT' smart constructor.
newtype GetHIT = GetHIT'
  { -- | The ID of the HIT to be retrieved.
    hITId :: Types.HITId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetHIT' value with any optional fields omitted.
mkGetHIT ::
  -- | 'hITId'
  Types.HITId ->
  GetHIT
mkGetHIT hITId = GetHIT' {hITId}

-- | The ID of the HIT to be retrieved.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghitHITId :: Lens.Lens' GetHIT Types.HITId
ghitHITId = Lens.field @"hITId"
{-# DEPRECATED ghitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

instance Core.FromJSON GetHIT where
  toJSON GetHIT {..} =
    Core.object (Core.catMaybes [Core.Just ("HITId" Core..= hITId)])

instance Core.AWSRequest GetHIT where
  type Rs GetHIT = GetHITResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "MTurkRequesterServiceV20170117.GetHIT")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHITResponse'
            Core.<$> (x Core..:? "HIT") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetHITResponse' smart constructor.
data GetHITResponse = GetHITResponse'
  { -- | Contains the requested HIT data.
    hit :: Core.Maybe Types.HIT,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetHITResponse' value with any optional fields omitted.
mkGetHITResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetHITResponse
mkGetHITResponse responseStatus =
  GetHITResponse' {hit = Core.Nothing, responseStatus}

-- | Contains the requested HIT data.
--
-- /Note:/ Consider using 'hit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghitrrsHIT :: Lens.Lens' GetHITResponse (Core.Maybe Types.HIT)
ghitrrsHIT = Lens.field @"hit"
{-# DEPRECATED ghitrrsHIT "Use generic-lens or generic-optics with 'hit' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghitrrsResponseStatus :: Lens.Lens' GetHITResponse Core.Int
ghitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ghitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
