{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns settings information for a specified trail.
module Network.AWS.CloudTrail.GetTrail
  ( -- * Creating a request
    GetTrail (..),
    mkGetTrail,

    -- ** Request lenses
    gtName,

    -- * Destructuring the response
    GetTrailResponse (..),
    mkGetTrailResponse,

    -- ** Response lenses
    gtrrsTrail,
    gtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetTrail' smart constructor.
newtype GetTrail = GetTrail'
  { -- | The name or the Amazon Resource Name (ARN) of the trail for which you want to retrieve settings information.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrail' value with any optional fields omitted.
mkGetTrail ::
  -- | 'name'
  Types.Name ->
  GetTrail
mkGetTrail name = GetTrail' {name}

-- | The name or the Amazon Resource Name (ARN) of the trail for which you want to retrieve settings information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTrail Types.Name
gtName = Lens.field @"name"
{-# DEPRECATED gtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetTrail where
  toJSON GetTrail {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetTrail where
  type Rs GetTrail = GetTrailResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrail"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrailResponse'
            Core.<$> (x Core..:? "Trail") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetTrailResponse' smart constructor.
data GetTrailResponse = GetTrailResponse'
  { trail :: Core.Maybe Types.Trail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTrailResponse' value with any optional fields omitted.
mkGetTrailResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTrailResponse
mkGetTrailResponse responseStatus =
  GetTrailResponse' {trail = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsTrail :: Lens.Lens' GetTrailResponse (Core.Maybe Types.Trail)
gtrrsTrail = Lens.field @"trail"
{-# DEPRECATED gtrrsTrail "Use generic-lens or generic-optics with 'trail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrrsResponseStatus :: Lens.Lens' GetTrailResponse Core.Int
gtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
