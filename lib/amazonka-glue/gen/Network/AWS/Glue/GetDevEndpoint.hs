{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDevEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified development endpoint.
module Network.AWS.Glue.GetDevEndpoint
  ( -- * Creating a request
    GetDevEndpoint (..),
    mkGetDevEndpoint,

    -- ** Request lenses
    gdeEndpointName,

    -- * Destructuring the response
    GetDevEndpointResponse (..),
    mkGetDevEndpointResponse,

    -- ** Response lenses
    gderfrsDevEndpoint,
    gderfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDevEndpoint' smart constructor.
newtype GetDevEndpoint = GetDevEndpoint'
  { -- | Name of the @DevEndpoint@ to retrieve information for.
    endpointName :: Types.EndpointName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevEndpoint' value with any optional fields omitted.
mkGetDevEndpoint ::
  -- | 'endpointName'
  Types.EndpointName ->
  GetDevEndpoint
mkGetDevEndpoint endpointName = GetDevEndpoint' {endpointName}

-- | Name of the @DevEndpoint@ to retrieve information for.
--
-- /Note:/ Consider using 'endpointName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeEndpointName :: Lens.Lens' GetDevEndpoint Types.EndpointName
gdeEndpointName = Lens.field @"endpointName"
{-# DEPRECATED gdeEndpointName "Use generic-lens or generic-optics with 'endpointName' instead." #-}

instance Core.FromJSON GetDevEndpoint where
  toJSON GetDevEndpoint {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EndpointName" Core..= endpointName)])

instance Core.AWSRequest GetDevEndpoint where
  type Rs GetDevEndpoint = GetDevEndpointResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetDevEndpoint")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointResponse'
            Core.<$> (x Core..:? "DevEndpoint") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDevEndpointResponse' smart constructor.
data GetDevEndpointResponse = GetDevEndpointResponse'
  { -- | A @DevEndpoint@ definition.
    devEndpoint :: Core.Maybe Types.DevEndpoint,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDevEndpointResponse' value with any optional fields omitted.
mkGetDevEndpointResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDevEndpointResponse
mkGetDevEndpointResponse responseStatus =
  GetDevEndpointResponse'
    { devEndpoint = Core.Nothing,
      responseStatus
    }

-- | A @DevEndpoint@ definition.
--
-- /Note:/ Consider using 'devEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderfrsDevEndpoint :: Lens.Lens' GetDevEndpointResponse (Core.Maybe Types.DevEndpoint)
gderfrsDevEndpoint = Lens.field @"devEndpoint"
{-# DEPRECATED gderfrsDevEndpoint "Use generic-lens or generic-optics with 'devEndpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderfrsResponseStatus :: Lens.Lens' GetDevEndpointResponse Core.Int
gderfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gderfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
