{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates mappings.
module Network.AWS.Glue.GetMapping
  ( -- * Creating a request
    GetMapping (..),
    mkGetMapping,

    -- ** Request lenses
    gmSource,
    gmLocation,
    gmSinks,

    -- * Destructuring the response
    GetMappingResponse (..),
    mkGetMappingResponse,

    -- ** Response lenses
    gmrrsMapping,
    gmrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMapping' smart constructor.
data GetMapping = GetMapping'
  { -- | Specifies the source table.
    source :: Types.CatalogEntry,
    -- | Parameters for the mapping.
    location :: Core.Maybe Types.Location,
    -- | A list of target tables.
    sinks :: Core.Maybe [Types.CatalogEntry]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMapping' value with any optional fields omitted.
mkGetMapping ::
  -- | 'source'
  Types.CatalogEntry ->
  GetMapping
mkGetMapping source =
  GetMapping'
    { source,
      location = Core.Nothing,
      sinks = Core.Nothing
    }

-- | Specifies the source table.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSource :: Lens.Lens' GetMapping Types.CatalogEntry
gmSource = Lens.field @"source"
{-# DEPRECATED gmSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Parameters for the mapping.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmLocation :: Lens.Lens' GetMapping (Core.Maybe Types.Location)
gmLocation = Lens.field @"location"
{-# DEPRECATED gmLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A list of target tables.
--
-- /Note:/ Consider using 'sinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmSinks :: Lens.Lens' GetMapping (Core.Maybe [Types.CatalogEntry])
gmSinks = Lens.field @"sinks"
{-# DEPRECATED gmSinks "Use generic-lens or generic-optics with 'sinks' instead." #-}

instance Core.FromJSON GetMapping where
  toJSON GetMapping {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Source" Core..= source),
            ("Location" Core..=) Core.<$> location,
            ("Sinks" Core..=) Core.<$> sinks
          ]
      )

instance Core.AWSRequest GetMapping where
  type Rs GetMapping = GetMappingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetMapping")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMappingResponse'
            Core.<$> (x Core..:? "Mapping" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMappingResponse' smart constructor.
data GetMappingResponse = GetMappingResponse'
  { -- | A list of mappings to the specified targets.
    mapping :: [Types.MappingEntry],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMappingResponse' value with any optional fields omitted.
mkGetMappingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMappingResponse
mkGetMappingResponse responseStatus =
  GetMappingResponse' {mapping = Core.mempty, responseStatus}

-- | A list of mappings to the specified targets.
--
-- /Note:/ Consider using 'mapping' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsMapping :: Lens.Lens' GetMappingResponse [Types.MappingEntry]
gmrrsMapping = Lens.field @"mapping"
{-# DEPRECATED gmrrsMapping "Use generic-lens or generic-optics with 'mapping' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsResponseStatus :: Lens.Lens' GetMappingResponse Core.Int
gmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
