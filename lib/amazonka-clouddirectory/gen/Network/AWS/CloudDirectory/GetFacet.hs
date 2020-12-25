{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of the 'Facet' , such as facet name, attributes, 'Rule' s, or @ObjectType@ . You can call this on all kinds of schema facets -- published, development, or applied.
module Network.AWS.CloudDirectory.GetFacet
  ( -- * Creating a request
    GetFacet (..),
    mkGetFacet,

    -- ** Request lenses
    gfSchemaArn,
    gfName,

    -- * Destructuring the response
    GetFacetResponse (..),
    mkGetFacetResponse,

    -- ** Response lenses
    gfrrsFacet,
    gfrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFacet' smart constructor.
data GetFacet = GetFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
    schemaArn :: Types.SchemaArn,
    -- | The name of the facet to retrieve.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFacet' value with any optional fields omitted.
mkGetFacet ::
  -- | 'schemaArn'
  Types.SchemaArn ->
  -- | 'name'
  Types.Name ->
  GetFacet
mkGetFacet schemaArn name = GetFacet' {schemaArn, name}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfSchemaArn :: Lens.Lens' GetFacet Types.SchemaArn
gfSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED gfSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the facet to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfName :: Lens.Lens' GetFacet Types.Name
gfName = Lens.field @"name"
{-# DEPRECATED gfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetFacet where
  toJSON GetFacet {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetFacet where
  type Rs GetFacet = GetFacetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/facet",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFacetResponse'
            Core.<$> (x Core..:? "Facet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFacetResponse' smart constructor.
data GetFacetResponse = GetFacetResponse'
  { -- | The 'Facet' structure that is associated with the facet.
    facet :: Core.Maybe Types.Facet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFacetResponse' value with any optional fields omitted.
mkGetFacetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFacetResponse
mkGetFacetResponse responseStatus =
  GetFacetResponse' {facet = Core.Nothing, responseStatus}

-- | The 'Facet' structure that is associated with the facet.
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsFacet :: Lens.Lens' GetFacetResponse (Core.Maybe Types.Facet)
gfrrsFacet = Lens.field @"facet"
{-# DEPRECATED gfrrsFacet "Use generic-lens or generic-optics with 'facet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFacetResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
