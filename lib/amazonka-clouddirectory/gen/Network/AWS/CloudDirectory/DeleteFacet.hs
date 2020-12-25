{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given 'Facet' . All attributes and 'Rule' s that are associated with the facet will be deleted. Only development schema facets are allowed deletion.
module Network.AWS.CloudDirectory.DeleteFacet
  ( -- * Creating a request
    DeleteFacet (..),
    mkDeleteFacet,

    -- ** Request lenses
    dfSchemaArn,
    dfName,

    -- * Destructuring the response
    DeleteFacetResponse (..),
    mkDeleteFacetResponse,

    -- ** Response lenses
    dfrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteFacet' smart constructor.
data DeleteFacet = DeleteFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
    schemaArn :: Types.Arn,
    -- | The name of the facet to delete.
    name :: Types.FacetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFacet' value with any optional fields omitted.
mkDeleteFacet ::
  -- | 'schemaArn'
  Types.Arn ->
  -- | 'name'
  Types.FacetName ->
  DeleteFacet
mkDeleteFacet schemaArn name = DeleteFacet' {schemaArn, name}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfSchemaArn :: Lens.Lens' DeleteFacet Types.Arn
dfSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED dfSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the facet to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DeleteFacet Types.FacetName
dfName = Lens.field @"name"
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteFacet where
  toJSON DeleteFacet {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteFacet where
  type Rs DeleteFacet = DeleteFacetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/facet/delete",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFacetResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteFacetResponse' smart constructor.
newtype DeleteFacetResponse = DeleteFacetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFacetResponse' value with any optional fields omitted.
mkDeleteFacetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteFacetResponse
mkDeleteFacetResponse responseStatus =
  DeleteFacetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrrsResponseStatus :: Lens.Lens' DeleteFacetResponse Core.Int
dfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
