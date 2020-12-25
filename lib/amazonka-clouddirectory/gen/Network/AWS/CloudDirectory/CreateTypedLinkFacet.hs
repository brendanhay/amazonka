{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateTypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.CreateTypedLinkFacet
  ( -- * Creating a request
    CreateTypedLinkFacet (..),
    mkCreateTypedLinkFacet,

    -- ** Request lenses
    ctlfSchemaArn,
    ctlfFacet,

    -- * Destructuring the response
    CreateTypedLinkFacetResponse (..),
    mkCreateTypedLinkFacetResponse,

    -- ** Response lenses
    ctlfrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTypedLinkFacet' smart constructor.
data CreateTypedLinkFacet = CreateTypedLinkFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaArn :: Types.SchemaArn,
    -- | 'Facet' structure that is associated with the typed link facet.
    facet :: Types.TypedLinkFacet
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTypedLinkFacet' value with any optional fields omitted.
mkCreateTypedLinkFacet ::
  -- | 'schemaArn'
  Types.SchemaArn ->
  -- | 'facet'
  Types.TypedLinkFacet ->
  CreateTypedLinkFacet
mkCreateTypedLinkFacet schemaArn facet =
  CreateTypedLinkFacet' {schemaArn, facet}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfSchemaArn :: Lens.Lens' CreateTypedLinkFacet Types.SchemaArn
ctlfSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED ctlfSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | 'Facet' structure that is associated with the typed link facet.
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfFacet :: Lens.Lens' CreateTypedLinkFacet Types.TypedLinkFacet
ctlfFacet = Lens.field @"facet"
{-# DEPRECATED ctlfFacet "Use generic-lens or generic-optics with 'facet' instead." #-}

instance Core.FromJSON CreateTypedLinkFacet where
  toJSON CreateTypedLinkFacet {..} =
    Core.object (Core.catMaybes [Core.Just ("Facet" Core..= facet)])

instance Core.AWSRequest CreateTypedLinkFacet where
  type Rs CreateTypedLinkFacet = CreateTypedLinkFacetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            "/amazonclouddirectory/2017-01-11/typedlink/facet/create",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "x-amz-data-partition" schemaArn,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTypedLinkFacetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTypedLinkFacetResponse' smart constructor.
newtype CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTypedLinkFacetResponse' value with any optional fields omitted.
mkCreateTypedLinkFacetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTypedLinkFacetResponse
mkCreateTypedLinkFacetResponse responseStatus =
  CreateTypedLinkFacetResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfrrsResponseStatus :: Lens.Lens' CreateTypedLinkFacetResponse Core.Int
ctlfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctlfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
