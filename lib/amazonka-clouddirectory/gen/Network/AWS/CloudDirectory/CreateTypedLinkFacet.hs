{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTypedLinkFacet (..)
    , mkCreateTypedLinkFacet
    -- ** Request lenses
    , ctlfSchemaArn
    , ctlfFacet

    -- * Destructuring the response
    , CreateTypedLinkFacetResponse (..)
    , mkCreateTypedLinkFacetResponse
    -- ** Response lenses
    , ctlfrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTypedLinkFacet' smart constructor.
data CreateTypedLinkFacet = CreateTypedLinkFacet'
  { schemaArn :: Types.SchemaArn
    -- ^ The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
  , facet :: Types.TypedLinkFacet
    -- ^ 'Facet' structure that is associated with the typed link facet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTypedLinkFacet' value with any optional fields omitted.
mkCreateTypedLinkFacet
    :: Types.SchemaArn -- ^ 'schemaArn'
    -> Types.TypedLinkFacet -- ^ 'facet'
    -> CreateTypedLinkFacet
mkCreateTypedLinkFacet schemaArn facet
  = CreateTypedLinkFacet'{schemaArn, facet}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfSchemaArn :: Lens.Lens' CreateTypedLinkFacet Types.SchemaArn
ctlfSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE ctlfSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | 'Facet' structure that is associated with the typed link facet.
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfFacet :: Lens.Lens' CreateTypedLinkFacet Types.TypedLinkFacet
ctlfFacet = Lens.field @"facet"
{-# INLINEABLE ctlfFacet #-}
{-# DEPRECATED facet "Use generic-lens or generic-optics with 'facet' instead"  #-}

instance Core.ToQuery CreateTypedLinkFacet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTypedLinkFacet where
        toHeaders CreateTypedLinkFacet{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON CreateTypedLinkFacet where
        toJSON CreateTypedLinkFacet{..}
          = Core.object (Core.catMaybes [Core.Just ("Facet" Core..= facet)])

instance Core.AWSRequest CreateTypedLinkFacet where
        type Rs CreateTypedLinkFacet = CreateTypedLinkFacetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/typedlink/facet/create",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateTypedLinkFacetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTypedLinkFacetResponse' smart constructor.
newtype CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTypedLinkFacetResponse' value with any optional fields omitted.
mkCreateTypedLinkFacetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTypedLinkFacetResponse
mkCreateTypedLinkFacetResponse responseStatus
  = CreateTypedLinkFacetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfrrsResponseStatus :: Lens.Lens' CreateTypedLinkFacetResponse Core.Int
ctlfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctlfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
