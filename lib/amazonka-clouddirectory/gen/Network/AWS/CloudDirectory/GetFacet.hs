{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetFacet (..)
    , mkGetFacet
    -- ** Request lenses
    , gfSchemaArn
    , gfName

    -- * Destructuring the response
    , GetFacetResponse (..)
    , mkGetFacetResponse
    -- ** Response lenses
    , gfrrsFacet
    , gfrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFacet' smart constructor.
data GetFacet = GetFacet'
  { schemaArn :: Types.SchemaArn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
  , name :: Types.Name
    -- ^ The name of the facet to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFacet' value with any optional fields omitted.
mkGetFacet
    :: Types.SchemaArn -- ^ 'schemaArn'
    -> Types.Name -- ^ 'name'
    -> GetFacet
mkGetFacet schemaArn name = GetFacet'{schemaArn, name}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfSchemaArn :: Lens.Lens' GetFacet Types.SchemaArn
gfSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE gfSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The name of the facet to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfName :: Lens.Lens' GetFacet Types.Name
gfName = Lens.field @"name"
{-# INLINEABLE gfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetFacet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFacet where
        toHeaders GetFacet{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON GetFacet where
        toJSON GetFacet{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetFacet where
        type Rs GetFacet = GetFacetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/facet",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFacetResponse' Core.<$>
                   (x Core..:? "Facet") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFacetResponse' smart constructor.
data GetFacetResponse = GetFacetResponse'
  { facet :: Core.Maybe Types.Facet
    -- ^ The 'Facet' structure that is associated with the facet.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFacetResponse' value with any optional fields omitted.
mkGetFacetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFacetResponse
mkGetFacetResponse responseStatus
  = GetFacetResponse'{facet = Core.Nothing, responseStatus}

-- | The 'Facet' structure that is associated with the facet.
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsFacet :: Lens.Lens' GetFacetResponse (Core.Maybe Types.Facet)
gfrrsFacet = Lens.field @"facet"
{-# INLINEABLE gfrrsFacet #-}
{-# DEPRECATED facet "Use generic-lens or generic-optics with 'facet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFacetResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
