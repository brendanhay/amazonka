{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.RemoveFacetFromObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified facet from the specified object.
module Network.AWS.CloudDirectory.RemoveFacetFromObject
    (
    -- * Creating a request
      RemoveFacetFromObject (..)
    , mkRemoveFacetFromObject
    -- ** Request lenses
    , rffoDirectoryArn
    , rffoSchemaFacet
    , rffoObjectReference

    -- * Destructuring the response
    , RemoveFacetFromObjectResponse (..)
    , mkRemoveFacetFromObjectResponse
    -- ** Response lenses
    , rfforrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveFacetFromObject' smart constructor.
data RemoveFacetFromObject = RemoveFacetFromObject'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the directory in which the object resides.
  , schemaFacet :: Types.SchemaFacet
    -- ^ The facet to remove. See 'SchemaFacet' for details.
  , objectReference :: Types.ObjectReference
    -- ^ A reference to the object to remove the facet from.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveFacetFromObject' value with any optional fields omitted.
mkRemoveFacetFromObject
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.SchemaFacet -- ^ 'schemaFacet'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> RemoveFacetFromObject
mkRemoveFacetFromObject directoryArn schemaFacet objectReference
  = RemoveFacetFromObject'{directoryArn, schemaFacet,
                           objectReference}

-- | The ARN of the directory in which the object resides.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rffoDirectoryArn :: Lens.Lens' RemoveFacetFromObject Types.Arn
rffoDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE rffoDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | The facet to remove. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rffoSchemaFacet :: Lens.Lens' RemoveFacetFromObject Types.SchemaFacet
rffoSchemaFacet = Lens.field @"schemaFacet"
{-# INLINEABLE rffoSchemaFacet #-}
{-# DEPRECATED schemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead"  #-}

-- | A reference to the object to remove the facet from.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rffoObjectReference :: Lens.Lens' RemoveFacetFromObject Types.ObjectReference
rffoObjectReference = Lens.field @"objectReference"
{-# INLINEABLE rffoObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

instance Core.ToQuery RemoveFacetFromObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveFacetFromObject where
        toHeaders RemoveFacetFromObject{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON RemoveFacetFromObject where
        toJSON RemoveFacetFromObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaFacet" Core..= schemaFacet),
                  Core.Just ("ObjectReference" Core..= objectReference)])

instance Core.AWSRequest RemoveFacetFromObject where
        type Rs RemoveFacetFromObject = RemoveFacetFromObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/object/facets/delete",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RemoveFacetFromObjectResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveFacetFromObjectResponse' smart constructor.
newtype RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveFacetFromObjectResponse' value with any optional fields omitted.
mkRemoveFacetFromObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveFacetFromObjectResponse
mkRemoveFacetFromObjectResponse responseStatus
  = RemoveFacetFromObjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfforrsResponseStatus :: Lens.Lens' RemoveFacetFromObjectResponse Core.Int
rfforrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rfforrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
