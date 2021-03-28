{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AddFacetToObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Facet' to an object. An object can have more than one facet applied on it.
module Network.AWS.CloudDirectory.AddFacetToObject
    (
    -- * Creating a request
      AddFacetToObject (..)
    , mkAddFacetToObject
    -- ** Request lenses
    , aftoDirectoryArn
    , aftoSchemaFacet
    , aftoObjectReference
    , aftoObjectAttributeList

    -- * Destructuring the response
    , AddFacetToObjectResponse (..)
    , mkAddFacetToObjectResponse
    -- ** Response lenses
    , aftorrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddFacetToObject' smart constructor.
data AddFacetToObject = AddFacetToObject'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
  , schemaFacet :: Types.SchemaFacet
    -- ^ Identifiers for the facet that you are adding to the object. See 'SchemaFacet' for details.
  , objectReference :: Types.ObjectReference
    -- ^ A reference to the object you are adding the specified facet to.
  , objectAttributeList :: Core.Maybe [Types.AttributeKeyAndValue]
    -- ^ Attributes on the facet that you are adding to the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AddFacetToObject' value with any optional fields omitted.
mkAddFacetToObject
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.SchemaFacet -- ^ 'schemaFacet'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> AddFacetToObject
mkAddFacetToObject directoryArn schemaFacet objectReference
  = AddFacetToObject'{directoryArn, schemaFacet, objectReference,
                      objectAttributeList = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoDirectoryArn :: Lens.Lens' AddFacetToObject Types.Arn
aftoDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE aftoDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Identifiers for the facet that you are adding to the object. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoSchemaFacet :: Lens.Lens' AddFacetToObject Types.SchemaFacet
aftoSchemaFacet = Lens.field @"schemaFacet"
{-# INLINEABLE aftoSchemaFacet #-}
{-# DEPRECATED schemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead"  #-}

-- | A reference to the object you are adding the specified facet to.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoObjectReference :: Lens.Lens' AddFacetToObject Types.ObjectReference
aftoObjectReference = Lens.field @"objectReference"
{-# INLINEABLE aftoObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Attributes on the facet that you are adding to the object.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoObjectAttributeList :: Lens.Lens' AddFacetToObject (Core.Maybe [Types.AttributeKeyAndValue])
aftoObjectAttributeList = Lens.field @"objectAttributeList"
{-# INLINEABLE aftoObjectAttributeList #-}
{-# DEPRECATED objectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead"  #-}

instance Core.ToQuery AddFacetToObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddFacetToObject where
        toHeaders AddFacetToObject{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON AddFacetToObject where
        toJSON AddFacetToObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaFacet" Core..= schemaFacet),
                  Core.Just ("ObjectReference" Core..= objectReference),
                  ("ObjectAttributeList" Core..=) Core.<$> objectAttributeList])

instance Core.AWSRequest AddFacetToObject where
        type Rs AddFacetToObject = AddFacetToObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/object/facets",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AddFacetToObjectResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddFacetToObjectResponse' smart constructor.
newtype AddFacetToObjectResponse = AddFacetToObjectResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddFacetToObjectResponse' value with any optional fields omitted.
mkAddFacetToObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddFacetToObjectResponse
mkAddFacetToObjectResponse responseStatus
  = AddFacetToObjectResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftorrsResponseStatus :: Lens.Lens' AddFacetToObjectResponse Core.Int
aftorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aftorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
