{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes within a facet that are associated with an object.
module Network.AWS.CloudDirectory.GetObjectAttributes
    (
    -- * Creating a request
      GetObjectAttributes (..)
    , mkGetObjectAttributes
    -- ** Request lenses
    , goaDirectoryArn
    , goaObjectReference
    , goaSchemaFacet
    , goaAttributeNames
    , goaConsistencyLevel

    -- * Destructuring the response
    , GetObjectAttributesResponse (..)
    , mkGetObjectAttributesResponse
    -- ** Response lenses
    , goarrsAttributes
    , goarrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetObjectAttributes' smart constructor.
data GetObjectAttributes = GetObjectAttributes'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides.
  , objectReference :: Types.ObjectReference
    -- ^ Reference that identifies the object whose attributes will be retrieved.
  , schemaFacet :: Types.SchemaFacet
    -- ^ Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
  , attributeNames :: [Types.AttributeName]
    -- ^ List of attribute names whose values will be retrieved.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ The consistency level at which to retrieve the attributes on an object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectAttributes' value with any optional fields omitted.
mkGetObjectAttributes
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> Types.SchemaFacet -- ^ 'schemaFacet'
    -> GetObjectAttributes
mkGetObjectAttributes directoryArn objectReference schemaFacet
  = GetObjectAttributes'{directoryArn, objectReference, schemaFacet,
                         attributeNames = Core.mempty, consistencyLevel = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaDirectoryArn :: Lens.Lens' GetObjectAttributes Types.Arn
goaDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE goaDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Reference that identifies the object whose attributes will be retrieved.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaObjectReference :: Lens.Lens' GetObjectAttributes Types.ObjectReference
goaObjectReference = Lens.field @"objectReference"
{-# INLINEABLE goaObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaSchemaFacet :: Lens.Lens' GetObjectAttributes Types.SchemaFacet
goaSchemaFacet = Lens.field @"schemaFacet"
{-# INLINEABLE goaSchemaFacet #-}
{-# DEPRECATED schemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead"  #-}

-- | List of attribute names whose values will be retrieved.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaAttributeNames :: Lens.Lens' GetObjectAttributes [Types.AttributeName]
goaAttributeNames = Lens.field @"attributeNames"
{-# INLINEABLE goaAttributeNames #-}
{-# DEPRECATED attributeNames "Use generic-lens or generic-optics with 'attributeNames' instead"  #-}

-- | The consistency level at which to retrieve the attributes on an object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaConsistencyLevel :: Lens.Lens' GetObjectAttributes (Core.Maybe Types.ConsistencyLevel)
goaConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE goaConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

instance Core.ToQuery GetObjectAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetObjectAttributes where
        toHeaders GetObjectAttributes{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn Core.<>
              Core.toHeaders "x-amz-consistency-level" consistencyLevel

instance Core.FromJSON GetObjectAttributes where
        toJSON GetObjectAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference),
                  Core.Just ("SchemaFacet" Core..= schemaFacet),
                  Core.Just ("AttributeNames" Core..= attributeNames)])

instance Core.AWSRequest GetObjectAttributes where
        type Rs GetObjectAttributes = GetObjectAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/object/attributes/get",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetObjectAttributesResponse' Core.<$>
                   (x Core..:? "Attributes") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetObjectAttributesResponse' smart constructor.
data GetObjectAttributesResponse = GetObjectAttributesResponse'
  { attributes :: Core.Maybe [Types.AttributeKeyAndValue]
    -- ^ The attributes that are associated with the object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetObjectAttributesResponse' value with any optional fields omitted.
mkGetObjectAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetObjectAttributesResponse
mkGetObjectAttributesResponse responseStatus
  = GetObjectAttributesResponse'{attributes = Core.Nothing,
                                 responseStatus}

-- | The attributes that are associated with the object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsAttributes :: Lens.Lens' GetObjectAttributesResponse (Core.Maybe [Types.AttributeKeyAndValue])
goarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE goarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsResponseStatus :: Lens.Lens' GetObjectAttributesResponse Core.Int
goarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
