{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an object in a 'Directory' . Additionally attaches the object to a parent, if a parent reference and @LinkName@ is specified. An object is simply a collection of 'Facet' attributes. You can also use this API call to create a policy object, if the facet from which you create the object is a policy facet. 
module Network.AWS.CloudDirectory.CreateObject
    (
    -- * Creating a request
      CreateObject (..)
    , mkCreateObject
    -- ** Request lenses
    , coDirectoryArn
    , coSchemaFacets
    , coLinkName
    , coObjectAttributeList
    , coParentReference

    -- * Destructuring the response
    , CreateObjectResponse (..)
    , mkCreateObjectResponse
    -- ** Response lenses
    , corrsObjectIdentifier
    , corrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateObject' smart constructor.
data CreateObject = CreateObject'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Directory' in which the object will be created. For more information, see 'arns' .
  , schemaFacets :: [Types.SchemaFacet]
    -- ^ A list of schema facets to be associated with the object. Do not provide minor version components. See 'SchemaFacet' for details.
  , linkName :: Core.Maybe Types.LinkName
    -- ^ The name of link that is used to attach this object to a parent.
  , objectAttributeList :: Core.Maybe [Types.AttributeKeyAndValue]
    -- ^ The attribute map whose attribute ARN contains the key and attribute value as the map value.
  , parentReference :: Core.Maybe Types.ObjectReference
    -- ^ If specified, the parent reference to which this object will be attached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateObject' value with any optional fields omitted.
mkCreateObject
    :: Types.Arn -- ^ 'directoryArn'
    -> CreateObject
mkCreateObject directoryArn
  = CreateObject'{directoryArn, schemaFacets = Core.mempty,
                  linkName = Core.Nothing, objectAttributeList = Core.Nothing,
                  parentReference = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' in which the object will be created. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coDirectoryArn :: Lens.Lens' CreateObject Types.Arn
coDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE coDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | A list of schema facets to be associated with the object. Do not provide minor version components. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSchemaFacets :: Lens.Lens' CreateObject [Types.SchemaFacet]
coSchemaFacets = Lens.field @"schemaFacets"
{-# INLINEABLE coSchemaFacets #-}
{-# DEPRECATED schemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead"  #-}

-- | The name of link that is used to attach this object to a parent.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coLinkName :: Lens.Lens' CreateObject (Core.Maybe Types.LinkName)
coLinkName = Lens.field @"linkName"
{-# INLINEABLE coLinkName #-}
{-# DEPRECATED linkName "Use generic-lens or generic-optics with 'linkName' instead"  #-}

-- | The attribute map whose attribute ARN contains the key and attribute value as the map value.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectAttributeList :: Lens.Lens' CreateObject (Core.Maybe [Types.AttributeKeyAndValue])
coObjectAttributeList = Lens.field @"objectAttributeList"
{-# INLINEABLE coObjectAttributeList #-}
{-# DEPRECATED objectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead"  #-}

-- | If specified, the parent reference to which this object will be attached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coParentReference :: Lens.Lens' CreateObject (Core.Maybe Types.ObjectReference)
coParentReference = Lens.field @"parentReference"
{-# INLINEABLE coParentReference #-}
{-# DEPRECATED parentReference "Use generic-lens or generic-optics with 'parentReference' instead"  #-}

instance Core.ToQuery CreateObject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateObject where
        toHeaders CreateObject{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON CreateObject where
        toJSON CreateObject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaFacets" Core..= schemaFacets),
                  ("LinkName" Core..=) Core.<$> linkName,
                  ("ObjectAttributeList" Core..=) Core.<$> objectAttributeList,
                  ("ParentReference" Core..=) Core.<$> parentReference])

instance Core.AWSRequest CreateObject where
        type Rs CreateObject = CreateObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/object",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateObjectResponse' Core.<$>
                   (x Core..:? "ObjectIdentifier") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateObjectResponse' smart constructor.
data CreateObjectResponse = CreateObjectResponse'
  { objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The identifier that is associated with the object.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateObjectResponse' value with any optional fields omitted.
mkCreateObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateObjectResponse
mkCreateObjectResponse responseStatus
  = CreateObjectResponse'{objectIdentifier = Core.Nothing,
                          responseStatus}

-- | The identifier that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsObjectIdentifier :: Lens.Lens' CreateObjectResponse (Core.Maybe Types.ObjectIdentifier)
corrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE corrsObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corrsResponseStatus :: Lens.Lens' CreateObjectResponse Core.Int
corrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE corrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
