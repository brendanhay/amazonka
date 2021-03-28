{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateTypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.UpdateTypedLinkFacet
    (
    -- * Creating a request
      UpdateTypedLinkFacet (..)
    , mkUpdateTypedLinkFacet
    -- ** Request lenses
    , utlfSchemaArn
    , utlfName
    , utlfAttributeUpdates
    , utlfIdentityAttributeOrder

    -- * Destructuring the response
    , UpdateTypedLinkFacetResponse (..)
    , mkUpdateTypedLinkFacetResponse
    -- ** Response lenses
    , utlfrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateTypedLinkFacet' smart constructor.
data UpdateTypedLinkFacet = UpdateTypedLinkFacet'
  { schemaArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
  , name :: Types.TypedLinkName
    -- ^ The unique name of the typed link facet.
  , attributeUpdates :: [Types.TypedLinkFacetAttributeUpdate]
    -- ^ Attributes update structure.
  , identityAttributeOrder :: [Types.AttributeName]
    -- ^ The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to a typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateTypedLinkFacet' value with any optional fields omitted.
mkUpdateTypedLinkFacet
    :: Types.Arn -- ^ 'schemaArn'
    -> Types.TypedLinkName -- ^ 'name'
    -> UpdateTypedLinkFacet
mkUpdateTypedLinkFacet schemaArn name
  = UpdateTypedLinkFacet'{schemaArn, name,
                          attributeUpdates = Core.mempty,
                          identityAttributeOrder = Core.mempty}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfSchemaArn :: Lens.Lens' UpdateTypedLinkFacet Types.Arn
utlfSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE utlfSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfName :: Lens.Lens' UpdateTypedLinkFacet Types.TypedLinkName
utlfName = Lens.field @"name"
{-# INLINEABLE utlfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfAttributeUpdates :: Lens.Lens' UpdateTypedLinkFacet [Types.TypedLinkFacetAttributeUpdate]
utlfAttributeUpdates = Lens.field @"attributeUpdates"
{-# INLINEABLE utlfAttributeUpdates #-}
{-# DEPRECATED attributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead"  #-}

-- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to a typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'identityAttributeOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfIdentityAttributeOrder :: Lens.Lens' UpdateTypedLinkFacet [Types.AttributeName]
utlfIdentityAttributeOrder = Lens.field @"identityAttributeOrder"
{-# INLINEABLE utlfIdentityAttributeOrder #-}
{-# DEPRECATED identityAttributeOrder "Use generic-lens or generic-optics with 'identityAttributeOrder' instead"  #-}

instance Core.ToQuery UpdateTypedLinkFacet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateTypedLinkFacet where
        toHeaders UpdateTypedLinkFacet{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON UpdateTypedLinkFacet where
        toJSON UpdateTypedLinkFacet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("AttributeUpdates" Core..= attributeUpdates),
                  Core.Just
                    ("IdentityAttributeOrder" Core..= identityAttributeOrder)])

instance Core.AWSRequest UpdateTypedLinkFacet where
        type Rs UpdateTypedLinkFacet = UpdateTypedLinkFacetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/typedlink/facet",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateTypedLinkFacetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateTypedLinkFacetResponse' smart constructor.
newtype UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTypedLinkFacetResponse' value with any optional fields omitted.
mkUpdateTypedLinkFacetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateTypedLinkFacetResponse
mkUpdateTypedLinkFacetResponse responseStatus
  = UpdateTypedLinkFacetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfrrsResponseStatus :: Lens.Lens' UpdateTypedLinkFacetResponse Core.Int
utlfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE utlfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
