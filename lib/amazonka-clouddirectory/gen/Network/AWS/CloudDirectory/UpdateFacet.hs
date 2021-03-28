{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Does the following:
--
--
--     * Adds new @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--
--     * Updates existing @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--
--     * Deletes existing @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--
module Network.AWS.CloudDirectory.UpdateFacet
    (
    -- * Creating a request
      UpdateFacet (..)
    , mkUpdateFacet
    -- ** Request lenses
    , ufSchemaArn
    , ufName
    , ufAttributeUpdates
    , ufObjectType

    -- * Destructuring the response
    , UpdateFacetResponse (..)
    , mkUpdateFacetResponse
    -- ** Response lenses
    , ufrrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateFacet' smart constructor.
data UpdateFacet = UpdateFacet'
  { schemaArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
  , name :: Types.FacetName
    -- ^ The name of the facet.
  , attributeUpdates :: Core.Maybe [Types.FacetAttributeUpdate]
    -- ^ List of attributes that need to be updated in a given schema 'Facet' . Each attribute is followed by @AttributeAction@ , which specifies the type of update operation to perform. 
  , objectType :: Core.Maybe Types.ObjectType
    -- ^ The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateFacet' value with any optional fields omitted.
mkUpdateFacet
    :: Types.Arn -- ^ 'schemaArn'
    -> Types.FacetName -- ^ 'name'
    -> UpdateFacet
mkUpdateFacet schemaArn name
  = UpdateFacet'{schemaArn, name, attributeUpdates = Core.Nothing,
                 objectType = Core.Nothing}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufSchemaArn :: Lens.Lens' UpdateFacet Types.Arn
ufSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE ufSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The name of the facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFacet Types.FacetName
ufName = Lens.field @"name"
{-# INLINEABLE ufName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | List of attributes that need to be updated in a given schema 'Facet' . Each attribute is followed by @AttributeAction@ , which specifies the type of update operation to perform. 
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAttributeUpdates :: Lens.Lens' UpdateFacet (Core.Maybe [Types.FacetAttributeUpdate])
ufAttributeUpdates = Lens.field @"attributeUpdates"
{-# INLINEABLE ufAttributeUpdates #-}
{-# DEPRECATED attributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead"  #-}

-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- /Note:/ Consider using 'objectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufObjectType :: Lens.Lens' UpdateFacet (Core.Maybe Types.ObjectType)
ufObjectType = Lens.field @"objectType"
{-# INLINEABLE ufObjectType #-}
{-# DEPRECATED objectType "Use generic-lens or generic-optics with 'objectType' instead"  #-}

instance Core.ToQuery UpdateFacet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateFacet where
        toHeaders UpdateFacet{..}
          = Core.toHeaders "x-amz-data-partition" schemaArn

instance Core.FromJSON UpdateFacet where
        toJSON UpdateFacet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("AttributeUpdates" Core..=) Core.<$> attributeUpdates,
                  ("ObjectType" Core..=) Core.<$> objectType])

instance Core.AWSRequest UpdateFacet where
        type Rs UpdateFacet = UpdateFacetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/facet",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateFacetResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateFacetResponse' smart constructor.
newtype UpdateFacetResponse = UpdateFacetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateFacetResponse' value with any optional fields omitted.
mkUpdateFacetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateFacetResponse
mkUpdateFacetResponse responseStatus
  = UpdateFacetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrrsResponseStatus :: Lens.Lens' UpdateFacetResponse Core.Int
ufrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ufrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
