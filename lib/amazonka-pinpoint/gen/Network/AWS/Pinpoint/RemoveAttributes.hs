{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.RemoveAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more attributes, of the same attribute type, from all the endpoints that are associated with an application.
module Network.AWS.Pinpoint.RemoveAttributes
    (
    -- * Creating a request
      RemoveAttributes (..)
    , mkRemoveAttributes
    -- ** Request lenses
    , raAttributeType
    , raApplicationId
    , raUpdateAttributesRequest

    -- * Destructuring the response
    , RemoveAttributesResponse (..)
    , mkRemoveAttributesResponse
    -- ** Response lenses
    , rarrsAttributesResource
    , rarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveAttributes' smart constructor.
data RemoveAttributes = RemoveAttributes'
  { attributeType :: Core.Text
    -- ^ The type of attribute or attributes to remove. Valid values are:
--
--
--     * endpoint-custom-attributes - Custom attributes that describe endpoints, such as the date when an associated user opted in or out of receiving communications from you through a specific type of channel.
--
--
--     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints, such as the number of app sessions or the number of items left in a cart.
--
--
--     * endpoint-user-attributes - Custom attributes that describe users, such as first name, last name, and age.
--
--
  , applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , updateAttributesRequest :: Types.UpdateAttributesRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributes' value with any optional fields omitted.
mkRemoveAttributes
    :: Core.Text -- ^ 'attributeType'
    -> Core.Text -- ^ 'applicationId'
    -> Types.UpdateAttributesRequest -- ^ 'updateAttributesRequest'
    -> RemoveAttributes
mkRemoveAttributes attributeType applicationId
  updateAttributesRequest
  = RemoveAttributes'{attributeType, applicationId,
                      updateAttributesRequest}

-- | The type of attribute or attributes to remove. Valid values are:
--
--
--     * endpoint-custom-attributes - Custom attributes that describe endpoints, such as the date when an associated user opted in or out of receiving communications from you through a specific type of channel.
--
--
--     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints, such as the number of app sessions or the number of items left in a cart.
--
--
--     * endpoint-user-attributes - Custom attributes that describe users, such as first name, last name, and age.
--
--
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAttributeType :: Lens.Lens' RemoveAttributes Core.Text
raAttributeType = Lens.field @"attributeType"
{-# INLINEABLE raAttributeType #-}
{-# DEPRECATED attributeType "Use generic-lens or generic-optics with 'attributeType' instead"  #-}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raApplicationId :: Lens.Lens' RemoveAttributes Core.Text
raApplicationId = Lens.field @"applicationId"
{-# INLINEABLE raApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateAttributesRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raUpdateAttributesRequest :: Lens.Lens' RemoveAttributes Types.UpdateAttributesRequest
raUpdateAttributesRequest = Lens.field @"updateAttributesRequest"
{-# INLINEABLE raUpdateAttributesRequest #-}
{-# DEPRECATED updateAttributesRequest "Use generic-lens or generic-optics with 'updateAttributesRequest' instead"  #-}

instance Core.ToQuery RemoveAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveAttributes where
        toHeaders RemoveAttributes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveAttributes where
        toJSON RemoveAttributes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("UpdateAttributesRequest" Core..= updateAttributesRequest)])

instance Core.AWSRequest RemoveAttributes where
        type Rs RemoveAttributes = RemoveAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<>
                             "/attributes/"
                             Core.<> Core.toText attributeType,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RemoveAttributesResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveAttributesResponse' smart constructor.
data RemoveAttributesResponse = RemoveAttributesResponse'
  { attributesResource :: Types.AttributesResource
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributesResponse' value with any optional fields omitted.
mkRemoveAttributesResponse
    :: Types.AttributesResource -- ^ 'attributesResource'
    -> Core.Int -- ^ 'responseStatus'
    -> RemoveAttributesResponse
mkRemoveAttributesResponse attributesResource responseStatus
  = RemoveAttributesResponse'{attributesResource, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'attributesResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsAttributesResource :: Lens.Lens' RemoveAttributesResponse Types.AttributesResource
rarrsAttributesResource = Lens.field @"attributesResource"
{-# INLINEABLE rarrsAttributesResource #-}
{-# DEPRECATED attributesResource "Use generic-lens or generic-optics with 'attributesResource' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsResponseStatus :: Lens.Lens' RemoveAttributesResponse Core.Int
rarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
