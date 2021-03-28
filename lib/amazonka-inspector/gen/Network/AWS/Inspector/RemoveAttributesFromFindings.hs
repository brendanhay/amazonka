{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.RemoveAttributesFromFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes entire attributes (key and value pairs) from the findings that are specified by the ARNs of the findings where an attribute with the specified key exists.
module Network.AWS.Inspector.RemoveAttributesFromFindings
    (
    -- * Creating a request
      RemoveAttributesFromFindings (..)
    , mkRemoveAttributesFromFindings
    -- ** Request lenses
    , raffFindingArns
    , raffAttributeKeys

    -- * Destructuring the response
    , RemoveAttributesFromFindingsResponse (..)
    , mkRemoveAttributesFromFindingsResponse
    -- ** Response lenses
    , raffrrsFailedItems
    , raffrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
  { findingArns :: Core.NonEmpty Types.Arn
    -- ^ The ARNs that specify the findings that you want to remove attributes from.
  , attributeKeys :: [Types.AttributeKey]
    -- ^ The array of attribute keys that you want to remove from specified findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributesFromFindings' value with any optional fields omitted.
mkRemoveAttributesFromFindings
    :: Core.NonEmpty Types.Arn -- ^ 'findingArns'
    -> RemoveAttributesFromFindings
mkRemoveAttributesFromFindings findingArns
  = RemoveAttributesFromFindings'{findingArns,
                                  attributeKeys = Core.mempty}

-- | The ARNs that specify the findings that you want to remove attributes from.
--
-- /Note:/ Consider using 'findingArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffFindingArns :: Lens.Lens' RemoveAttributesFromFindings (Core.NonEmpty Types.Arn)
raffFindingArns = Lens.field @"findingArns"
{-# INLINEABLE raffFindingArns #-}
{-# DEPRECATED findingArns "Use generic-lens or generic-optics with 'findingArns' instead"  #-}

-- | The array of attribute keys that you want to remove from specified findings.
--
-- /Note:/ Consider using 'attributeKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffAttributeKeys :: Lens.Lens' RemoveAttributesFromFindings [Types.AttributeKey]
raffAttributeKeys = Lens.field @"attributeKeys"
{-# INLINEABLE raffAttributeKeys #-}
{-# DEPRECATED attributeKeys "Use generic-lens or generic-optics with 'attributeKeys' instead"  #-}

instance Core.ToQuery RemoveAttributesFromFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveAttributesFromFindings where
        toHeaders RemoveAttributesFromFindings{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.RemoveAttributesFromFindings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveAttributesFromFindings where
        toJSON RemoveAttributesFromFindings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("findingArns" Core..= findingArns),
                  Core.Just ("attributeKeys" Core..= attributeKeys)])

instance Core.AWSRequest RemoveAttributesFromFindings where
        type Rs RemoveAttributesFromFindings =
             RemoveAttributesFromFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RemoveAttributesFromFindingsResponse' Core.<$>
                   (x Core..:? "failedItems" Core..!= Core.mempty) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
  { failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails
    -- ^ Attributes details that cannot be described. An error code is provided for each failed item.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributesFromFindingsResponse' value with any optional fields omitted.
mkRemoveAttributesFromFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveAttributesFromFindingsResponse
mkRemoveAttributesFromFindingsResponse responseStatus
  = RemoveAttributesFromFindingsResponse'{failedItems = Core.mempty,
                                          responseStatus}

-- | Attributes details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffrrsFailedItems :: Lens.Lens' RemoveAttributesFromFindingsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
raffrrsFailedItems = Lens.field @"failedItems"
{-# INLINEABLE raffrrsFailedItems #-}
{-# DEPRECATED failedItems "Use generic-lens or generic-optics with 'failedItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffrrsResponseStatus :: Lens.Lens' RemoveAttributesFromFindingsResponse Core.Int
raffrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE raffrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
