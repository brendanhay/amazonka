{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.AddAttributesToFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns attributes (key and value pairs) to the findings that are specified by the ARNs of the findings.
module Network.AWS.Inspector.AddAttributesToFindings
    (
    -- * Creating a request
      AddAttributesToFindings (..)
    , mkAddAttributesToFindings
    -- ** Request lenses
    , aatfFindingArns
    , aatfAttributes

    -- * Destructuring the response
    , AddAttributesToFindingsResponse (..)
    , mkAddAttributesToFindingsResponse
    -- ** Response lenses
    , aatfrrsFailedItems
    , aatfrrsResponseStatus
    ) where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAddAttributesToFindings' smart constructor.
data AddAttributesToFindings = AddAttributesToFindings'
  { findingArns :: Core.NonEmpty Types.Arn
    -- ^ The ARNs that specify the findings that you want to assign attributes to.
  , attributes :: [Types.Attribute]
    -- ^ The array of attributes that you want to assign to specified findings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddAttributesToFindings' value with any optional fields omitted.
mkAddAttributesToFindings
    :: Core.NonEmpty Types.Arn -- ^ 'findingArns'
    -> AddAttributesToFindings
mkAddAttributesToFindings findingArns
  = AddAttributesToFindings'{findingArns, attributes = Core.mempty}

-- | The ARNs that specify the findings that you want to assign attributes to.
--
-- /Note:/ Consider using 'findingArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfFindingArns :: Lens.Lens' AddAttributesToFindings (Core.NonEmpty Types.Arn)
aatfFindingArns = Lens.field @"findingArns"
{-# INLINEABLE aatfFindingArns #-}
{-# DEPRECATED findingArns "Use generic-lens or generic-optics with 'findingArns' instead"  #-}

-- | The array of attributes that you want to assign to specified findings.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfAttributes :: Lens.Lens' AddAttributesToFindings [Types.Attribute]
aatfAttributes = Lens.field @"attributes"
{-# INLINEABLE aatfAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.ToQuery AddAttributesToFindings where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AddAttributesToFindings where
        toHeaders AddAttributesToFindings{..}
          = Core.pure
              ("X-Amz-Target", "InspectorService.AddAttributesToFindings")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AddAttributesToFindings where
        toJSON AddAttributesToFindings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("findingArns" Core..= findingArns),
                  Core.Just ("attributes" Core..= attributes)])

instance Core.AWSRequest AddAttributesToFindings where
        type Rs AddAttributesToFindings = AddAttributesToFindingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AddAttributesToFindingsResponse' Core.<$>
                   (x Core..:? "failedItems" Core..!= Core.mempty) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAddAttributesToFindingsResponse' smart constructor.
data AddAttributesToFindingsResponse = AddAttributesToFindingsResponse'
  { failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails
    -- ^ Attribute details that cannot be described. An error code is provided for each failed item.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddAttributesToFindingsResponse' value with any optional fields omitted.
mkAddAttributesToFindingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AddAttributesToFindingsResponse
mkAddAttributesToFindingsResponse responseStatus
  = AddAttributesToFindingsResponse'{failedItems = Core.mempty,
                                     responseStatus}

-- | Attribute details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfrrsFailedItems :: Lens.Lens' AddAttributesToFindingsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
aatfrrsFailedItems = Lens.field @"failedItems"
{-# INLINEABLE aatfrrsFailedItems #-}
{-# DEPRECATED failedItems "Use generic-lens or generic-optics with 'failedItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aatfrrsResponseStatus :: Lens.Lens' AddAttributesToFindingsResponse Core.Int
aatfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aatfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
