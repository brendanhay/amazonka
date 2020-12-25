{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RemoveAttributesFromFindings (..),
    mkRemoveAttributesFromFindings,

    -- ** Request lenses
    raffFindingArns,
    raffAttributeKeys,

    -- * Destructuring the response
    RemoveAttributesFromFindingsResponse (..),
    mkRemoveAttributesFromFindingsResponse,

    -- ** Response lenses
    raffrrsFailedItems,
    raffrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
  { -- | The ARNs that specify the findings that you want to remove attributes from.
    findingArns :: Core.NonEmpty Types.Arn,
    -- | The array of attribute keys that you want to remove from specified findings.
    attributeKeys :: [Types.AttributeKey]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributesFromFindings' value with any optional fields omitted.
mkRemoveAttributesFromFindings ::
  -- | 'findingArns'
  Core.NonEmpty Types.Arn ->
  RemoveAttributesFromFindings
mkRemoveAttributesFromFindings findingArns =
  RemoveAttributesFromFindings'
    { findingArns,
      attributeKeys = Core.mempty
    }

-- | The ARNs that specify the findings that you want to remove attributes from.
--
-- /Note:/ Consider using 'findingArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffFindingArns :: Lens.Lens' RemoveAttributesFromFindings (Core.NonEmpty Types.Arn)
raffFindingArns = Lens.field @"findingArns"
{-# DEPRECATED raffFindingArns "Use generic-lens or generic-optics with 'findingArns' instead." #-}

-- | The array of attribute keys that you want to remove from specified findings.
--
-- /Note:/ Consider using 'attributeKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffAttributeKeys :: Lens.Lens' RemoveAttributesFromFindings [Types.AttributeKey]
raffAttributeKeys = Lens.field @"attributeKeys"
{-# DEPRECATED raffAttributeKeys "Use generic-lens or generic-optics with 'attributeKeys' instead." #-}

instance Core.FromJSON RemoveAttributesFromFindings where
  toJSON RemoveAttributesFromFindings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("findingArns" Core..= findingArns),
            Core.Just ("attributeKeys" Core..= attributeKeys)
          ]
      )

instance Core.AWSRequest RemoveAttributesFromFindings where
  type
    Rs RemoveAttributesFromFindings =
      RemoveAttributesFromFindingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.RemoveAttributesFromFindings")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveAttributesFromFindingsResponse'
            Core.<$> (x Core..:? "failedItems" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
  { -- | Attributes details that cannot be described. An error code is provided for each failed item.
    failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAttributesFromFindingsResponse' value with any optional fields omitted.
mkRemoveAttributesFromFindingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveAttributesFromFindingsResponse
mkRemoveAttributesFromFindingsResponse responseStatus =
  RemoveAttributesFromFindingsResponse'
    { failedItems = Core.mempty,
      responseStatus
    }

-- | Attributes details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffrrsFailedItems :: Lens.Lens' RemoveAttributesFromFindingsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
raffrrsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED raffrrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raffrrsResponseStatus :: Lens.Lens' RemoveAttributesFromFindingsResponse Core.Int
raffrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED raffrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
