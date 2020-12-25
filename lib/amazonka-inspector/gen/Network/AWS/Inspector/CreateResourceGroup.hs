{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.CreateResourceGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource group using the specified set of tags (key and value pairs) that are used to select the EC2 instances to be included in an Amazon Inspector assessment target. The created resource group is then used to create an Amazon Inspector assessment target. For more information, see 'CreateAssessmentTarget' .
module Network.AWS.Inspector.CreateResourceGroup
  ( -- * Creating a request
    CreateResourceGroup (..),
    mkCreateResourceGroup,

    -- ** Request lenses
    crgResourceGroupTags,

    -- * Destructuring the response
    CreateResourceGroupResponse (..),
    mkCreateResourceGroupResponse,

    -- ** Response lenses
    crgrrsResourceGroupArn,
    crgrrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateResourceGroup' smart constructor.
newtype CreateResourceGroup = CreateResourceGroup'
  { -- | A collection of keys and an array of possible values, '[{"key":"key1","values":["Value1","Value2"]},{"key":"Key2","values":["Value3"]}]'.
    --
    -- For example,'[{"key":"Name","values":["TestEC2Instance"]}]'.
    resourceGroupTags :: Core.NonEmpty Types.ResourceGroupTag
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceGroup' value with any optional fields omitted.
mkCreateResourceGroup ::
  -- | 'resourceGroupTags'
  Core.NonEmpty Types.ResourceGroupTag ->
  CreateResourceGroup
mkCreateResourceGroup resourceGroupTags =
  CreateResourceGroup' {resourceGroupTags}

-- | A collection of keys and an array of possible values, '[{"key":"key1","values":["Value1","Value2"]},{"key":"Key2","values":["Value3"]}]'.
--
-- For example,'[{"key":"Name","values":["TestEC2Instance"]}]'.
--
-- /Note:/ Consider using 'resourceGroupTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgResourceGroupTags :: Lens.Lens' CreateResourceGroup (Core.NonEmpty Types.ResourceGroupTag)
crgResourceGroupTags = Lens.field @"resourceGroupTags"
{-# DEPRECATED crgResourceGroupTags "Use generic-lens or generic-optics with 'resourceGroupTags' instead." #-}

instance Core.FromJSON CreateResourceGroup where
  toJSON CreateResourceGroup {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("resourceGroupTags" Core..= resourceGroupTags)]
      )

instance Core.AWSRequest CreateResourceGroup where
  type Rs CreateResourceGroup = CreateResourceGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.CreateResourceGroup")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceGroupResponse'
            Core.<$> (x Core..: "resourceGroupArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateResourceGroupResponse' smart constructor.
data CreateResourceGroupResponse = CreateResourceGroupResponse'
  { -- | The ARN that specifies the resource group that is created.
    resourceGroupArn :: Types.ResourceGroupArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceGroupResponse' value with any optional fields omitted.
mkCreateResourceGroupResponse ::
  -- | 'resourceGroupArn'
  Types.ResourceGroupArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateResourceGroupResponse
mkCreateResourceGroupResponse resourceGroupArn responseStatus =
  CreateResourceGroupResponse' {resourceGroupArn, responseStatus}

-- | The ARN that specifies the resource group that is created.
--
-- /Note:/ Consider using 'resourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsResourceGroupArn :: Lens.Lens' CreateResourceGroupResponse Types.ResourceGroupArn
crgrrsResourceGroupArn = Lens.field @"resourceGroupArn"
{-# DEPRECATED crgrrsResourceGroupArn "Use generic-lens or generic-optics with 'resourceGroupArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsResponseStatus :: Lens.Lens' CreateResourceGroupResponse Core.Int
crgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
