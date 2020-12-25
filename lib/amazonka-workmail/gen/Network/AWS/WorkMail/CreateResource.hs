{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CreateResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail resource.
module Network.AWS.WorkMail.CreateResource
  ( -- * Creating a request
    CreateResource (..),
    mkCreateResource,

    -- ** Request lenses
    crOrganizationId,
    crName,
    crType,

    -- * Destructuring the response
    CreateResourceResponse (..),
    mkCreateResourceResponse,

    -- ** Response lenses
    crrrsResourceId,
    crrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCreateResource' smart constructor.
data CreateResource = CreateResource'
  { -- | The identifier associated with the organization for which the resource is created.
    organizationId :: Types.OrganizationId,
    -- | The name of the new resource.
    name :: Types.ResourceName,
    -- | The type of the new resource. The available types are @equipment@ and @room@ .
    type' :: Types.ResourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResource' value with any optional fields omitted.
mkCreateResource ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'name'
  Types.ResourceName ->
  -- | 'type\''
  Types.ResourceType ->
  CreateResource
mkCreateResource organizationId name type' =
  CreateResource' {organizationId, name, type'}

-- | The identifier associated with the organization for which the resource is created.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crOrganizationId :: Lens.Lens' CreateResource Types.OrganizationId
crOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED crOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The name of the new resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' CreateResource Types.ResourceName
crName = Lens.field @"name"
{-# DEPRECATED crName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the new resource. The available types are @equipment@ and @room@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crType :: Lens.Lens' CreateResource Types.ResourceType
crType = Lens.field @"type'"
{-# DEPRECATED crType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON CreateResource where
  toJSON CreateResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type')
          ]
      )

instance Core.AWSRequest CreateResource where
  type Rs CreateResource = CreateResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.CreateResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceResponse'
            Core.<$> (x Core..:? "ResourceId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateResourceResponse' smart constructor.
data CreateResourceResponse = CreateResourceResponse'
  { -- | The identifier of the new resource.
    resourceId :: Core.Maybe Types.ResourceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceResponse' value with any optional fields omitted.
mkCreateResourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateResourceResponse
mkCreateResourceResponse responseStatus =
  CreateResourceResponse'
    { resourceId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the new resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResourceId :: Lens.Lens' CreateResourceResponse (Core.Maybe Types.ResourceId)
crrrsResourceId = Lens.field @"resourceId"
{-# DEPRECATED crrrsResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrrsResponseStatus :: Lens.Lens' CreateResourceResponse Core.Int
crrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
