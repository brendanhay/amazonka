{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateRegistry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing registry which is used to hold a collection of schemas. The updated properties relate to the registry, and do not modify any of the schemas within the registry.
module Network.AWS.Glue.UpdateRegistry
  ( -- * Creating a request
    UpdateRegistry (..),
    mkUpdateRegistry,

    -- ** Request lenses
    urRegistryId,
    urDescription,

    -- * Destructuring the response
    UpdateRegistryResponse (..),
    mkUpdateRegistryResponse,

    -- ** Response lenses
    urrrsRegistryArn,
    urrrsRegistryName,
    urrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRegistry' smart constructor.
data UpdateRegistry = UpdateRegistry'
  { -- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
    registryId :: Types.RegistryId,
    -- | A description of the registry. If description is not provided, this field will not be updated.
    description :: Types.DescriptionString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegistry' value with any optional fields omitted.
mkUpdateRegistry ::
  -- | 'registryId'
  Types.RegistryId ->
  -- | 'description'
  Types.DescriptionString ->
  UpdateRegistry
mkUpdateRegistry registryId description =
  UpdateRegistry' {registryId, description}

-- | This is a wrapper structure that may contain the registry name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'registryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urRegistryId :: Lens.Lens' UpdateRegistry Types.RegistryId
urRegistryId = Lens.field @"registryId"
{-# DEPRECATED urRegistryId "Use generic-lens or generic-optics with 'registryId' instead." #-}

-- | A description of the registry. If description is not provided, this field will not be updated.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urDescription :: Lens.Lens' UpdateRegistry Types.DescriptionString
urDescription = Lens.field @"description"
{-# DEPRECATED urDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON UpdateRegistry where
  toJSON UpdateRegistry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("RegistryId" Core..= registryId),
            Core.Just ("Description" Core..= description)
          ]
      )

instance Core.AWSRequest UpdateRegistry where
  type Rs UpdateRegistry = UpdateRegistryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateRegistry")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRegistryResponse'
            Core.<$> (x Core..:? "RegistryArn")
            Core.<*> (x Core..:? "RegistryName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRegistryResponse' smart constructor.
data UpdateRegistryResponse = UpdateRegistryResponse'
  { -- | The Amazon Resource name (ARN) of the updated registry.
    registryArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the updated registry.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRegistryResponse' value with any optional fields omitted.
mkUpdateRegistryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRegistryResponse
mkUpdateRegistryResponse responseStatus =
  UpdateRegistryResponse'
    { registryArn = Core.Nothing,
      registryName = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource name (ARN) of the updated registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsRegistryArn :: Lens.Lens' UpdateRegistryResponse (Core.Maybe Types.GlueResourceArn)
urrrsRegistryArn = Lens.field @"registryArn"
{-# DEPRECATED urrrsRegistryArn "Use generic-lens or generic-optics with 'registryArn' instead." #-}

-- | The name of the updated registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsRegistryName :: Lens.Lens' UpdateRegistryResponse (Core.Maybe Types.RegistryName)
urrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED urrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urrrsResponseStatus :: Lens.Lens' UpdateRegistryResponse Core.Int
urrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
