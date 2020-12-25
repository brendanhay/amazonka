{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description, compatibility setting, or version checkpoint for a schema set.
--
-- For updating the compatibility setting, the call will not validate compatibility for the entire set of schema versions with the new compatibility setting. If the value for @Compatibility@ is provided, the @VersionNumber@ (a checkpoint) is also required. The API will validate the checkpoint version number for consistency.
-- If the value for the @VersionNumber@ (checkpoint) is provided, @Compatibility@ is optional and this can be used to set/reset a checkpoint for the schema.
-- This update will happen only if the schema is in the AVAILABLE state.
module Network.AWS.Glue.UpdateSchema
  ( -- * Creating a request
    UpdateSchema (..),
    mkUpdateSchema,

    -- ** Request lenses
    usSchemaId,
    usCompatibility,
    usDescription,
    usSchemaVersionNumber,

    -- * Destructuring the response
    UpdateSchemaResponse (..),
    mkUpdateSchemaResponse,

    -- ** Response lenses
    usrrsRegistryName,
    usrrsSchemaArn,
    usrrsSchemaName,
    usrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
    schemaId :: Types.SchemaId,
    -- | The new compatibility setting for the schema.
    compatibility :: Core.Maybe Types.Compatibility,
    -- | The new description for the schema.
    description :: Core.Maybe Types.DescriptionString,
    -- | Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
    schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSchema' value with any optional fields omitted.
mkUpdateSchema ::
  -- | 'schemaId'
  Types.SchemaId ->
  UpdateSchema
mkUpdateSchema schemaId =
  UpdateSchema'
    { schemaId,
      compatibility = Core.Nothing,
      description = Core.Nothing,
      schemaVersionNumber = Core.Nothing
    }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSchemaId :: Lens.Lens' UpdateSchema Types.SchemaId
usSchemaId = Lens.field @"schemaId"
{-# DEPRECATED usSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The new compatibility setting for the schema.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usCompatibility :: Lens.Lens' UpdateSchema (Core.Maybe Types.Compatibility)
usCompatibility = Lens.field @"compatibility"
{-# DEPRECATED usCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | The new description for the schema.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usDescription :: Lens.Lens' UpdateSchema (Core.Maybe Types.DescriptionString)
usDescription = Lens.field @"description"
{-# DEPRECATED usDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Version number required for check pointing. One of @VersionNumber@ or @Compatibility@ has to be provided.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSchemaVersionNumber :: Lens.Lens' UpdateSchema (Core.Maybe Types.SchemaVersionNumber)
usSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# DEPRECATED usSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Core.FromJSON UpdateSchema where
  toJSON UpdateSchema {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaId" Core..= schemaId),
            ("Compatibility" Core..=) Core.<$> compatibility,
            ("Description" Core..=) Core.<$> description,
            ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber
          ]
      )

instance Core.AWSRequest UpdateSchema where
  type Rs UpdateSchema = UpdateSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.UpdateSchema")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Core.<$> (x Core..:? "RegistryName")
            Core.<*> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "SchemaName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The name of the registry that contains the schema.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the schema.
    schemaName :: Core.Maybe Types.SchemaName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSchemaResponse' value with any optional fields omitted.
mkUpdateSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSchemaResponse
mkUpdateSchemaResponse responseStatus =
  UpdateSchemaResponse'
    { registryName = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaName = Core.Nothing,
      responseStatus
    }

-- | The name of the registry that contains the schema.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsRegistryName :: Lens.Lens' UpdateSchemaResponse (Core.Maybe Types.RegistryName)
usrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED usrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSchemaArn :: Lens.Lens' UpdateSchemaResponse (Core.Maybe Types.GlueResourceArn)
usrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED usrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsSchemaName :: Lens.Lens' UpdateSchemaResponse (Core.Maybe Types.SchemaName)
usrrsSchemaName = Lens.field @"schemaName"
{-# DEPRECATED usrrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrsResponseStatus :: Lens.Lens' UpdateSchemaResponse Core.Int
usrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
