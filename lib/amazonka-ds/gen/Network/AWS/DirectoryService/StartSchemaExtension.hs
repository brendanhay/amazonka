{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.StartSchemaExtension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a schema extension to a Microsoft AD directory.
module Network.AWS.DirectoryService.StartSchemaExtension
  ( -- * Creating a request
    StartSchemaExtension (..),
    mkStartSchemaExtension,

    -- ** Request lenses
    sseDirectoryId,
    sseCreateSnapshotBeforeSchemaExtension,
    sseLdifContent,
    sseDescription,

    -- * Destructuring the response
    StartSchemaExtensionResponse (..),
    mkStartSchemaExtensionResponse,

    -- ** Response lenses
    sserrsSchemaExtensionId,
    sserrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSchemaExtension' smart constructor.
data StartSchemaExtension = StartSchemaExtension'
  { -- | The identifier of the directory for which the schema extension will be applied to.
    directoryId :: Types.DirectoryId,
    -- | If true, creates a snapshot of the directory before applying the schema extension.
    createSnapshotBeforeSchemaExtension :: Core.Bool,
    -- | The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
    ldifContent :: Types.LdifContent,
    -- | A description of the schema extension.
    description :: Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSchemaExtension' value with any optional fields omitted.
mkStartSchemaExtension ::
  -- | 'directoryId'
  Types.DirectoryId ->
  -- | 'createSnapshotBeforeSchemaExtension'
  Core.Bool ->
  -- | 'ldifContent'
  Types.LdifContent ->
  -- | 'description'
  Types.Description ->
  StartSchemaExtension
mkStartSchemaExtension
  directoryId
  createSnapshotBeforeSchemaExtension
  ldifContent
  description =
    StartSchemaExtension'
      { directoryId,
        createSnapshotBeforeSchemaExtension,
        ldifContent,
        description
      }

-- | The identifier of the directory for which the schema extension will be applied to.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseDirectoryId :: Lens.Lens' StartSchemaExtension Types.DirectoryId
sseDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED sseDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | If true, creates a snapshot of the directory before applying the schema extension.
--
-- /Note:/ Consider using 'createSnapshotBeforeSchemaExtension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseCreateSnapshotBeforeSchemaExtension :: Lens.Lens' StartSchemaExtension Core.Bool
sseCreateSnapshotBeforeSchemaExtension = Lens.field @"createSnapshotBeforeSchemaExtension"
{-# DEPRECATED sseCreateSnapshotBeforeSchemaExtension "Use generic-lens or generic-optics with 'createSnapshotBeforeSchemaExtension' instead." #-}

-- | The LDIF file represented as a string. To construct the LdifContent string, precede each line as it would be formatted in an ldif file with \n. See the example request below for more details. The file size can be no larger than 1MB.
--
-- /Note:/ Consider using 'ldifContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseLdifContent :: Lens.Lens' StartSchemaExtension Types.LdifContent
sseLdifContent = Lens.field @"ldifContent"
{-# DEPRECATED sseLdifContent "Use generic-lens or generic-optics with 'ldifContent' instead." #-}

-- | A description of the schema extension.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sseDescription :: Lens.Lens' StartSchemaExtension Types.Description
sseDescription = Lens.field @"description"
{-# DEPRECATED sseDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON StartSchemaExtension where
  toJSON StartSchemaExtension {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just
              ( "CreateSnapshotBeforeSchemaExtension"
                  Core..= createSnapshotBeforeSchemaExtension
              ),
            Core.Just ("LdifContent" Core..= ldifContent),
            Core.Just ("Description" Core..= description)
          ]
      )

instance Core.AWSRequest StartSchemaExtension where
  type Rs StartSchemaExtension = StartSchemaExtensionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DirectoryService_20150416.StartSchemaExtension")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSchemaExtensionResponse'
            Core.<$> (x Core..:? "SchemaExtensionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartSchemaExtensionResponse' smart constructor.
data StartSchemaExtensionResponse = StartSchemaExtensionResponse'
  { -- | The identifier of the schema extension that will be applied.
    schemaExtensionId :: Core.Maybe Types.SchemaExtensionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSchemaExtensionResponse' value with any optional fields omitted.
mkStartSchemaExtensionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartSchemaExtensionResponse
mkStartSchemaExtensionResponse responseStatus =
  StartSchemaExtensionResponse'
    { schemaExtensionId = Core.Nothing,
      responseStatus
    }

-- | The identifier of the schema extension that will be applied.
--
-- /Note:/ Consider using 'schemaExtensionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsSchemaExtensionId :: Lens.Lens' StartSchemaExtensionResponse (Core.Maybe Types.SchemaExtensionId)
sserrsSchemaExtensionId = Lens.field @"schemaExtensionId"
{-# DEPRECATED sserrsSchemaExtensionId "Use generic-lens or generic-optics with 'schemaExtensionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sserrsResponseStatus :: Lens.Lens' StartSchemaExtensionResponse Core.Int
sserrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sserrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
