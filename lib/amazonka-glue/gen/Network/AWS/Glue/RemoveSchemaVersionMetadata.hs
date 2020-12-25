{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.RemoveSchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a key value pair from the schema version metadata for the specified schema version ID.
module Network.AWS.Glue.RemoveSchemaVersionMetadata
  ( -- * Creating a request
    RemoveSchemaVersionMetadata (..),
    mkRemoveSchemaVersionMetadata,

    -- ** Request lenses
    rsvmMetadataKeyValue,
    rsvmSchemaId,
    rsvmSchemaVersionId,
    rsvmSchemaVersionNumber,

    -- * Destructuring the response
    RemoveSchemaVersionMetadataResponse (..),
    mkRemoveSchemaVersionMetadataResponse,

    -- ** Response lenses
    rsvmrrsLatestVersion,
    rsvmrrsMetadataKey,
    rsvmrrsMetadataValue,
    rsvmrrsRegistryName,
    rsvmrrsSchemaArn,
    rsvmrrsSchemaName,
    rsvmrrsSchemaVersionId,
    rsvmrrsVersionNumber,
    rsvmrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { -- | The value of the metadata key.
    metadataKeyValue :: Types.MetadataKeyValuePair,
    -- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
    schemaId :: Core.Maybe Types.SchemaId,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveSchemaVersionMetadata' value with any optional fields omitted.
mkRemoveSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  Types.MetadataKeyValuePair ->
  RemoveSchemaVersionMetadata
mkRemoveSchemaVersionMetadata metadataKeyValue =
  RemoveSchemaVersionMetadata'
    { metadataKeyValue,
      schemaId = Core.Nothing,
      schemaVersionId = Core.Nothing,
      schemaVersionNumber = Core.Nothing
    }

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmMetadataKeyValue :: Lens.Lens' RemoveSchemaVersionMetadata Types.MetadataKeyValuePair
rsvmMetadataKeyValue = Lens.field @"metadataKeyValue"
{-# DEPRECATED rsvmMetadataKeyValue "Use generic-lens or generic-optics with 'metadataKeyValue' instead." #-}

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaId :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Types.SchemaId)
rsvmSchemaId = Lens.field @"schemaId"
{-# DEPRECATED rsvmSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Types.SchemaVersionId)
rsvmSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED rsvmSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Types.SchemaVersionNumber)
rsvmSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# DEPRECATED rsvmSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Core.FromJSON RemoveSchemaVersionMetadata where
  toJSON RemoveSchemaVersionMetadata {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MetadataKeyValue" Core..= metadataKeyValue),
            ("SchemaId" Core..=) Core.<$> schemaId,
            ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber
          ]
      )

instance Core.AWSRequest RemoveSchemaVersionMetadata where
  type
    Rs RemoveSchemaVersionMetadata =
      RemoveSchemaVersionMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.RemoveSchemaVersionMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveSchemaVersionMetadataResponse'
            Core.<$> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "MetadataKey")
            Core.<*> (x Core..:? "MetadataValue")
            Core.<*> (x Core..:? "RegistryName")
            Core.<*> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "SchemaName")
            Core.<*> (x Core..:? "SchemaVersionId")
            Core.<*> (x Core..:? "VersionNumber")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRemoveSchemaVersionMetadataResponse' smart constructor.
data RemoveSchemaVersionMetadataResponse = RemoveSchemaVersionMetadataResponse'
  { -- | The latest version of the schema.
    latestVersion :: Core.Maybe Core.Bool,
    -- | The metadata key.
    metadataKey :: Core.Maybe Types.MetadataKey,
    -- | The value of the metadata key.
    metadataValue :: Core.Maybe Types.MetadataValueString,
    -- | The name of the registry.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the schema.
    schemaName :: Core.Maybe Types.SchemaName,
    -- | The version ID for the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveSchemaVersionMetadataResponse' value with any optional fields omitted.
mkRemoveSchemaVersionMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RemoveSchemaVersionMetadataResponse
mkRemoveSchemaVersionMetadataResponse responseStatus =
  RemoveSchemaVersionMetadataResponse'
    { latestVersion =
        Core.Nothing,
      metadataKey = Core.Nothing,
      metadataValue = Core.Nothing,
      registryName = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaName = Core.Nothing,
      schemaVersionId = Core.Nothing,
      versionNumber = Core.Nothing,
      responseStatus
    }

-- | The latest version of the schema.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsLatestVersion :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Bool)
rsvmrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED rsvmrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsMetadataKey :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.MetadataKey)
rsvmrrsMetadataKey = Lens.field @"metadataKey"
{-# DEPRECATED rsvmrrsMetadataKey "Use generic-lens or generic-optics with 'metadataKey' instead." #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsMetadataValue :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.MetadataValueString)
rsvmrrsMetadataValue = Lens.field @"metadataValue"
{-# DEPRECATED rsvmrrsMetadataValue "Use generic-lens or generic-optics with 'metadataValue' instead." #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsRegistryName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.RegistryName)
rsvmrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED rsvmrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsSchemaArn :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.GlueResourceArn)
rsvmrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED rsvmrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsSchemaName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.SchemaName)
rsvmrrsSchemaName = Lens.field @"schemaName"
{-# DEPRECATED rsvmrrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The version ID for the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsSchemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.SchemaVersionId)
rsvmrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED rsvmrrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Natural)
rsvmrrsVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED rsvmrrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsResponseStatus :: Lens.Lens' RemoveSchemaVersionMetadataResponse Core.Int
rsvmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsvmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
