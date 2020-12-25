{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.PutSchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the metadata key value pair for a specified schema version ID. A maximum of 10 key value pairs will be allowed per schema version. They can be added over one or more calls.
module Network.AWS.Glue.PutSchemaVersionMetadata
  ( -- * Creating a request
    PutSchemaVersionMetadata (..),
    mkPutSchemaVersionMetadata,

    -- ** Request lenses
    psvmMetadataKeyValue,
    psvmSchemaId,
    psvmSchemaVersionId,
    psvmSchemaVersionNumber,

    -- * Destructuring the response
    PutSchemaVersionMetadataResponse (..),
    mkPutSchemaVersionMetadataResponse,

    -- ** Response lenses
    psvmrrsLatestVersion,
    psvmrrsMetadataKey,
    psvmrrsMetadataValue,
    psvmrrsRegistryName,
    psvmrrsSchemaArn,
    psvmrrsSchemaName,
    psvmrrsSchemaVersionId,
    psvmrrsVersionNumber,
    psvmrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutSchemaVersionMetadata' smart constructor.
data PutSchemaVersionMetadata = PutSchemaVersionMetadata'
  { -- | The metadata key's corresponding value.
    metadataKeyValue :: Types.MetadataKeyValuePair,
    -- | The unique ID for the schema.
    schemaId :: Core.Maybe Types.SchemaId,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionIdString,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSchemaVersionMetadata' value with any optional fields omitted.
mkPutSchemaVersionMetadata ::
  -- | 'metadataKeyValue'
  Types.MetadataKeyValuePair ->
  PutSchemaVersionMetadata
mkPutSchemaVersionMetadata metadataKeyValue =
  PutSchemaVersionMetadata'
    { metadataKeyValue,
      schemaId = Core.Nothing,
      schemaVersionId = Core.Nothing,
      schemaVersionNumber = Core.Nothing
    }

-- | The metadata key's corresponding value.
--
-- /Note:/ Consider using 'metadataKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmMetadataKeyValue :: Lens.Lens' PutSchemaVersionMetadata Types.MetadataKeyValuePair
psvmMetadataKeyValue = Lens.field @"metadataKeyValue"
{-# DEPRECATED psvmMetadataKeyValue "Use generic-lens or generic-optics with 'metadataKeyValue' instead." #-}

-- | The unique ID for the schema.
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaId :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Types.SchemaId)
psvmSchemaId = Lens.field @"schemaId"
{-# DEPRECATED psvmSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaVersionId :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Types.SchemaVersionIdString)
psvmSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED psvmSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaVersionNumber :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Types.SchemaVersionNumber)
psvmSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# DEPRECATED psvmSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Core.FromJSON PutSchemaVersionMetadata where
  toJSON PutSchemaVersionMetadata {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MetadataKeyValue" Core..= metadataKeyValue),
            ("SchemaId" Core..=) Core.<$> schemaId,
            ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber
          ]
      )

instance Core.AWSRequest PutSchemaVersionMetadata where
  type Rs PutSchemaVersionMetadata = PutSchemaVersionMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.PutSchemaVersionMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSchemaVersionMetadataResponse'
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

-- | /See:/ 'mkPutSchemaVersionMetadataResponse' smart constructor.
data PutSchemaVersionMetadataResponse = PutSchemaVersionMetadataResponse'
  { -- | The latest version of the schema.
    latestVersion :: Core.Maybe Core.Bool,
    -- | The metadata key.
    metadataKey :: Core.Maybe Types.MetadataKey,
    -- | The value of the metadata key.
    metadataValue :: Core.Maybe Types.MetadataValueString,
    -- | The name for the registry.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The Amazon Resource Name (ARN) for the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name for the schema.
    schemaName :: Core.Maybe Types.SchemaName,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSchemaVersionMetadataResponse' value with any optional fields omitted.
mkPutSchemaVersionMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutSchemaVersionMetadataResponse
mkPutSchemaVersionMetadataResponse responseStatus =
  PutSchemaVersionMetadataResponse'
    { latestVersion = Core.Nothing,
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
psvmrrsLatestVersion :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Bool)
psvmrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED psvmrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsMetadataKey :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.MetadataKey)
psvmrrsMetadataKey = Lens.field @"metadataKey"
{-# DEPRECATED psvmrrsMetadataKey "Use generic-lens or generic-optics with 'metadataKey' instead." #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsMetadataValue :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.MetadataValueString)
psvmrrsMetadataValue = Lens.field @"metadataValue"
{-# DEPRECATED psvmrrsMetadataValue "Use generic-lens or generic-optics with 'metadataValue' instead." #-}

-- | The name for the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsRegistryName :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.RegistryName)
psvmrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED psvmrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) for the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsSchemaArn :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.GlueResourceArn)
psvmrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED psvmrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The name for the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsSchemaName :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.SchemaName)
psvmrrsSchemaName = Lens.field @"schemaName"
{-# DEPRECATED psvmrrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsSchemaVersionId :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.SchemaVersionId)
psvmrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED psvmrrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsVersionNumber :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Natural)
psvmrrsVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED psvmrrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsResponseStatus :: Lens.Lens' PutSchemaVersionMetadataResponse Core.Int
psvmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED psvmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
