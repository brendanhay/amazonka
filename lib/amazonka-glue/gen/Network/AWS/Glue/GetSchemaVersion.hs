{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the specified schema by its unique ID assigned when a version of the schema is created or registered. Schema versions in Deleted status will not be included in the results.
module Network.AWS.Glue.GetSchemaVersion
  ( -- * Creating a request
    GetSchemaVersion (..),
    mkGetSchemaVersion,

    -- ** Request lenses
    gsvSchemaId,
    gsvSchemaVersionId,
    gsvSchemaVersionNumber,

    -- * Destructuring the response
    GetSchemaVersionResponse (..),
    mkGetSchemaVersionResponse,

    -- ** Response lenses
    gsvrrsCreatedTime,
    gsvrrsDataFormat,
    gsvrrsSchemaArn,
    gsvrrsSchemaDefinition,
    gsvrrsSchemaVersionId,
    gsvrrsStatus,
    gsvrrsVersionNumber,
    gsvrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaVersion' smart constructor.
data GetSchemaVersion = GetSchemaVersion'
  { -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: Core.Maybe Types.SchemaId,
    -- | The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaVersion' value with any optional fields omitted.
mkGetSchemaVersion ::
  GetSchemaVersion
mkGetSchemaVersion =
  GetSchemaVersion'
    { schemaId = Core.Nothing,
      schemaVersionId = Core.Nothing,
      schemaVersionNumber = Core.Nothing
    }

-- | This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaId :: Lens.Lens' GetSchemaVersion (Core.Maybe Types.SchemaId)
gsvSchemaId = Lens.field @"schemaId"
{-# DEPRECATED gsvSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaVersionId :: Lens.Lens' GetSchemaVersion (Core.Maybe Types.SchemaVersionId)
gsvSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED gsvSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaVersionNumber :: Lens.Lens' GetSchemaVersion (Core.Maybe Types.SchemaVersionNumber)
gsvSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# DEPRECATED gsvSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Core.FromJSON GetSchemaVersion where
  toJSON GetSchemaVersion {..} =
    Core.object
      ( Core.catMaybes
          [ ("SchemaId" Core..=) Core.<$> schemaId,
            ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber
          ]
      )

instance Core.AWSRequest GetSchemaVersion where
  type Rs GetSchemaVersion = GetSchemaVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetSchemaVersion")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaVersionResponse'
            Core.<$> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "DataFormat")
            Core.<*> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "SchemaDefinition")
            Core.<*> (x Core..:? "SchemaVersionId")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "VersionNumber")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSchemaVersionResponse' smart constructor.
data GetSchemaVersionResponse = GetSchemaVersionResponse'
  { -- | The date and time the schema version was created.
    createdTime :: Core.Maybe Types.CreatedTime,
    -- | The data format of the schema definition. Currently only @AVRO@ is supported.
    dataFormat :: Core.Maybe Types.DataFormat,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The schema definition for the schema ID.
    schemaDefinition :: Core.Maybe Types.SchemaDefinition,
    -- | The @SchemaVersionId@ of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The status of the schema version.
    status :: Core.Maybe Types.SchemaVersionStatus,
    -- | The version number of the schema.
    versionNumber :: Core.Maybe Core.Natural,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaVersionResponse' value with any optional fields omitted.
mkGetSchemaVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSchemaVersionResponse
mkGetSchemaVersionResponse responseStatus =
  GetSchemaVersionResponse'
    { createdTime = Core.Nothing,
      dataFormat = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaDefinition = Core.Nothing,
      schemaVersionId = Core.Nothing,
      status = Core.Nothing,
      versionNumber = Core.Nothing,
      responseStatus
    }

-- | The date and time the schema version was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsCreatedTime :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.CreatedTime)
gsvrrsCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED gsvrrsCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsDataFormat :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.DataFormat)
gsvrrsDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED gsvrrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSchemaArn :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.GlueResourceArn)
gsvrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED gsvrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The schema definition for the schema ID.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSchemaDefinition :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.SchemaDefinition)
gsvrrsSchemaDefinition = Lens.field @"schemaDefinition"
{-# DEPRECATED gsvrrsSchemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead." #-}

-- | The @SchemaVersionId@ of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSchemaVersionId :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.SchemaVersionId)
gsvrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED gsvrrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsStatus :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.SchemaVersionStatus)
gsvrrsStatus = Lens.field @"status"
{-# DEPRECATED gsvrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsVersionNumber :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Natural)
gsvrrsVersionNumber = Lens.field @"versionNumber"
{-# DEPRECATED gsvrrsVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsResponseStatus :: Lens.Lens' GetSchemaVersionResponse Core.Int
gsvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
