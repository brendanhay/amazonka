{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified schema in detail.
module Network.AWS.Glue.GetSchema
  ( -- * Creating a request
    GetSchema (..),
    mkGetSchema,

    -- ** Request lenses
    gsSchemaId,

    -- * Destructuring the response
    GetSchemaResponse (..),
    mkGetSchemaResponse,

    -- ** Response lenses
    gsrrsCompatibility,
    gsrrsCreatedTime,
    gsrrsDataFormat,
    gsrrsDescription,
    gsrrsLatestSchemaVersion,
    gsrrsNextSchemaVersion,
    gsrrsRegistryArn,
    gsrrsRegistryName,
    gsrrsSchemaArn,
    gsrrsSchemaCheckpoint,
    gsrrsSchemaName,
    gsrrsSchemaStatus,
    gsrrsUpdatedTime,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchema' smart constructor.
newtype GetSchema = GetSchema'
  { -- | This is a wrapper structure to contain schema identity fields. The structure contains:
    --
    --
    --     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
    --
    --
    --     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
    schemaId :: Types.SchemaId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchema' value with any optional fields omitted.
mkGetSchema ::
  -- | 'schemaId'
  Types.SchemaId ->
  GetSchema
mkGetSchema schemaId = GetSchema' {schemaId}

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
gsSchemaId :: Lens.Lens' GetSchema Types.SchemaId
gsSchemaId = Lens.field @"schemaId"
{-# DEPRECATED gsSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

instance Core.FromJSON GetSchema where
  toJSON GetSchema {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SchemaId" Core..= schemaId)])

instance Core.AWSRequest GetSchema where
  type Rs GetSchema = GetSchemaResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetSchema")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaResponse'
            Core.<$> (x Core..:? "Compatibility")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "DataFormat")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "LatestSchemaVersion")
            Core.<*> (x Core..:? "NextSchemaVersion")
            Core.<*> (x Core..:? "RegistryArn")
            Core.<*> (x Core..:? "RegistryName")
            Core.<*> (x Core..:? "SchemaArn")
            Core.<*> (x Core..:? "SchemaCheckpoint")
            Core.<*> (x Core..:? "SchemaName")
            Core.<*> (x Core..:? "SchemaStatus")
            Core.<*> (x Core..:? "UpdatedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { -- | The compatibility mode of the schema.
    compatibility :: Core.Maybe Types.Compatibility,
    -- | The date and time the schema was created.
    createdTime :: Core.Maybe Types.CreatedTime,
    -- | The data format of the schema definition. Currently only @AVRO@ is supported.
    dataFormat :: Core.Maybe Types.DataFormat,
    -- | A description of schema if specified when created
    description :: Core.Maybe Types.DescriptionString,
    -- | The latest version of the schema associated with the returned schema definition.
    latestSchemaVersion :: Core.Maybe Core.Natural,
    -- | The next version of the schema associated with the returned schema definition.
    nextSchemaVersion :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the registry.
    registryArn :: Core.Maybe Types.GlueResourceArn,
    -- | The name of the registry.
    registryName :: Core.Maybe Types.RegistryName,
    -- | The Amazon Resource Name (ARN) of the schema.
    schemaArn :: Core.Maybe Types.GlueResourceArn,
    -- | The version number of the checkpoint (the last time the compatibility mode was changed).
    schemaCheckpoint :: Core.Maybe Core.Natural,
    -- | The name of the schema.
    schemaName :: Core.Maybe Types.SchemaName,
    -- | The status of the schema.
    schemaStatus :: Core.Maybe Types.SchemaStatus,
    -- | The date and time the schema was updated.
    updatedTime :: Core.Maybe Types.UpdatedTimestamp,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaResponse' value with any optional fields omitted.
mkGetSchemaResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSchemaResponse
mkGetSchemaResponse responseStatus =
  GetSchemaResponse'
    { compatibility = Core.Nothing,
      createdTime = Core.Nothing,
      dataFormat = Core.Nothing,
      description = Core.Nothing,
      latestSchemaVersion = Core.Nothing,
      nextSchemaVersion = Core.Nothing,
      registryArn = Core.Nothing,
      registryName = Core.Nothing,
      schemaArn = Core.Nothing,
      schemaCheckpoint = Core.Nothing,
      schemaName = Core.Nothing,
      schemaStatus = Core.Nothing,
      updatedTime = Core.Nothing,
      responseStatus
    }

-- | The compatibility mode of the schema.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCompatibility :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.Compatibility)
gsrrsCompatibility = Lens.field @"compatibility"
{-# DEPRECATED gsrrsCompatibility "Use generic-lens or generic-optics with 'compatibility' instead." #-}

-- | The date and time the schema was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCreatedTime :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.CreatedTime)
gsrrsCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED gsrrsCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsDataFormat :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.DataFormat)
gsrrsDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED gsrrsDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | A description of schema if specified when created
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsDescription :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.DescriptionString)
gsrrsDescription = Lens.field @"description"
{-# DEPRECATED gsrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The latest version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'latestSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLatestSchemaVersion :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
gsrrsLatestSchemaVersion = Lens.field @"latestSchemaVersion"
{-# DEPRECATED gsrrsLatestSchemaVersion "Use generic-lens or generic-optics with 'latestSchemaVersion' instead." #-}

-- | The next version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'nextSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsNextSchemaVersion :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
gsrrsNextSchemaVersion = Lens.field @"nextSchemaVersion"
{-# DEPRECATED gsrrsNextSchemaVersion "Use generic-lens or generic-optics with 'nextSchemaVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsRegistryArn :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.GlueResourceArn)
gsrrsRegistryArn = Lens.field @"registryArn"
{-# DEPRECATED gsrrsRegistryArn "Use generic-lens or generic-optics with 'registryArn' instead." #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsRegistryName :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.RegistryName)
gsrrsRegistryName = Lens.field @"registryName"
{-# DEPRECATED gsrrsRegistryName "Use generic-lens or generic-optics with 'registryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaArn :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.GlueResourceArn)
gsrrsSchemaArn = Lens.field @"schemaArn"
{-# DEPRECATED gsrrsSchemaArn "Use generic-lens or generic-optics with 'schemaArn' instead." #-}

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- /Note:/ Consider using 'schemaCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaCheckpoint :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
gsrrsSchemaCheckpoint = Lens.field @"schemaCheckpoint"
{-# DEPRECATED gsrrsSchemaCheckpoint "Use generic-lens or generic-optics with 'schemaCheckpoint' instead." #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaName :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.SchemaName)
gsrrsSchemaName = Lens.field @"schemaName"
{-# DEPRECATED gsrrsSchemaName "Use generic-lens or generic-optics with 'schemaName' instead." #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'schemaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaStatus :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.SchemaStatus)
gsrrsSchemaStatus = Lens.field @"schemaStatus"
{-# DEPRECATED gsrrsSchemaStatus "Use generic-lens or generic-optics with 'schemaStatus' instead." #-}

-- | The date and time the schema was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsUpdatedTime :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.UpdatedTimestamp)
gsrrsUpdatedTime = Lens.field @"updatedTime"
{-# DEPRECATED gsrrsUpdatedTime "Use generic-lens or generic-optics with 'updatedTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSchemaResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
