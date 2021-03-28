{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetSchema (..)
    , mkGetSchema
    -- ** Request lenses
    , gsSchemaId

    -- * Destructuring the response
    , GetSchemaResponse (..)
    , mkGetSchemaResponse
    -- ** Response lenses
    , gsrrsCompatibility
    , gsrrsCreatedTime
    , gsrrsDataFormat
    , gsrrsDescription
    , gsrrsLatestSchemaVersion
    , gsrrsNextSchemaVersion
    , gsrrsRegistryArn
    , gsrrsRegistryName
    , gsrrsSchemaArn
    , gsrrsSchemaCheckpoint
    , gsrrsSchemaName
    , gsrrsSchemaStatus
    , gsrrsUpdatedTime
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchema' smart constructor.
newtype GetSchema = GetSchema'
  { schemaId :: Types.SchemaId
    -- ^ This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchema' value with any optional fields omitted.
mkGetSchema
    :: Types.SchemaId -- ^ 'schemaId'
    -> GetSchema
mkGetSchema schemaId = GetSchema'{schemaId}

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
{-# INLINEABLE gsSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

instance Core.ToQuery GetSchema where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSchema where
        toHeaders GetSchema{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetSchema") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSchema where
        toJSON GetSchema{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SchemaId" Core..= schemaId)])

instance Core.AWSRequest GetSchema where
        type Rs GetSchema = GetSchemaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSchemaResponse' Core.<$>
                   (x Core..:? "Compatibility") Core.<*> x Core..:? "CreatedTime"
                     Core.<*> x Core..:? "DataFormat"
                     Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "LatestSchemaVersion"
                     Core.<*> x Core..:? "NextSchemaVersion"
                     Core.<*> x Core..:? "RegistryArn"
                     Core.<*> x Core..:? "RegistryName"
                     Core.<*> x Core..:? "SchemaArn"
                     Core.<*> x Core..:? "SchemaCheckpoint"
                     Core.<*> x Core..:? "SchemaName"
                     Core.<*> x Core..:? "SchemaStatus"
                     Core.<*> x Core..:? "UpdatedTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { compatibility :: Core.Maybe Types.Compatibility
    -- ^ The compatibility mode of the schema.
  , createdTime :: Core.Maybe Types.CreatedTime
    -- ^ The date and time the schema was created.
  , dataFormat :: Core.Maybe Types.DataFormat
    -- ^ The data format of the schema definition. Currently only @AVRO@ is supported.
  , description :: Core.Maybe Types.DescriptionString
    -- ^ A description of schema if specified when created
  , latestSchemaVersion :: Core.Maybe Core.Natural
    -- ^ The latest version of the schema associated with the returned schema definition.
  , nextSchemaVersion :: Core.Maybe Core.Natural
    -- ^ The next version of the schema associated with the returned schema definition.
  , registryArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the registry.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ The name of the registry.
  , schemaArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the schema.
  , schemaCheckpoint :: Core.Maybe Core.Natural
    -- ^ The version number of the checkpoint (the last time the compatibility mode was changed).
  , schemaName :: Core.Maybe Types.SchemaName
    -- ^ The name of the schema.
  , schemaStatus :: Core.Maybe Types.SchemaStatus
    -- ^ The status of the schema.
  , updatedTime :: Core.Maybe Types.UpdatedTimestamp
    -- ^ The date and time the schema was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaResponse' value with any optional fields omitted.
mkGetSchemaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSchemaResponse
mkGetSchemaResponse responseStatus
  = GetSchemaResponse'{compatibility = Core.Nothing,
                       createdTime = Core.Nothing, dataFormat = Core.Nothing,
                       description = Core.Nothing, latestSchemaVersion = Core.Nothing,
                       nextSchemaVersion = Core.Nothing, registryArn = Core.Nothing,
                       registryName = Core.Nothing, schemaArn = Core.Nothing,
                       schemaCheckpoint = Core.Nothing, schemaName = Core.Nothing,
                       schemaStatus = Core.Nothing, updatedTime = Core.Nothing,
                       responseStatus}

-- | The compatibility mode of the schema.
--
-- /Note:/ Consider using 'compatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCompatibility :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.Compatibility)
gsrrsCompatibility = Lens.field @"compatibility"
{-# INLINEABLE gsrrsCompatibility #-}
{-# DEPRECATED compatibility "Use generic-lens or generic-optics with 'compatibility' instead"  #-}

-- | The date and time the schema was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCreatedTime :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.CreatedTime)
gsrrsCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE gsrrsCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsDataFormat :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.DataFormat)
gsrrsDataFormat = Lens.field @"dataFormat"
{-# INLINEABLE gsrrsDataFormat #-}
{-# DEPRECATED dataFormat "Use generic-lens or generic-optics with 'dataFormat' instead"  #-}

-- | A description of schema if specified when created
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsDescription :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.DescriptionString)
gsrrsDescription = Lens.field @"description"
{-# INLINEABLE gsrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The latest version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'latestSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLatestSchemaVersion :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
gsrrsLatestSchemaVersion = Lens.field @"latestSchemaVersion"
{-# INLINEABLE gsrrsLatestSchemaVersion #-}
{-# DEPRECATED latestSchemaVersion "Use generic-lens or generic-optics with 'latestSchemaVersion' instead"  #-}

-- | The next version of the schema associated with the returned schema definition.
--
-- /Note:/ Consider using 'nextSchemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsNextSchemaVersion :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
gsrrsNextSchemaVersion = Lens.field @"nextSchemaVersion"
{-# INLINEABLE gsrrsNextSchemaVersion #-}
{-# DEPRECATED nextSchemaVersion "Use generic-lens or generic-optics with 'nextSchemaVersion' instead"  #-}

-- | The Amazon Resource Name (ARN) of the registry.
--
-- /Note:/ Consider using 'registryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsRegistryArn :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.GlueResourceArn)
gsrrsRegistryArn = Lens.field @"registryArn"
{-# INLINEABLE gsrrsRegistryArn #-}
{-# DEPRECATED registryArn "Use generic-lens or generic-optics with 'registryArn' instead"  #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsRegistryName :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.RegistryName)
gsrrsRegistryName = Lens.field @"registryName"
{-# INLINEABLE gsrrsRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaArn :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.GlueResourceArn)
gsrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE gsrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The version number of the checkpoint (the last time the compatibility mode was changed).
--
-- /Note:/ Consider using 'schemaCheckpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaCheckpoint :: Lens.Lens' GetSchemaResponse (Core.Maybe Core.Natural)
gsrrsSchemaCheckpoint = Lens.field @"schemaCheckpoint"
{-# INLINEABLE gsrrsSchemaCheckpoint #-}
{-# DEPRECATED schemaCheckpoint "Use generic-lens or generic-optics with 'schemaCheckpoint' instead"  #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaName :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.SchemaName)
gsrrsSchemaName = Lens.field @"schemaName"
{-# INLINEABLE gsrrsSchemaName #-}
{-# DEPRECATED schemaName "Use generic-lens or generic-optics with 'schemaName' instead"  #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'schemaStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSchemaStatus :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.SchemaStatus)
gsrrsSchemaStatus = Lens.field @"schemaStatus"
{-# INLINEABLE gsrrsSchemaStatus #-}
{-# DEPRECATED schemaStatus "Use generic-lens or generic-optics with 'schemaStatus' instead"  #-}

-- | The date and time the schema was updated.
--
-- /Note:/ Consider using 'updatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsUpdatedTime :: Lens.Lens' GetSchemaResponse (Core.Maybe Types.UpdatedTimestamp)
gsrrsUpdatedTime = Lens.field @"updatedTime"
{-# INLINEABLE gsrrsUpdatedTime #-}
{-# DEPRECATED updatedTime "Use generic-lens or generic-optics with 'updatedTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSchemaResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
