{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutSchemaVersionMetadata (..)
    , mkPutSchemaVersionMetadata
    -- ** Request lenses
    , psvmMetadataKeyValue
    , psvmSchemaId
    , psvmSchemaVersionId
    , psvmSchemaVersionNumber

    -- * Destructuring the response
    , PutSchemaVersionMetadataResponse (..)
    , mkPutSchemaVersionMetadataResponse
    -- ** Response lenses
    , psvmrrsLatestVersion
    , psvmrrsMetadataKey
    , psvmrrsMetadataValue
    , psvmrrsRegistryName
    , psvmrrsSchemaArn
    , psvmrrsSchemaName
    , psvmrrsSchemaVersionId
    , psvmrrsVersionNumber
    , psvmrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutSchemaVersionMetadata' smart constructor.
data PutSchemaVersionMetadata = PutSchemaVersionMetadata'
  { metadataKeyValue :: Types.MetadataKeyValuePair
    -- ^ The metadata key's corresponding value.
  , schemaId :: Core.Maybe Types.SchemaId
    -- ^ The unique ID for the schema.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionIdString
    -- ^ The unique version ID of the schema version.
  , schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
    -- ^ The version number of the schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSchemaVersionMetadata' value with any optional fields omitted.
mkPutSchemaVersionMetadata
    :: Types.MetadataKeyValuePair -- ^ 'metadataKeyValue'
    -> PutSchemaVersionMetadata
mkPutSchemaVersionMetadata metadataKeyValue
  = PutSchemaVersionMetadata'{metadataKeyValue,
                              schemaId = Core.Nothing, schemaVersionId = Core.Nothing,
                              schemaVersionNumber = Core.Nothing}

-- | The metadata key's corresponding value.
--
-- /Note:/ Consider using 'metadataKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmMetadataKeyValue :: Lens.Lens' PutSchemaVersionMetadata Types.MetadataKeyValuePair
psvmMetadataKeyValue = Lens.field @"metadataKeyValue"
{-# INLINEABLE psvmMetadataKeyValue #-}
{-# DEPRECATED metadataKeyValue "Use generic-lens or generic-optics with 'metadataKeyValue' instead"  #-}

-- | The unique ID for the schema.
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaId :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Types.SchemaId)
psvmSchemaId = Lens.field @"schemaId"
{-# INLINEABLE psvmSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaVersionId :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Types.SchemaVersionIdString)
psvmSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE psvmSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmSchemaVersionNumber :: Lens.Lens' PutSchemaVersionMetadata (Core.Maybe Types.SchemaVersionNumber)
psvmSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# INLINEABLE psvmSchemaVersionNumber #-}
{-# DEPRECATED schemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead"  #-}

instance Core.ToQuery PutSchemaVersionMetadata where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutSchemaVersionMetadata where
        toHeaders PutSchemaVersionMetadata{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.PutSchemaVersionMetadata")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutSchemaVersionMetadata where
        toJSON PutSchemaVersionMetadata{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MetadataKeyValue" Core..= metadataKeyValue),
                  ("SchemaId" Core..=) Core.<$> schemaId,
                  ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
                  ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber])

instance Core.AWSRequest PutSchemaVersionMetadata where
        type Rs PutSchemaVersionMetadata = PutSchemaVersionMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutSchemaVersionMetadataResponse' Core.<$>
                   (x Core..:? "LatestVersion") Core.<*> x Core..:? "MetadataKey"
                     Core.<*> x Core..:? "MetadataValue"
                     Core.<*> x Core..:? "RegistryName"
                     Core.<*> x Core..:? "SchemaArn"
                     Core.<*> x Core..:? "SchemaName"
                     Core.<*> x Core..:? "SchemaVersionId"
                     Core.<*> x Core..:? "VersionNumber"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutSchemaVersionMetadataResponse' smart constructor.
data PutSchemaVersionMetadataResponse = PutSchemaVersionMetadataResponse'
  { latestVersion :: Core.Maybe Core.Bool
    -- ^ The latest version of the schema.
  , metadataKey :: Core.Maybe Types.MetadataKey
    -- ^ The metadata key.
  , metadataValue :: Core.Maybe Types.MetadataValueString
    -- ^ The value of the metadata key.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ The name for the registry.
  , schemaArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) for the schema.
  , schemaName :: Core.Maybe Types.SchemaName
    -- ^ The name for the schema.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The unique version ID of the schema version.
  , versionNumber :: Core.Maybe Core.Natural
    -- ^ The version number of the schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutSchemaVersionMetadataResponse' value with any optional fields omitted.
mkPutSchemaVersionMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutSchemaVersionMetadataResponse
mkPutSchemaVersionMetadataResponse responseStatus
  = PutSchemaVersionMetadataResponse'{latestVersion = Core.Nothing,
                                      metadataKey = Core.Nothing, metadataValue = Core.Nothing,
                                      registryName = Core.Nothing, schemaArn = Core.Nothing,
                                      schemaName = Core.Nothing, schemaVersionId = Core.Nothing,
                                      versionNumber = Core.Nothing, responseStatus}

-- | The latest version of the schema.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsLatestVersion :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Bool)
psvmrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE psvmrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsMetadataKey :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.MetadataKey)
psvmrrsMetadataKey = Lens.field @"metadataKey"
{-# INLINEABLE psvmrrsMetadataKey #-}
{-# DEPRECATED metadataKey "Use generic-lens or generic-optics with 'metadataKey' instead"  #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsMetadataValue :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.MetadataValueString)
psvmrrsMetadataValue = Lens.field @"metadataValue"
{-# INLINEABLE psvmrrsMetadataValue #-}
{-# DEPRECATED metadataValue "Use generic-lens or generic-optics with 'metadataValue' instead"  #-}

-- | The name for the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsRegistryName :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.RegistryName)
psvmrrsRegistryName = Lens.field @"registryName"
{-# INLINEABLE psvmrrsRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | The Amazon Resource Name (ARN) for the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsSchemaArn :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.GlueResourceArn)
psvmrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE psvmrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The name for the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsSchemaName :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.SchemaName)
psvmrrsSchemaName = Lens.field @"schemaName"
{-# INLINEABLE psvmrrsSchemaName #-}
{-# DEPRECATED schemaName "Use generic-lens or generic-optics with 'schemaName' instead"  #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsSchemaVersionId :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Types.SchemaVersionId)
psvmrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE psvmrrsSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsVersionNumber :: Lens.Lens' PutSchemaVersionMetadataResponse (Core.Maybe Core.Natural)
psvmrrsVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE psvmrrsVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psvmrrsResponseStatus :: Lens.Lens' PutSchemaVersionMetadataResponse Core.Int
psvmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE psvmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
