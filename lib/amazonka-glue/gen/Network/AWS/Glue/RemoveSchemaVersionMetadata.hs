{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RemoveSchemaVersionMetadata (..)
    , mkRemoveSchemaVersionMetadata
    -- ** Request lenses
    , rsvmMetadataKeyValue
    , rsvmSchemaId
    , rsvmSchemaVersionId
    , rsvmSchemaVersionNumber

    -- * Destructuring the response
    , RemoveSchemaVersionMetadataResponse (..)
    , mkRemoveSchemaVersionMetadataResponse
    -- ** Response lenses
    , rsvmrrsLatestVersion
    , rsvmrrsMetadataKey
    , rsvmrrsMetadataValue
    , rsvmrrsRegistryName
    , rsvmrrsSchemaArn
    , rsvmrrsSchemaName
    , rsvmrrsSchemaVersionId
    , rsvmrrsVersionNumber
    , rsvmrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveSchemaVersionMetadata' smart constructor.
data RemoveSchemaVersionMetadata = RemoveSchemaVersionMetadata'
  { metadataKeyValue :: Types.MetadataKeyValuePair
    -- ^ The value of the metadata key.
  , schemaId :: Core.Maybe Types.SchemaId
    -- ^ A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The unique version ID of the schema version.
  , schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
    -- ^ The version number of the schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveSchemaVersionMetadata' value with any optional fields omitted.
mkRemoveSchemaVersionMetadata
    :: Types.MetadataKeyValuePair -- ^ 'metadataKeyValue'
    -> RemoveSchemaVersionMetadata
mkRemoveSchemaVersionMetadata metadataKeyValue
  = RemoveSchemaVersionMetadata'{metadataKeyValue,
                                 schemaId = Core.Nothing, schemaVersionId = Core.Nothing,
                                 schemaVersionNumber = Core.Nothing}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataKeyValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmMetadataKeyValue :: Lens.Lens' RemoveSchemaVersionMetadata Types.MetadataKeyValuePair
rsvmMetadataKeyValue = Lens.field @"metadataKeyValue"
{-# INLINEABLE rsvmMetadataKeyValue #-}
{-# DEPRECATED metadataKeyValue "Use generic-lens or generic-optics with 'metadataKeyValue' instead"  #-}

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaId :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Types.SchemaId)
rsvmSchemaId = Lens.field @"schemaId"
{-# INLINEABLE rsvmSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Types.SchemaVersionId)
rsvmSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE rsvmSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmSchemaVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadata (Core.Maybe Types.SchemaVersionNumber)
rsvmSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# INLINEABLE rsvmSchemaVersionNumber #-}
{-# DEPRECATED schemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead"  #-}

instance Core.ToQuery RemoveSchemaVersionMetadata where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveSchemaVersionMetadata where
        toHeaders RemoveSchemaVersionMetadata{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.RemoveSchemaVersionMetadata")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveSchemaVersionMetadata where
        toJSON RemoveSchemaVersionMetadata{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MetadataKeyValue" Core..= metadataKeyValue),
                  ("SchemaId" Core..=) Core.<$> schemaId,
                  ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
                  ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber])

instance Core.AWSRequest RemoveSchemaVersionMetadata where
        type Rs RemoveSchemaVersionMetadata =
             RemoveSchemaVersionMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RemoveSchemaVersionMetadataResponse' Core.<$>
                   (x Core..:? "LatestVersion") Core.<*> x Core..:? "MetadataKey"
                     Core.<*> x Core..:? "MetadataValue"
                     Core.<*> x Core..:? "RegistryName"
                     Core.<*> x Core..:? "SchemaArn"
                     Core.<*> x Core..:? "SchemaName"
                     Core.<*> x Core..:? "SchemaVersionId"
                     Core.<*> x Core..:? "VersionNumber"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveSchemaVersionMetadataResponse' smart constructor.
data RemoveSchemaVersionMetadataResponse = RemoveSchemaVersionMetadataResponse'
  { latestVersion :: Core.Maybe Core.Bool
    -- ^ The latest version of the schema.
  , metadataKey :: Core.Maybe Types.MetadataKey
    -- ^ The metadata key.
  , metadataValue :: Core.Maybe Types.MetadataValueString
    -- ^ The value of the metadata key.
  , registryName :: Core.Maybe Types.RegistryName
    -- ^ The name of the registry.
  , schemaArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the schema.
  , schemaName :: Core.Maybe Types.SchemaName
    -- ^ The name of the schema.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The version ID for the schema version.
  , versionNumber :: Core.Maybe Core.Natural
    -- ^ The version number of the schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveSchemaVersionMetadataResponse' value with any optional fields omitted.
mkRemoveSchemaVersionMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveSchemaVersionMetadataResponse
mkRemoveSchemaVersionMetadataResponse responseStatus
  = RemoveSchemaVersionMetadataResponse'{latestVersion =
                                           Core.Nothing,
                                         metadataKey = Core.Nothing, metadataValue = Core.Nothing,
                                         registryName = Core.Nothing, schemaArn = Core.Nothing,
                                         schemaName = Core.Nothing, schemaVersionId = Core.Nothing,
                                         versionNumber = Core.Nothing, responseStatus}

-- | The latest version of the schema.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsLatestVersion :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Bool)
rsvmrrsLatestVersion = Lens.field @"latestVersion"
{-# INLINEABLE rsvmrrsLatestVersion #-}
{-# DEPRECATED latestVersion "Use generic-lens or generic-optics with 'latestVersion' instead"  #-}

-- | The metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsMetadataKey :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.MetadataKey)
rsvmrrsMetadataKey = Lens.field @"metadataKey"
{-# INLINEABLE rsvmrrsMetadataKey #-}
{-# DEPRECATED metadataKey "Use generic-lens or generic-optics with 'metadataKey' instead"  #-}

-- | The value of the metadata key.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsMetadataValue :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.MetadataValueString)
rsvmrrsMetadataValue = Lens.field @"metadataValue"
{-# INLINEABLE rsvmrrsMetadataValue #-}
{-# DEPRECATED metadataValue "Use generic-lens or generic-optics with 'metadataValue' instead"  #-}

-- | The name of the registry.
--
-- /Note:/ Consider using 'registryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsRegistryName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.RegistryName)
rsvmrrsRegistryName = Lens.field @"registryName"
{-# INLINEABLE rsvmrrsRegistryName #-}
{-# DEPRECATED registryName "Use generic-lens or generic-optics with 'registryName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsSchemaArn :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.GlueResourceArn)
rsvmrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE rsvmrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The name of the schema.
--
-- /Note:/ Consider using 'schemaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsSchemaName :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.SchemaName)
rsvmrrsSchemaName = Lens.field @"schemaName"
{-# INLINEABLE rsvmrrsSchemaName #-}
{-# DEPRECATED schemaName "Use generic-lens or generic-optics with 'schemaName' instead"  #-}

-- | The version ID for the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsSchemaVersionId :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Types.SchemaVersionId)
rsvmrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE rsvmrrsSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsVersionNumber :: Lens.Lens' RemoveSchemaVersionMetadataResponse (Core.Maybe Core.Natural)
rsvmrrsVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE rsvmrrsVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvmrrsResponseStatus :: Lens.Lens' RemoveSchemaVersionMetadataResponse Core.Int
rsvmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsvmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
