{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetSchemaVersion (..)
    , mkGetSchemaVersion
    -- ** Request lenses
    , gsvSchemaId
    , gsvSchemaVersionId
    , gsvSchemaVersionNumber

    -- * Destructuring the response
    , GetSchemaVersionResponse (..)
    , mkGetSchemaVersionResponse
    -- ** Response lenses
    , gsvrrsCreatedTime
    , gsvrrsDataFormat
    , gsvrrsSchemaArn
    , gsvrrsSchemaDefinition
    , gsvrrsSchemaVersionId
    , gsvrrsStatus
    , gsvrrsVersionNumber
    , gsvrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaVersion' smart constructor.
data GetSchemaVersion = GetSchemaVersion'
  { schemaId :: Core.Maybe Types.SchemaId
    -- ^ This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. Either @SchemaArn@ or @SchemaName@ and @RegistryName@ has to be provided.
--
--
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
  , schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
    -- ^ The version number of the schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaVersion' value with any optional fields omitted.
mkGetSchemaVersion
    :: GetSchemaVersion
mkGetSchemaVersion
  = GetSchemaVersion'{schemaId = Core.Nothing,
                      schemaVersionId = Core.Nothing, schemaVersionNumber = Core.Nothing}

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
{-# INLINEABLE gsvSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The @SchemaVersionId@ of the schema version. This field is required for fetching by schema ID. Either this or the @SchemaId@ wrapper has to be provided.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaVersionId :: Lens.Lens' GetSchemaVersion (Core.Maybe Types.SchemaVersionId)
gsvSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE gsvSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvSchemaVersionNumber :: Lens.Lens' GetSchemaVersion (Core.Maybe Types.SchemaVersionNumber)
gsvSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# INLINEABLE gsvSchemaVersionNumber #-}
{-# DEPRECATED schemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead"  #-}

instance Core.ToQuery GetSchemaVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSchemaVersion where
        toHeaders GetSchemaVersion{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetSchemaVersion") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSchemaVersion where
        toJSON GetSchemaVersion{..}
          = Core.object
              (Core.catMaybes
                 [("SchemaId" Core..=) Core.<$> schemaId,
                  ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
                  ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber])

instance Core.AWSRequest GetSchemaVersion where
        type Rs GetSchemaVersion = GetSchemaVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSchemaVersionResponse' Core.<$>
                   (x Core..:? "CreatedTime") Core.<*> x Core..:? "DataFormat"
                     Core.<*> x Core..:? "SchemaArn"
                     Core.<*> x Core..:? "SchemaDefinition"
                     Core.<*> x Core..:? "SchemaVersionId"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "VersionNumber"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSchemaVersionResponse' smart constructor.
data GetSchemaVersionResponse = GetSchemaVersionResponse'
  { createdTime :: Core.Maybe Types.CreatedTime
    -- ^ The date and time the schema version was created.
  , dataFormat :: Core.Maybe Types.DataFormat
    -- ^ The data format of the schema definition. Currently only @AVRO@ is supported.
  , schemaArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the schema.
  , schemaDefinition :: Core.Maybe Types.SchemaDefinition
    -- ^ The schema definition for the schema ID.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The @SchemaVersionId@ of the schema version.
  , status :: Core.Maybe Types.SchemaVersionStatus
    -- ^ The status of the schema version. 
  , versionNumber :: Core.Maybe Core.Natural
    -- ^ The version number of the schema.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaVersionResponse' value with any optional fields omitted.
mkGetSchemaVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSchemaVersionResponse
mkGetSchemaVersionResponse responseStatus
  = GetSchemaVersionResponse'{createdTime = Core.Nothing,
                              dataFormat = Core.Nothing, schemaArn = Core.Nothing,
                              schemaDefinition = Core.Nothing, schemaVersionId = Core.Nothing,
                              status = Core.Nothing, versionNumber = Core.Nothing,
                              responseStatus}

-- | The date and time the schema version was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsCreatedTime :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.CreatedTime)
gsvrrsCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE gsvrrsCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsDataFormat :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.DataFormat)
gsvrrsDataFormat = Lens.field @"dataFormat"
{-# INLINEABLE gsvrrsDataFormat #-}
{-# DEPRECATED dataFormat "Use generic-lens or generic-optics with 'dataFormat' instead"  #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSchemaArn :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.GlueResourceArn)
gsvrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE gsvrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The schema definition for the schema ID.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSchemaDefinition :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.SchemaDefinition)
gsvrrsSchemaDefinition = Lens.field @"schemaDefinition"
{-# INLINEABLE gsvrrsSchemaDefinition #-}
{-# DEPRECATED schemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead"  #-}

-- | The @SchemaVersionId@ of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsSchemaVersionId :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.SchemaVersionId)
gsvrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE gsvrrsSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The status of the schema version. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsStatus :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Types.SchemaVersionStatus)
gsvrrsStatus = Lens.field @"status"
{-# INLINEABLE gsvrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsVersionNumber :: Lens.Lens' GetSchemaVersionResponse (Core.Maybe Core.Natural)
gsvrrsVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE gsvrrsVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsvrrsResponseStatus :: Lens.Lens' GetSchemaVersionResponse Core.Int
gsvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
