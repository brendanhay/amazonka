{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSchemaByDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a schema by the @SchemaDefinition@ . The schema definition is sent to the Schema Registry, canonicalized, and hashed. If the hash is matched within the scope of the @SchemaName@ or ARN (or the default registry, if none is supplied), that schema’s metadata is returned. Otherwise, a 404 or NotFound error is returned. Schema versions in @Deleted@ statuses will not be included in the results.
module Network.AWS.Glue.GetSchemaByDefinition
    (
    -- * Creating a request
      GetSchemaByDefinition (..)
    , mkGetSchemaByDefinition
    -- ** Request lenses
    , gsbdSchemaId
    , gsbdSchemaDefinition

    -- * Destructuring the response
    , GetSchemaByDefinitionResponse (..)
    , mkGetSchemaByDefinitionResponse
    -- ** Response lenses
    , gsbdrrsCreatedTime
    , gsbdrrsDataFormat
    , gsbdrrsSchemaArn
    , gsbdrrsSchemaVersionId
    , gsbdrrsStatus
    , gsbdrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaByDefinition' smart constructor.
data GetSchemaByDefinition = GetSchemaByDefinition'
  { schemaId :: Types.SchemaId
    -- ^ This is a wrapper structure to contain schema identity fields. The structure contains:
--
--
--     * SchemaId$SchemaArn: The Amazon Resource Name (ARN) of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
--     * SchemaId$SchemaName: The name of the schema. One of @SchemaArn@ or @SchemaName@ has to be provided.
--
--
  , schemaDefinition :: Types.SchemaDefinitionString
    -- ^ The definition of the schema for which schema details are required.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaByDefinition' value with any optional fields omitted.
mkGetSchemaByDefinition
    :: Types.SchemaId -- ^ 'schemaId'
    -> Types.SchemaDefinitionString -- ^ 'schemaDefinition'
    -> GetSchemaByDefinition
mkGetSchemaByDefinition schemaId schemaDefinition
  = GetSchemaByDefinition'{schemaId, schemaDefinition}

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
gsbdSchemaId :: Lens.Lens' GetSchemaByDefinition Types.SchemaId
gsbdSchemaId = Lens.field @"schemaId"
{-# INLINEABLE gsbdSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The definition of the schema for which schema details are required.
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdSchemaDefinition :: Lens.Lens' GetSchemaByDefinition Types.SchemaDefinitionString
gsbdSchemaDefinition = Lens.field @"schemaDefinition"
{-# INLINEABLE gsbdSchemaDefinition #-}
{-# DEPRECATED schemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead"  #-}

instance Core.ToQuery GetSchemaByDefinition where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSchemaByDefinition where
        toHeaders GetSchemaByDefinition{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetSchemaByDefinition")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSchemaByDefinition where
        toJSON GetSchemaByDefinition{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaId" Core..= schemaId),
                  Core.Just ("SchemaDefinition" Core..= schemaDefinition)])

instance Core.AWSRequest GetSchemaByDefinition where
        type Rs GetSchemaByDefinition = GetSchemaByDefinitionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSchemaByDefinitionResponse' Core.<$>
                   (x Core..:? "CreatedTime") Core.<*> x Core..:? "DataFormat"
                     Core.<*> x Core..:? "SchemaArn"
                     Core.<*> x Core..:? "SchemaVersionId"
                     Core.<*> x Core..:? "Status"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSchemaByDefinitionResponse' smart constructor.
data GetSchemaByDefinitionResponse = GetSchemaByDefinitionResponse'
  { createdTime :: Core.Maybe Types.CreatedTime
    -- ^ The date and time the schema was created.
  , dataFormat :: Core.Maybe Types.DataFormat
    -- ^ The data format of the schema definition. Currently only @AVRO@ is supported.
  , schemaArn :: Core.Maybe Types.GlueResourceArn
    -- ^ The Amazon Resource Name (ARN) of the schema.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The schema ID of the schema version.
  , status :: Core.Maybe Types.SchemaVersionStatus
    -- ^ The status of the schema version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaByDefinitionResponse' value with any optional fields omitted.
mkGetSchemaByDefinitionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSchemaByDefinitionResponse
mkGetSchemaByDefinitionResponse responseStatus
  = GetSchemaByDefinitionResponse'{createdTime = Core.Nothing,
                                   dataFormat = Core.Nothing, schemaArn = Core.Nothing,
                                   schemaVersionId = Core.Nothing, status = Core.Nothing,
                                   responseStatus}

-- | The date and time the schema was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrrsCreatedTime :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Types.CreatedTime)
gsbdrrsCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE gsbdrrsCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | The data format of the schema definition. Currently only @AVRO@ is supported.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrrsDataFormat :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Types.DataFormat)
gsbdrrsDataFormat = Lens.field @"dataFormat"
{-# INLINEABLE gsbdrrsDataFormat #-}
{-# DEPRECATED dataFormat "Use generic-lens or generic-optics with 'dataFormat' instead"  #-}

-- | The Amazon Resource Name (ARN) of the schema.
--
-- /Note:/ Consider using 'schemaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrrsSchemaArn :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Types.GlueResourceArn)
gsbdrrsSchemaArn = Lens.field @"schemaArn"
{-# INLINEABLE gsbdrrsSchemaArn #-}
{-# DEPRECATED schemaArn "Use generic-lens or generic-optics with 'schemaArn' instead"  #-}

-- | The schema ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrrsSchemaVersionId :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Types.SchemaVersionId)
gsbdrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE gsbdrrsSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrrsStatus :: Lens.Lens' GetSchemaByDefinitionResponse (Core.Maybe Types.SchemaVersionStatus)
gsbdrrsStatus = Lens.field @"status"
{-# INLINEABLE gsbdrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsbdrrsResponseStatus :: Lens.Lens' GetSchemaByDefinitionResponse Core.Int
gsbdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsbdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
