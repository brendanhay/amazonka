{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.RegisterSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new version to the existing schema. Returns an error if new version of schema does not meet the compatibility requirements of the schema set. This API will not create a new schema set and will return a 404 error if the schema set is not already present in the Schema Registry.
--
-- If this is the first schema definition to be registered in the Schema Registry, this API will store the schema version and return immediately. Otherwise, this call has the potential to run longer than other operations due to compatibility modes. You can call the @GetSchemaVersion@ API with the @SchemaVersionId@ to check compatibility modes.
-- If the same schema definition is already stored in Schema Registry as a version, the schema ID of the existing schema is returned to the caller.
module Network.AWS.Glue.RegisterSchemaVersion
    (
    -- * Creating a request
      RegisterSchemaVersion (..)
    , mkRegisterSchemaVersion
    -- ** Request lenses
    , rsvSchemaId
    , rsvSchemaDefinition

    -- * Destructuring the response
    , RegisterSchemaVersionResponse (..)
    , mkRegisterSchemaVersionResponse
    -- ** Response lenses
    , rsvrrsSchemaVersionId
    , rsvrrsStatus
    , rsvrrsVersionNumber
    , rsvrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterSchemaVersion' smart constructor.
data RegisterSchemaVersion = RegisterSchemaVersion'
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
  , schemaDefinition :: Types.SchemaDefinition
    -- ^ The schema definition using the @DataFormat@ setting for the @SchemaName@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterSchemaVersion' value with any optional fields omitted.
mkRegisterSchemaVersion
    :: Types.SchemaId -- ^ 'schemaId'
    -> Types.SchemaDefinition -- ^ 'schemaDefinition'
    -> RegisterSchemaVersion
mkRegisterSchemaVersion schemaId schemaDefinition
  = RegisterSchemaVersion'{schemaId, schemaDefinition}

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
rsvSchemaId :: Lens.Lens' RegisterSchemaVersion Types.SchemaId
rsvSchemaId = Lens.field @"schemaId"
{-# INLINEABLE rsvSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The schema definition using the @DataFormat@ setting for the @SchemaName@ .
--
-- /Note:/ Consider using 'schemaDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvSchemaDefinition :: Lens.Lens' RegisterSchemaVersion Types.SchemaDefinition
rsvSchemaDefinition = Lens.field @"schemaDefinition"
{-# INLINEABLE rsvSchemaDefinition #-}
{-# DEPRECATED schemaDefinition "Use generic-lens or generic-optics with 'schemaDefinition' instead"  #-}

instance Core.ToQuery RegisterSchemaVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterSchemaVersion where
        toHeaders RegisterSchemaVersion{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.RegisterSchemaVersion")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterSchemaVersion where
        toJSON RegisterSchemaVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SchemaId" Core..= schemaId),
                  Core.Just ("SchemaDefinition" Core..= schemaDefinition)])

instance Core.AWSRequest RegisterSchemaVersion where
        type Rs RegisterSchemaVersion = RegisterSchemaVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterSchemaVersionResponse' Core.<$>
                   (x Core..:? "SchemaVersionId") Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "VersionNumber"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterSchemaVersionResponse' smart constructor.
data RegisterSchemaVersionResponse = RegisterSchemaVersionResponse'
  { schemaVersionId :: Core.Maybe Types.SchemaVersionIdString
    -- ^ The unique ID that represents the version of this schema.
  , status :: Core.Maybe Types.SchemaVersionStatus
    -- ^ The status of the schema version.
  , versionNumber :: Core.Maybe Core.Natural
    -- ^ The version of this schema (for sync flow only, in case this is the first version).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterSchemaVersionResponse' value with any optional fields omitted.
mkRegisterSchemaVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterSchemaVersionResponse
mkRegisterSchemaVersionResponse responseStatus
  = RegisterSchemaVersionResponse'{schemaVersionId = Core.Nothing,
                                   status = Core.Nothing, versionNumber = Core.Nothing,
                                   responseStatus}

-- | The unique ID that represents the version of this schema.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrrsSchemaVersionId :: Lens.Lens' RegisterSchemaVersionResponse (Core.Maybe Types.SchemaVersionIdString)
rsvrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE rsvrrsSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The status of the schema version.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrrsStatus :: Lens.Lens' RegisterSchemaVersionResponse (Core.Maybe Types.SchemaVersionStatus)
rsvrrsStatus = Lens.field @"status"
{-# INLINEABLE rsvrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The version of this schema (for sync flow only, in case this is the first version).
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrrsVersionNumber :: Lens.Lens' RegisterSchemaVersionResponse (Core.Maybe Core.Natural)
rsvrrsVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE rsvrrsVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsvrrsResponseStatus :: Lens.Lens' RegisterSchemaVersionResponse Core.Int
rsvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
