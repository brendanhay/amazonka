{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.QuerySchemaVersionMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Queries for the schema version metadata information. 
module Network.AWS.Glue.QuerySchemaVersionMetadata
    (
    -- * Creating a request
      QuerySchemaVersionMetadata (..)
    , mkQuerySchemaVersionMetadata
    -- ** Request lenses
    , qsvmMaxResults
    , qsvmMetadataList
    , qsvmNextToken
    , qsvmSchemaId
    , qsvmSchemaVersionId
    , qsvmSchemaVersionNumber

    -- * Destructuring the response
    , QuerySchemaVersionMetadataResponse (..)
    , mkQuerySchemaVersionMetadataResponse
    -- ** Response lenses
    , qsvmrrsMetadataInfoMap
    , qsvmrrsNextToken
    , qsvmrrsSchemaVersionId
    , qsvmrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkQuerySchemaVersionMetadata' smart constructor.
data QuerySchemaVersionMetadata = QuerySchemaVersionMetadata'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
  , metadataList :: Core.Maybe [Types.MetadataKeyValuePair]
    -- ^ Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
  , nextToken :: Core.Maybe Types.SchemaRegistryTokenString
    -- ^ A continuation token, if this is a continuation call.
  , schemaId :: Core.Maybe Types.SchemaId
    -- ^ A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The unique version ID of the schema version.
  , schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
    -- ^ The version number of the schema.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QuerySchemaVersionMetadata' value with any optional fields omitted.
mkQuerySchemaVersionMetadata
    :: QuerySchemaVersionMetadata
mkQuerySchemaVersionMetadata
  = QuerySchemaVersionMetadata'{maxResults = Core.Nothing,
                                metadataList = Core.Nothing, nextToken = Core.Nothing,
                                schemaId = Core.Nothing, schemaVersionId = Core.Nothing,
                                schemaVersionNumber = Core.Nothing}

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmMaxResults :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Core.Natural)
qsvmMaxResults = Lens.field @"maxResults"
{-# INLINEABLE qsvmMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
--
-- /Note:/ Consider using 'metadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmMetadataList :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe [Types.MetadataKeyValuePair])
qsvmMetadataList = Lens.field @"metadataList"
{-# INLINEABLE qsvmMetadataList #-}
{-# DEPRECATED metadataList "Use generic-lens or generic-optics with 'metadataList' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmNextToken :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaRegistryTokenString)
qsvmNextToken = Lens.field @"nextToken"
{-# INLINEABLE qsvmNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaId :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaId)
qsvmSchemaId = Lens.field @"schemaId"
{-# INLINEABLE qsvmSchemaId #-}
{-# DEPRECATED schemaId "Use generic-lens or generic-optics with 'schemaId' instead"  #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaVersionId :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaVersionId)
qsvmSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE qsvmSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaVersionNumber :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaVersionNumber)
qsvmSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# INLINEABLE qsvmSchemaVersionNumber #-}
{-# DEPRECATED schemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead"  #-}

instance Core.ToQuery QuerySchemaVersionMetadata where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders QuerySchemaVersionMetadata where
        toHeaders QuerySchemaVersionMetadata{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.QuerySchemaVersionMetadata")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON QuerySchemaVersionMetadata where
        toJSON QuerySchemaVersionMetadata{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("MetadataList" Core..=) Core.<$> metadataList,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("SchemaId" Core..=) Core.<$> schemaId,
                  ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
                  ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber])

instance Core.AWSRequest QuerySchemaVersionMetadata where
        type Rs QuerySchemaVersionMetadata =
             QuerySchemaVersionMetadataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 QuerySchemaVersionMetadataResponse' Core.<$>
                   (x Core..:? "MetadataInfoMap") Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "SchemaVersionId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkQuerySchemaVersionMetadataResponse' smart constructor.
data QuerySchemaVersionMetadataResponse = QuerySchemaVersionMetadataResponse'
  { metadataInfoMap :: Core.Maybe (Core.HashMap Types.MetadataKeyString Types.MetadataInfo)
    -- ^ A map of a metadata key and associated values.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
  , schemaVersionId :: Core.Maybe Types.SchemaVersionId
    -- ^ The unique version ID of the schema version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QuerySchemaVersionMetadataResponse' value with any optional fields omitted.
mkQuerySchemaVersionMetadataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> QuerySchemaVersionMetadataResponse
mkQuerySchemaVersionMetadataResponse responseStatus
  = QuerySchemaVersionMetadataResponse'{metadataInfoMap =
                                          Core.Nothing,
                                        nextToken = Core.Nothing, schemaVersionId = Core.Nothing,
                                        responseStatus}

-- | A map of a metadata key and associated values.
--
-- /Note:/ Consider using 'metadataInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsMetadataInfoMap :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe (Core.HashMap Types.MetadataKeyString Types.MetadataInfo))
qsvmrrsMetadataInfoMap = Lens.field @"metadataInfoMap"
{-# INLINEABLE qsvmrrsMetadataInfoMap #-}
{-# DEPRECATED metadataInfoMap "Use generic-lens or generic-optics with 'metadataInfoMap' instead"  #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsNextToken :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe Types.NextToken)
qsvmrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE qsvmrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsSchemaVersionId :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe Types.SchemaVersionId)
qsvmrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# INLINEABLE qsvmrrsSchemaVersionId #-}
{-# DEPRECATED schemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsResponseStatus :: Lens.Lens' QuerySchemaVersionMetadataResponse Core.Int
qsvmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE qsvmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
