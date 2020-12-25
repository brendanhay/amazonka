{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    QuerySchemaVersionMetadata (..),
    mkQuerySchemaVersionMetadata,

    -- ** Request lenses
    qsvmMaxResults,
    qsvmMetadataList,
    qsvmNextToken,
    qsvmSchemaId,
    qsvmSchemaVersionId,
    qsvmSchemaVersionNumber,

    -- * Destructuring the response
    QuerySchemaVersionMetadataResponse (..),
    mkQuerySchemaVersionMetadataResponse,

    -- ** Response lenses
    qsvmrrsMetadataInfoMap,
    qsvmrrsNextToken,
    qsvmrrsSchemaVersionId,
    qsvmrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkQuerySchemaVersionMetadata' smart constructor.
data QuerySchemaVersionMetadata = QuerySchemaVersionMetadata'
  { -- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
    metadataList :: Core.Maybe [Types.MetadataKeyValuePair],
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.SchemaRegistryTokenString,
    -- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
    schemaId :: Core.Maybe Types.SchemaId,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The version number of the schema.
    schemaVersionNumber :: Core.Maybe Types.SchemaVersionNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QuerySchemaVersionMetadata' value with any optional fields omitted.
mkQuerySchemaVersionMetadata ::
  QuerySchemaVersionMetadata
mkQuerySchemaVersionMetadata =
  QuerySchemaVersionMetadata'
    { maxResults = Core.Nothing,
      metadataList = Core.Nothing,
      nextToken = Core.Nothing,
      schemaId = Core.Nothing,
      schemaVersionId = Core.Nothing,
      schemaVersionNumber = Core.Nothing
    }

-- | Maximum number of results required per page. If the value is not supplied, this will be defaulted to 25 per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmMaxResults :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Core.Natural)
qsvmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED qsvmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Search key-value pairs for metadata, if they are not provided all the metadata information will be fetched.
--
-- /Note:/ Consider using 'metadataList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmMetadataList :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe [Types.MetadataKeyValuePair])
qsvmMetadataList = Lens.field @"metadataList"
{-# DEPRECATED qsvmMetadataList "Use generic-lens or generic-optics with 'metadataList' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmNextToken :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaRegistryTokenString)
qsvmNextToken = Lens.field @"nextToken"
{-# DEPRECATED qsvmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaId :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaId)
qsvmSchemaId = Lens.field @"schemaId"
{-# DEPRECATED qsvmSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaVersionId :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaVersionId)
qsvmSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED qsvmSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The version number of the schema.
--
-- /Note:/ Consider using 'schemaVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmSchemaVersionNumber :: Lens.Lens' QuerySchemaVersionMetadata (Core.Maybe Types.SchemaVersionNumber)
qsvmSchemaVersionNumber = Lens.field @"schemaVersionNumber"
{-# DEPRECATED qsvmSchemaVersionNumber "Use generic-lens or generic-optics with 'schemaVersionNumber' instead." #-}

instance Core.FromJSON QuerySchemaVersionMetadata where
  toJSON QuerySchemaVersionMetadata {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("MetadataList" Core..=) Core.<$> metadataList,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SchemaId" Core..=) Core.<$> schemaId,
            ("SchemaVersionId" Core..=) Core.<$> schemaVersionId,
            ("SchemaVersionNumber" Core..=) Core.<$> schemaVersionNumber
          ]
      )

instance Core.AWSRequest QuerySchemaVersionMetadata where
  type
    Rs QuerySchemaVersionMetadata =
      QuerySchemaVersionMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.QuerySchemaVersionMetadata")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          QuerySchemaVersionMetadataResponse'
            Core.<$> (x Core..:? "MetadataInfoMap")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SchemaVersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkQuerySchemaVersionMetadataResponse' smart constructor.
data QuerySchemaVersionMetadataResponse = QuerySchemaVersionMetadataResponse'
  { -- | A map of a metadata key and associated values.
    metadataInfoMap :: Core.Maybe (Core.HashMap Types.MetadataKeyString Types.MetadataInfo),
    -- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The unique version ID of the schema version.
    schemaVersionId :: Core.Maybe Types.SchemaVersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QuerySchemaVersionMetadataResponse' value with any optional fields omitted.
mkQuerySchemaVersionMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  QuerySchemaVersionMetadataResponse
mkQuerySchemaVersionMetadataResponse responseStatus =
  QuerySchemaVersionMetadataResponse'
    { metadataInfoMap =
        Core.Nothing,
      nextToken = Core.Nothing,
      schemaVersionId = Core.Nothing,
      responseStatus
    }

-- | A map of a metadata key and associated values.
--
-- /Note:/ Consider using 'metadataInfoMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsMetadataInfoMap :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe (Core.HashMap Types.MetadataKeyString Types.MetadataInfo))
qsvmrrsMetadataInfoMap = Lens.field @"metadataInfoMap"
{-# DEPRECATED qsvmrrsMetadataInfoMap "Use generic-lens or generic-optics with 'metadataInfoMap' instead." #-}

-- | A continuation token for paginating the returned list of tokens, returned if the current segment of the list is not the last.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsNextToken :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe Types.NextToken)
qsvmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED qsvmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique version ID of the schema version.
--
-- /Note:/ Consider using 'schemaVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsSchemaVersionId :: Lens.Lens' QuerySchemaVersionMetadataResponse (Core.Maybe Types.SchemaVersionId)
qsvmrrsSchemaVersionId = Lens.field @"schemaVersionId"
{-# DEPRECATED qsvmrrsSchemaVersionId "Use generic-lens or generic-optics with 'schemaVersionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qsvmrrsResponseStatus :: Lens.Lens' QuerySchemaVersionMetadataResponse Core.Int
qsvmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED qsvmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
