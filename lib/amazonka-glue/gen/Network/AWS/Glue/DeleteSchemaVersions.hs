{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteSchemaVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove versions from the specified schema. A version number or range may be supplied. If the compatibility mode forbids deleting of a version that is necessary, such as BACKWARDS_FULL, an error is returned. Calling the @GetSchemaVersions@ API after this call will list the status of the deleted versions.
--
-- When the range of version numbers contain check pointed version, the API will return a 409 conflict and will not proceed with the deletion. You have to remove the checkpoint first using the @DeleteSchemaCheckpoint@ API before using this API.
-- You cannot use the @DeleteSchemaVersions@ API to delete the first schema version in the schema set. The first schema version can only be deleted by the @DeleteSchema@ API. This operation will also delete the attached @SchemaVersionMetadata@ under the schema versions. Hard deletes will be enforced on the database.
-- If the compatibility mode forbids deleting of a version that is necessary, such as BACKWARDS_FULL, an error is returned.
module Network.AWS.Glue.DeleteSchemaVersions
  ( -- * Creating a request
    DeleteSchemaVersions (..),
    mkDeleteSchemaVersions,

    -- ** Request lenses
    dsvSchemaId,
    dsvVersions,

    -- * Destructuring the response
    DeleteSchemaVersionsResponse (..),
    mkDeleteSchemaVersionsResponse,

    -- ** Response lenses
    dsvrrsSchemaVersionErrors,
    dsvrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteSchemaVersions' smart constructor.
data DeleteSchemaVersions = DeleteSchemaVersions'
  { -- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
    schemaId :: Types.SchemaId,
    -- | A version range may be supplied which may be of the format:
    --
    --
    --     * a single version number, 5
    --
    --
    --     * a range, 5-8 : deletes versions 5, 6, 7, 8
    versions :: Types.VersionsString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchemaVersions' value with any optional fields omitted.
mkDeleteSchemaVersions ::
  -- | 'schemaId'
  Types.SchemaId ->
  -- | 'versions'
  Types.VersionsString ->
  DeleteSchemaVersions
mkDeleteSchemaVersions schemaId versions =
  DeleteSchemaVersions' {schemaId, versions}

-- | This is a wrapper structure that may contain the schema name and Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'schemaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvSchemaId :: Lens.Lens' DeleteSchemaVersions Types.SchemaId
dsvSchemaId = Lens.field @"schemaId"
{-# DEPRECATED dsvSchemaId "Use generic-lens or generic-optics with 'schemaId' instead." #-}

-- | A version range may be supplied which may be of the format:
--
--
--     * a single version number, 5
--
--
--     * a range, 5-8 : deletes versions 5, 6, 7, 8
--
--
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvVersions :: Lens.Lens' DeleteSchemaVersions Types.VersionsString
dsvVersions = Lens.field @"versions"
{-# DEPRECATED dsvVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

instance Core.FromJSON DeleteSchemaVersions where
  toJSON DeleteSchemaVersions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaId" Core..= schemaId),
            Core.Just ("Versions" Core..= versions)
          ]
      )

instance Core.AWSRequest DeleteSchemaVersions where
  type Rs DeleteSchemaVersions = DeleteSchemaVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.DeleteSchemaVersions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSchemaVersionsResponse'
            Core.<$> (x Core..:? "SchemaVersionErrors")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteSchemaVersionsResponse' smart constructor.
data DeleteSchemaVersionsResponse = DeleteSchemaVersionsResponse'
  { -- | A list of @SchemaVersionErrorItem@ objects, each containing an error and schema version.
    schemaVersionErrors :: Core.Maybe [Types.SchemaVersionErrorItem],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSchemaVersionsResponse' value with any optional fields omitted.
mkDeleteSchemaVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteSchemaVersionsResponse
mkDeleteSchemaVersionsResponse responseStatus =
  DeleteSchemaVersionsResponse'
    { schemaVersionErrors = Core.Nothing,
      responseStatus
    }

-- | A list of @SchemaVersionErrorItem@ objects, each containing an error and schema version.
--
-- /Note:/ Consider using 'schemaVersionErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvrrsSchemaVersionErrors :: Lens.Lens' DeleteSchemaVersionsResponse (Core.Maybe [Types.SchemaVersionErrorItem])
dsvrrsSchemaVersionErrors = Lens.field @"schemaVersionErrors"
{-# DEPRECATED dsvrrsSchemaVersionErrors "Use generic-lens or generic-optics with 'schemaVersionErrors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvrrsResponseStatus :: Lens.Lens' DeleteSchemaVersionsResponse Core.Int
dsvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
