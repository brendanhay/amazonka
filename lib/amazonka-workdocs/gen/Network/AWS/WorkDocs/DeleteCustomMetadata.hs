{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteCustomMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes custom metadata from the specified resource.
module Network.AWS.WorkDocs.DeleteCustomMetadata
  ( -- * Creating a request
    DeleteCustomMetadata (..),
    mkDeleteCustomMetadata,

    -- ** Request lenses
    dcmResourceId,
    dcmAuthenticationToken,
    dcmDeleteAll,
    dcmKeys,
    dcmVersionId,

    -- * Destructuring the response
    DeleteCustomMetadataResponse (..),
    mkDeleteCustomMetadataResponse,

    -- ** Response lenses
    dcmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { -- | The ID of the resource, either a document or folder.
    resourceId :: Types.ResourceIdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | Flag to indicate removal of all custom metadata properties from the specified resource.
    deleteAll :: Core.Maybe Core.Bool,
    -- | List of properties to remove.
    keys :: Core.Maybe [Types.CustomMetadataKeyType],
    -- | The ID of the version, if the custom metadata is being deleted from a document version.
    versionId :: Core.Maybe Types.DocumentVersionIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomMetadata' value with any optional fields omitted.
mkDeleteCustomMetadata ::
  -- | 'resourceId'
  Types.ResourceIdType ->
  DeleteCustomMetadata
mkDeleteCustomMetadata resourceId =
  DeleteCustomMetadata'
    { resourceId,
      authenticationToken = Core.Nothing,
      deleteAll = Core.Nothing,
      keys = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The ID of the resource, either a document or folder.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmResourceId :: Lens.Lens' DeleteCustomMetadata Types.ResourceIdType
dcmResourceId = Lens.field @"resourceId"
{-# DEPRECATED dcmResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmAuthenticationToken :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Types.AuthenticationHeaderType)
dcmAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dcmAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Flag to indicate removal of all custom metadata properties from the specified resource.
--
-- /Note:/ Consider using 'deleteAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmDeleteAll :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Core.Bool)
dcmDeleteAll = Lens.field @"deleteAll"
{-# DEPRECATED dcmDeleteAll "Use generic-lens or generic-optics with 'deleteAll' instead." #-}

-- | List of properties to remove.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmKeys :: Lens.Lens' DeleteCustomMetadata (Core.Maybe [Types.CustomMetadataKeyType])
dcmKeys = Lens.field @"keys"
{-# DEPRECATED dcmKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The ID of the version, if the custom metadata is being deleted from a document version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmVersionId :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Types.DocumentVersionIdType)
dcmVersionId = Lens.field @"versionId"
{-# DEPRECATED dcmVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest DeleteCustomMetadata where
  type Rs DeleteCustomMetadata = DeleteCustomMetadataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/resources/" Core.<> (Core.toText resourceId)
                Core.<> ("/customMetadata")
            ),
        Core._rqQuery =
          Core.toQueryValue "deleteAll" Core.<$> deleteAll
            Core.<> ( Core.toQueryValue
                        "keys"
                        (Core.toQueryList "member" Core.<$> keys)
                    )
            Core.<> (Core.toQueryValue "versionId" Core.<$> versionId),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomMetadataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteCustomMetadataResponse' smart constructor.
newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomMetadataResponse' value with any optional fields omitted.
mkDeleteCustomMetadataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteCustomMetadataResponse
mkDeleteCustomMetadataResponse responseStatus =
  DeleteCustomMetadataResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcmrrsResponseStatus :: Lens.Lens' DeleteCustomMetadataResponse Core.Int
dcmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
