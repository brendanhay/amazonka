{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified list of labels from a resource.
module Network.AWS.WorkDocs.DeleteLabels
  ( -- * Creating a request
    DeleteLabels (..),
    mkDeleteLabels,

    -- ** Request lenses
    dlResourceId,
    dlAuthenticationToken,
    dlDeleteAll,
    dlLabels,

    -- * Destructuring the response
    DeleteLabelsResponse (..),
    mkDeleteLabelsResponse,

    -- ** Response lenses
    dlrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteLabels' smart constructor.
data DeleteLabels = DeleteLabels'
  { -- | The ID of the resource.
    resourceId :: Types.ResourceIdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | Flag to request removal of all labels from the specified resource.
    deleteAll :: Core.Maybe Core.Bool,
    -- | List of labels to delete from the resource.
    labels :: Core.Maybe [Types.SharedLabel]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLabels' value with any optional fields omitted.
mkDeleteLabels ::
  -- | 'resourceId'
  Types.ResourceIdType ->
  DeleteLabels
mkDeleteLabels resourceId =
  DeleteLabels'
    { resourceId,
      authenticationToken = Core.Nothing,
      deleteAll = Core.Nothing,
      labels = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlResourceId :: Lens.Lens' DeleteLabels Types.ResourceIdType
dlResourceId = Lens.field @"resourceId"
{-# DEPRECATED dlResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlAuthenticationToken :: Lens.Lens' DeleteLabels (Core.Maybe Types.AuthenticationHeaderType)
dlAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dlAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Flag to request removal of all labels from the specified resource.
--
-- /Note:/ Consider using 'deleteAll' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlDeleteAll :: Lens.Lens' DeleteLabels (Core.Maybe Core.Bool)
dlDeleteAll = Lens.field @"deleteAll"
{-# DEPRECATED dlDeleteAll "Use generic-lens or generic-optics with 'deleteAll' instead." #-}

-- | List of labels to delete from the resource.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLabels :: Lens.Lens' DeleteLabels (Core.Maybe [Types.SharedLabel])
dlLabels = Lens.field @"labels"
{-# DEPRECATED dlLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

instance Core.AWSRequest DeleteLabels where
  type Rs DeleteLabels = DeleteLabelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/resources/" Core.<> (Core.toText resourceId)
                Core.<> ("/labels")
            ),
        Core._rqQuery =
          Core.toQueryValue "deleteAll" Core.<$> deleteAll
            Core.<> ( Core.toQueryValue
                        "labels"
                        (Core.toQueryList "member" Core.<$> labels)
                    ),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLabelsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteLabelsResponse' smart constructor.
newtype DeleteLabelsResponse = DeleteLabelsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLabelsResponse' value with any optional fields omitted.
mkDeleteLabelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteLabelsResponse
mkDeleteLabelsResponse responseStatus =
  DeleteLabelsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrrsResponseStatus :: Lens.Lens' DeleteLabelsResponse Core.Int
dlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
