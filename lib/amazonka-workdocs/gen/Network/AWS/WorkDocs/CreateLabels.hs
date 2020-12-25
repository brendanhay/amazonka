{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateLabels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified list of labels to the given resource (a document or folder)
module Network.AWS.WorkDocs.CreateLabels
  ( -- * Creating a request
    CreateLabels (..),
    mkCreateLabels,

    -- ** Request lenses
    clResourceId,
    clLabels,
    clAuthenticationToken,

    -- * Destructuring the response
    CreateLabelsResponse (..),
    mkCreateLabelsResponse,

    -- ** Response lenses
    clrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateLabels' smart constructor.
data CreateLabels = CreateLabels'
  { -- | The ID of the resource.
    resourceId :: Types.ResourceId,
    -- | List of labels to add to the resource.
    labels :: [Types.SharedLabel],
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLabels' value with any optional fields omitted.
mkCreateLabels ::
  -- | 'resourceId'
  Types.ResourceId ->
  CreateLabels
mkCreateLabels resourceId =
  CreateLabels'
    { resourceId,
      labels = Core.mempty,
      authenticationToken = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clResourceId :: Lens.Lens' CreateLabels Types.ResourceId
clResourceId = Lens.field @"resourceId"
{-# DEPRECATED clResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | List of labels to add to the resource.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clLabels :: Lens.Lens' CreateLabels [Types.SharedLabel]
clLabels = Lens.field @"labels"
{-# DEPRECATED clLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clAuthenticationToken :: Lens.Lens' CreateLabels (Core.Maybe Types.AuthenticationHeaderType)
clAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED clAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.FromJSON CreateLabels where
  toJSON CreateLabels {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Labels" Core..= labels)])

instance Core.AWSRequest CreateLabels where
  type Rs CreateLabels = CreateLabelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/resources/" Core.<> (Core.toText resourceId)
                Core.<> ("/labels")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateLabelsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateLabelsResponse' smart constructor.
newtype CreateLabelsResponse = CreateLabelsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLabelsResponse' value with any optional fields omitted.
mkCreateLabelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLabelsResponse
mkCreateLabelsResponse responseStatus =
  CreateLabelsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clrrsResponseStatus :: Lens.Lens' CreateLabelsResponse Core.Int
clrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED clrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
