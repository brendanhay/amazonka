{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of development endpoint names. After calling the @ListDevEndpoints@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetDevEndpoints
  ( -- * Creating a request
    BatchGetDevEndpoints (..),
    mkBatchGetDevEndpoints,

    -- ** Request lenses
    bgdeDevEndpointNames,

    -- * Destructuring the response
    BatchGetDevEndpointsResponse (..),
    mkBatchGetDevEndpointsResponse,

    -- ** Response lenses
    bgderrsDevEndpoints,
    bgderrsDevEndpointsNotFound,
    bgderrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetDevEndpoints' smart constructor.
newtype BatchGetDevEndpoints = BatchGetDevEndpoints'
  { -- | The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
    devEndpointNames :: Core.NonEmpty Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetDevEndpoints' value with any optional fields omitted.
mkBatchGetDevEndpoints ::
  -- | 'devEndpointNames'
  Core.NonEmpty Types.GenericString ->
  BatchGetDevEndpoints
mkBatchGetDevEndpoints devEndpointNames =
  BatchGetDevEndpoints' {devEndpointNames}

-- | The list of @DevEndpoint@ names, which might be the names returned from the @ListDevEndpoint@ operation.
--
-- /Note:/ Consider using 'devEndpointNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgdeDevEndpointNames :: Lens.Lens' BatchGetDevEndpoints (Core.NonEmpty Types.GenericString)
bgdeDevEndpointNames = Lens.field @"devEndpointNames"
{-# DEPRECATED bgdeDevEndpointNames "Use generic-lens or generic-optics with 'devEndpointNames' instead." #-}

instance Core.FromJSON BatchGetDevEndpoints where
  toJSON BatchGetDevEndpoints {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DevEndpointNames" Core..= devEndpointNames)]
      )

instance Core.AWSRequest BatchGetDevEndpoints where
  type Rs BatchGetDevEndpoints = BatchGetDevEndpointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.BatchGetDevEndpoints")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetDevEndpointsResponse'
            Core.<$> (x Core..:? "DevEndpoints")
            Core.<*> (x Core..:? "DevEndpointsNotFound")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkBatchGetDevEndpointsResponse' smart constructor.
data BatchGetDevEndpointsResponse = BatchGetDevEndpointsResponse'
  { -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Core.Maybe [Types.DevEndpoint],
    -- | A list of @DevEndpoints@ not found.
    devEndpointsNotFound :: Core.Maybe (Core.NonEmpty Types.GenericString),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchGetDevEndpointsResponse' value with any optional fields omitted.
mkBatchGetDevEndpointsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  BatchGetDevEndpointsResponse
mkBatchGetDevEndpointsResponse responseStatus =
  BatchGetDevEndpointsResponse'
    { devEndpoints = Core.Nothing,
      devEndpointsNotFound = Core.Nothing,
      responseStatus
    }

-- | A list of @DevEndpoint@ definitions.
--
-- /Note:/ Consider using 'devEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgderrsDevEndpoints :: Lens.Lens' BatchGetDevEndpointsResponse (Core.Maybe [Types.DevEndpoint])
bgderrsDevEndpoints = Lens.field @"devEndpoints"
{-# DEPRECATED bgderrsDevEndpoints "Use generic-lens or generic-optics with 'devEndpoints' instead." #-}

-- | A list of @DevEndpoints@ not found.
--
-- /Note:/ Consider using 'devEndpointsNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgderrsDevEndpointsNotFound :: Lens.Lens' BatchGetDevEndpointsResponse (Core.Maybe (Core.NonEmpty Types.GenericString))
bgderrsDevEndpointsNotFound = Lens.field @"devEndpointsNotFound"
{-# DEPRECATED bgderrsDevEndpointsNotFound "Use generic-lens or generic-optics with 'devEndpointsNotFound' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgderrsResponseStatus :: Lens.Lens' BatchGetDevEndpointsResponse Core.Int
bgderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED bgderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
