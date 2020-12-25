{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeleteResourcesByExternalId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes resources linked to an external ID.
module Network.AWS.CodeDeploy.DeleteResourcesByExternalId
  ( -- * Creating a request
    DeleteResourcesByExternalId (..),
    mkDeleteResourcesByExternalId,

    -- ** Request lenses
    drbeiExternalId,

    -- * Destructuring the response
    DeleteResourcesByExternalIdResponse (..),
    mkDeleteResourcesByExternalIdResponse,

    -- ** Response lenses
    drbeirrsResponseStatus,
  )
where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteResourcesByExternalId' smart constructor.
newtype DeleteResourcesByExternalId = DeleteResourcesByExternalId'
  { -- | The unique ID of an external resource (for example, a CloudFormation stack ID) that is linked to one or more CodeDeploy resources.
    externalId :: Core.Maybe Types.ExternalId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcesByExternalId' value with any optional fields omitted.
mkDeleteResourcesByExternalId ::
  DeleteResourcesByExternalId
mkDeleteResourcesByExternalId =
  DeleteResourcesByExternalId' {externalId = Core.Nothing}

-- | The unique ID of an external resource (for example, a CloudFormation stack ID) that is linked to one or more CodeDeploy resources.
--
-- /Note:/ Consider using 'externalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbeiExternalId :: Lens.Lens' DeleteResourcesByExternalId (Core.Maybe Types.ExternalId)
drbeiExternalId = Lens.field @"externalId"
{-# DEPRECATED drbeiExternalId "Use generic-lens or generic-optics with 'externalId' instead." #-}

instance Core.FromJSON DeleteResourcesByExternalId where
  toJSON DeleteResourcesByExternalId {..} =
    Core.object
      (Core.catMaybes [("externalId" Core..=) Core.<$> externalId])

instance Core.AWSRequest DeleteResourcesByExternalId where
  type
    Rs DeleteResourcesByExternalId =
      DeleteResourcesByExternalIdResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeDeploy_20141006.DeleteResourcesByExternalId")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcesByExternalIdResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteResourcesByExternalIdResponse' smart constructor.
newtype DeleteResourcesByExternalIdResponse = DeleteResourcesByExternalIdResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcesByExternalIdResponse' value with any optional fields omitted.
mkDeleteResourcesByExternalIdResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteResourcesByExternalIdResponse
mkDeleteResourcesByExternalIdResponse responseStatus =
  DeleteResourcesByExternalIdResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drbeirrsResponseStatus :: Lens.Lens' DeleteResourcesByExternalIdResponse Core.Int
drbeirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drbeirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
