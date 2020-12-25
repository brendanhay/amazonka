{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DeleteApplications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of applications and their associations with configuration items.
module Network.AWS.Discovery.DeleteApplications
  ( -- * Creating a request
    DeleteApplications (..),
    mkDeleteApplications,

    -- ** Request lenses
    daConfigurationIds,

    -- * Destructuring the response
    DeleteApplicationsResponse (..),
    mkDeleteApplicationsResponse,

    -- ** Response lenses
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteApplications' smart constructor.
newtype DeleteApplications = DeleteApplications'
  { -- | Configuration ID of an application to be deleted.
    configurationIds :: [Types.ApplicationId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplications' value with any optional fields omitted.
mkDeleteApplications ::
  DeleteApplications
mkDeleteApplications =
  DeleteApplications' {configurationIds = Core.mempty}

-- | Configuration ID of an application to be deleted.
--
-- /Note:/ Consider using 'configurationIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daConfigurationIds :: Lens.Lens' DeleteApplications [Types.ApplicationId]
daConfigurationIds = Lens.field @"configurationIds"
{-# DEPRECATED daConfigurationIds "Use generic-lens or generic-optics with 'configurationIds' instead." #-}

instance Core.FromJSON DeleteApplications where
  toJSON DeleteApplications {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("configurationIds" Core..= configurationIds)]
      )

instance Core.AWSRequest DeleteApplications where
  type Rs DeleteApplications = DeleteApplicationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSPoseidonService_V2015_11_01.DeleteApplications"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteApplicationsResponse' smart constructor.
newtype DeleteApplicationsResponse = DeleteApplicationsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteApplicationsResponse' value with any optional fields omitted.
mkDeleteApplicationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteApplicationsResponse
mkDeleteApplicationsResponse responseStatus =
  DeleteApplicationsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteApplicationsResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
