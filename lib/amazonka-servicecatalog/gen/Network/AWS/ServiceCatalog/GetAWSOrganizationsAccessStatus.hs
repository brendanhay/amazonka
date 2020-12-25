{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the Access Status for AWS Organization portfolio share feature. This API can only be called by the management account in the organization or by a delegated admin.
module Network.AWS.ServiceCatalog.GetAWSOrganizationsAccessStatus
  ( -- * Creating a request
    GetAWSOrganizationsAccessStatus (..),
    mkGetAWSOrganizationsAccessStatus,

    -- * Destructuring the response
    GetAWSOrganizationsAccessStatusResponse (..),
    mkGetAWSOrganizationsAccessStatusResponse,

    -- ** Response lenses
    gawsoasrrsAccessStatus,
    gawsoasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkGetAWSOrganizationsAccessStatus' smart constructor.
data GetAWSOrganizationsAccessStatus = GetAWSOrganizationsAccessStatus'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAWSOrganizationsAccessStatus' value with any optional fields omitted.
mkGetAWSOrganizationsAccessStatus ::
  GetAWSOrganizationsAccessStatus
mkGetAWSOrganizationsAccessStatus =
  GetAWSOrganizationsAccessStatus'

instance Core.FromJSON GetAWSOrganizationsAccessStatus where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetAWSOrganizationsAccessStatus where
  type
    Rs GetAWSOrganizationsAccessStatus =
      GetAWSOrganizationsAccessStatusResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.GetAWSOrganizationsAccessStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAWSOrganizationsAccessStatusResponse'
            Core.<$> (x Core..:? "AccessStatus") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAWSOrganizationsAccessStatusResponse' smart constructor.
data GetAWSOrganizationsAccessStatusResponse = GetAWSOrganizationsAccessStatusResponse'
  { -- | The status of the portfolio share feature.
    accessStatus :: Core.Maybe Types.AccessStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAWSOrganizationsAccessStatusResponse' value with any optional fields omitted.
mkGetAWSOrganizationsAccessStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetAWSOrganizationsAccessStatusResponse
mkGetAWSOrganizationsAccessStatusResponse responseStatus =
  GetAWSOrganizationsAccessStatusResponse'
    { accessStatus =
        Core.Nothing,
      responseStatus
    }

-- | The status of the portfolio share feature.
--
-- /Note:/ Consider using 'accessStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gawsoasrrsAccessStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse (Core.Maybe Types.AccessStatus)
gawsoasrrsAccessStatus = Lens.field @"accessStatus"
{-# DEPRECATED gawsoasrrsAccessStatus "Use generic-lens or generic-optics with 'accessStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gawsoasrrsResponseStatus :: Lens.Lens' GetAWSOrganizationsAccessStatusResponse Core.Int
gawsoasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gawsoasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
