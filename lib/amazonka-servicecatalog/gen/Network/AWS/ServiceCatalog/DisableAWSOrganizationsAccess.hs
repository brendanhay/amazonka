{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disable portfolio sharing through AWS Organizations feature. This feature will not delete your current shares but it will prevent you from creating new shares throughout your organization. Current shares will not be in sync with your organization structure if it changes after calling this API. This API can only be called by the management account in the organization.
--
-- This API can't be invoked if there are active delegated administrators in the organization.
-- Note that a delegated administrator is not authorized to invoke @DisableAWSOrganizationsAccess@ .
module Network.AWS.ServiceCatalog.DisableAWSOrganizationsAccess
  ( -- * Creating a request
    DisableAWSOrganizationsAccess (..),
    mkDisableAWSOrganizationsAccess,

    -- * Destructuring the response
    DisableAWSOrganizationsAccessResponse (..),
    mkDisableAWSOrganizationsAccessResponse,

    -- ** Response lenses
    dawsoarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkDisableAWSOrganizationsAccess' smart constructor.
data DisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAWSOrganizationsAccess' value with any optional fields omitted.
mkDisableAWSOrganizationsAccess ::
  DisableAWSOrganizationsAccess
mkDisableAWSOrganizationsAccess = DisableAWSOrganizationsAccess'

instance Core.FromJSON DisableAWSOrganizationsAccess where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisableAWSOrganizationsAccess where
  type
    Rs DisableAWSOrganizationsAccess =
      DisableAWSOrganizationsAccessResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWS242ServiceCatalogService.DisableAWSOrganizationsAccess"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableAWSOrganizationsAccessResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisableAWSOrganizationsAccessResponse' smart constructor.
newtype DisableAWSOrganizationsAccessResponse = DisableAWSOrganizationsAccessResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAWSOrganizationsAccessResponse' value with any optional fields omitted.
mkDisableAWSOrganizationsAccessResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableAWSOrganizationsAccessResponse
mkDisableAWSOrganizationsAccessResponse responseStatus =
  DisableAWSOrganizationsAccessResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dawsoarrsResponseStatus :: Lens.Lens' DisableAWSOrganizationsAccessResponse Core.Int
dawsoarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dawsoarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
