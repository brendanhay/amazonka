{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DisableAWSServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the integration of an AWS service (the service that is specified by @ServicePrincipal@ ) with AWS Organizations. When you disable integration, the specified service no longer can create a <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role> in /new/ accounts in your organization. This means the service can't perform operations on your behalf on any new accounts in your organization. The service can still perform operations in older accounts until the service completes its clean-up from AWS Organizations.
--
--
-- /Important:/ We recommend that you disable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the other service is aware that it can clean up any resources that are required only for the integration. How the service cleans up its resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.
-- After you perform the @DisableAWSServiceAccess@ operation, the specified service can no longer perform operations in your organization's accounts unless the operations are explicitly permitted by the IAM policies that are attached to your roles.
-- For more information about integrating other services with AWS Organizations, including the list of services that work with Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DisableAWSServiceAccess
  ( -- * Creating a request
    DisableAWSServiceAccess (..),
    mkDisableAWSServiceAccess,

    -- ** Request lenses
    dawssaServicePrincipal,

    -- * Destructuring the response
    DisableAWSServiceAccessResponse (..),
    mkDisableAWSServiceAccessResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableAWSServiceAccess' smart constructor.
newtype DisableAWSServiceAccess = DisableAWSServiceAccess'
  { -- | The service principal name of the AWS service for which you want to disable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
    servicePrincipal :: Types.ServicePrincipal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAWSServiceAccess' value with any optional fields omitted.
mkDisableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Types.ServicePrincipal ->
  DisableAWSServiceAccess
mkDisableAWSServiceAccess servicePrincipal =
  DisableAWSServiceAccess' {servicePrincipal}

-- | The service principal name of the AWS service for which you want to disable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dawssaServicePrincipal :: Lens.Lens' DisableAWSServiceAccess Types.ServicePrincipal
dawssaServicePrincipal = Lens.field @"servicePrincipal"
{-# DEPRECATED dawssaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Core.FromJSON DisableAWSServiceAccess where
  toJSON DisableAWSServiceAccess {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ServicePrincipal" Core..= servicePrincipal)]
      )

instance Core.AWSRequest DisableAWSServiceAccess where
  type Rs DisableAWSServiceAccess = DisableAWSServiceAccessResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.DisableAWSServiceAccess"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DisableAWSServiceAccessResponse'

-- | /See:/ 'mkDisableAWSServiceAccessResponse' smart constructor.
data DisableAWSServiceAccessResponse = DisableAWSServiceAccessResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAWSServiceAccessResponse' value with any optional fields omitted.
mkDisableAWSServiceAccessResponse ::
  DisableAWSServiceAccessResponse
mkDisableAWSServiceAccessResponse =
  DisableAWSServiceAccessResponse'
