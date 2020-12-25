{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.EnableAWSServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the integration of an AWS service (the service that is specified by @ServicePrincipal@ ) with AWS Organizations. When you enable integration, you allow the specified service to create a <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role> in all the accounts in your organization. This allows the service to perform operations on your behalf in your organization and its accounts.
--
-- /Important:/ We recommend that you enable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the service is aware that it can create the resources that are required for the integration. How the service creates those resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.
-- For more information about enabling services to integrate with AWS Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account and only if the organization has <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html enabled all features> .
module Network.AWS.Organizations.EnableAWSServiceAccess
  ( -- * Creating a request
    EnableAWSServiceAccess (..),
    mkEnableAWSServiceAccess,

    -- ** Request lenses
    eawssaServicePrincipal,

    -- * Destructuring the response
    EnableAWSServiceAccessResponse (..),
    mkEnableAWSServiceAccessResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableAWSServiceAccess' smart constructor.
newtype EnableAWSServiceAccess = EnableAWSServiceAccess'
  { -- | The service principal name of the AWS service for which you want to enable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
    servicePrincipal :: Types.ServicePrincipal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAWSServiceAccess' value with any optional fields omitted.
mkEnableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Types.ServicePrincipal ->
  EnableAWSServiceAccess
mkEnableAWSServiceAccess servicePrincipal =
  EnableAWSServiceAccess' {servicePrincipal}

-- | The service principal name of the AWS service for which you want to enable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eawssaServicePrincipal :: Lens.Lens' EnableAWSServiceAccess Types.ServicePrincipal
eawssaServicePrincipal = Lens.field @"servicePrincipal"
{-# DEPRECATED eawssaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Core.FromJSON EnableAWSServiceAccess where
  toJSON EnableAWSServiceAccess {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ServicePrincipal" Core..= servicePrincipal)]
      )

instance Core.AWSRequest EnableAWSServiceAccess where
  type Rs EnableAWSServiceAccess = EnableAWSServiceAccessResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.EnableAWSServiceAccess"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull EnableAWSServiceAccessResponse'

-- | /See:/ 'mkEnableAWSServiceAccessResponse' smart constructor.
data EnableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAWSServiceAccessResponse' value with any optional fields omitted.
mkEnableAWSServiceAccessResponse ::
  EnableAWSServiceAccessResponse
mkEnableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'
