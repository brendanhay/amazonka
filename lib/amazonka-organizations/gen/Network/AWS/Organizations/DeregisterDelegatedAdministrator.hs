{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeregisterDelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member AWS account as a delegated administrator for the specified AWS service.
--
-- /Important:/ Deregistering a delegated administrator can have unintended impacts on the functionality of the enabled AWS service. See the documentation for the enabled service before you deregister a delegated administrator so that you understand any potential impacts.
-- You can run this action only for AWS services that support this feature. For a current list of services that support it, see the column /Supports Delegated Administrator/ in the table at <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrated-services-list.html AWS Services that you can use with AWS Organizations> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DeregisterDelegatedAdministrator
  ( -- * Creating a request
    DeregisterDelegatedAdministrator (..),
    mkDeregisterDelegatedAdministrator,

    -- ** Request lenses
    ddaAccountId,
    ddaServicePrincipal,

    -- * Destructuring the response
    DeregisterDelegatedAdministratorResponse (..),
    mkDeregisterDelegatedAdministratorResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterDelegatedAdministrator' smart constructor.
data DeregisterDelegatedAdministrator = DeregisterDelegatedAdministrator'
  { -- | The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
    accountId :: Types.AccountId,
    -- | The service principal name of an AWS service for which the account is a delegated administrator.
    --
    -- Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
    servicePrincipal :: Types.ServicePrincipal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterDelegatedAdministrator' value with any optional fields omitted.
mkDeregisterDelegatedAdministrator ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'servicePrincipal'
  Types.ServicePrincipal ->
  DeregisterDelegatedAdministrator
mkDeregisterDelegatedAdministrator accountId servicePrincipal =
  DeregisterDelegatedAdministrator' {accountId, servicePrincipal}

-- | The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddaAccountId :: Lens.Lens' DeregisterDelegatedAdministrator Types.AccountId
ddaAccountId = Lens.field @"accountId"
{-# DEPRECATED ddaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The service principal name of an AWS service for which the account is a delegated administrator.
--
-- Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddaServicePrincipal :: Lens.Lens' DeregisterDelegatedAdministrator Types.ServicePrincipal
ddaServicePrincipal = Lens.field @"servicePrincipal"
{-# DEPRECATED ddaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Core.FromJSON DeregisterDelegatedAdministrator where
  toJSON DeregisterDelegatedAdministrator {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("ServicePrincipal" Core..= servicePrincipal)
          ]
      )

instance Core.AWSRequest DeregisterDelegatedAdministrator where
  type
    Rs DeregisterDelegatedAdministrator =
      DeregisterDelegatedAdministratorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.DeregisterDelegatedAdministrator"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull DeregisterDelegatedAdministratorResponse'

-- | /See:/ 'mkDeregisterDelegatedAdministratorResponse' smart constructor.
data DeregisterDelegatedAdministratorResponse = DeregisterDelegatedAdministratorResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterDelegatedAdministratorResponse' value with any optional fields omitted.
mkDeregisterDelegatedAdministratorResponse ::
  DeregisterDelegatedAdministratorResponse
mkDeregisterDelegatedAdministratorResponse =
  DeregisterDelegatedAdministratorResponse'
