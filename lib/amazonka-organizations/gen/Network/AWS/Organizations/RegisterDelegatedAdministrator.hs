{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.RegisterDelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified member account to administer the Organizations features of the specified AWS service. It grants read-only access to AWS Organizations service data. The account still requires IAM permissions to access and administer the AWS service.
--
-- You can run this action only for AWS services that support this feature. For a current list of services that support it, see the column /Supports Delegated Administrator/ in the table at <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrated-services-list.html AWS Services that you can use with AWS Organizations> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.RegisterDelegatedAdministrator
  ( -- * Creating a request
    RegisterDelegatedAdministrator (..),
    mkRegisterDelegatedAdministrator,

    -- ** Request lenses
    rdaAccountId,
    rdaServicePrincipal,

    -- * Destructuring the response
    RegisterDelegatedAdministratorResponse (..),
    mkRegisterDelegatedAdministratorResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterDelegatedAdministrator' smart constructor.
data RegisterDelegatedAdministrator = RegisterDelegatedAdministrator'
  { -- | The account ID number of the member account in the organization to register as a delegated administrator.
    accountId :: Types.AccountId,
    -- | The service principal of the AWS service for which you want to make the member account a delegated administrator.
    servicePrincipal :: Types.ServicePrincipal
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDelegatedAdministrator' value with any optional fields omitted.
mkRegisterDelegatedAdministrator ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'servicePrincipal'
  Types.ServicePrincipal ->
  RegisterDelegatedAdministrator
mkRegisterDelegatedAdministrator accountId servicePrincipal =
  RegisterDelegatedAdministrator' {accountId, servicePrincipal}

-- | The account ID number of the member account in the organization to register as a delegated administrator.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdaAccountId :: Lens.Lens' RegisterDelegatedAdministrator Types.AccountId
rdaAccountId = Lens.field @"accountId"
{-# DEPRECATED rdaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The service principal of the AWS service for which you want to make the member account a delegated administrator.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdaServicePrincipal :: Lens.Lens' RegisterDelegatedAdministrator Types.ServicePrincipal
rdaServicePrincipal = Lens.field @"servicePrincipal"
{-# DEPRECATED rdaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Core.FromJSON RegisterDelegatedAdministrator where
  toJSON RegisterDelegatedAdministrator {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("ServicePrincipal" Core..= servicePrincipal)
          ]
      )

instance Core.AWSRequest RegisterDelegatedAdministrator where
  type
    Rs RegisterDelegatedAdministrator =
      RegisterDelegatedAdministratorResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.RegisterDelegatedAdministrator"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull RegisterDelegatedAdministratorResponse'

-- | /See:/ 'mkRegisterDelegatedAdministratorResponse' smart constructor.
data RegisterDelegatedAdministratorResponse = RegisterDelegatedAdministratorResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDelegatedAdministratorResponse' value with any optional fields omitted.
mkRegisterDelegatedAdministratorResponse ::
  RegisterDelegatedAdministratorResponse
mkRegisterDelegatedAdministratorResponse =
  RegisterDelegatedAdministratorResponse'
