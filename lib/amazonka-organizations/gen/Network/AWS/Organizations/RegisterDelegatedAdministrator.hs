{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RegisterDelegatedAdministrator (..)
    , mkRegisterDelegatedAdministrator
    -- ** Request lenses
    , rdaAccountId
    , rdaServicePrincipal

    -- * Destructuring the response
    , RegisterDelegatedAdministratorResponse (..)
    , mkRegisterDelegatedAdministratorResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterDelegatedAdministrator' smart constructor.
data RegisterDelegatedAdministrator = RegisterDelegatedAdministrator'
  { accountId :: Types.AccountId
    -- ^ The account ID number of the member account in the organization to register as a delegated administrator.
  , servicePrincipal :: Types.ServicePrincipal
    -- ^ The service principal of the AWS service for which you want to make the member account a delegated administrator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDelegatedAdministrator' value with any optional fields omitted.
mkRegisterDelegatedAdministrator
    :: Types.AccountId -- ^ 'accountId'
    -> Types.ServicePrincipal -- ^ 'servicePrincipal'
    -> RegisterDelegatedAdministrator
mkRegisterDelegatedAdministrator accountId servicePrincipal
  = RegisterDelegatedAdministrator'{accountId, servicePrincipal}

-- | The account ID number of the member account in the organization to register as a delegated administrator.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdaAccountId :: Lens.Lens' RegisterDelegatedAdministrator Types.AccountId
rdaAccountId = Lens.field @"accountId"
{-# INLINEABLE rdaAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The service principal of the AWS service for which you want to make the member account a delegated administrator.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdaServicePrincipal :: Lens.Lens' RegisterDelegatedAdministrator Types.ServicePrincipal
rdaServicePrincipal = Lens.field @"servicePrincipal"
{-# INLINEABLE rdaServicePrincipal #-}
{-# DEPRECATED servicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead"  #-}

instance Core.ToQuery RegisterDelegatedAdministrator where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterDelegatedAdministrator where
        toHeaders RegisterDelegatedAdministrator{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.RegisterDelegatedAdministrator")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterDelegatedAdministrator where
        toJSON RegisterDelegatedAdministrator{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("ServicePrincipal" Core..= servicePrincipal)])

instance Core.AWSRequest RegisterDelegatedAdministrator where
        type Rs RegisterDelegatedAdministrator =
             RegisterDelegatedAdministratorResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull RegisterDelegatedAdministratorResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterDelegatedAdministratorResponse' smart constructor.
data RegisterDelegatedAdministratorResponse = RegisterDelegatedAdministratorResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterDelegatedAdministratorResponse' value with any optional fields omitted.
mkRegisterDelegatedAdministratorResponse
    :: RegisterDelegatedAdministratorResponse
mkRegisterDelegatedAdministratorResponse
  = RegisterDelegatedAdministratorResponse'
