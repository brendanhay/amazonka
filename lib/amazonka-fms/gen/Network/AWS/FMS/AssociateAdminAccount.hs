{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.AssociateAdminAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the AWS Firewall Manager administrator account. AWS Firewall Manager must be associated with the master account of your AWS organization or associated with a member account that has the appropriate permissions. If the account ID that you submit is not an AWS Organizations master account, AWS Firewall Manager will set the appropriate permissions for the given member account.
--
-- The account that you associate with AWS Firewall Manager is called the AWS Firewall Manager administrator account. 
module Network.AWS.FMS.AssociateAdminAccount
    (
    -- * Creating a request
      AssociateAdminAccount (..)
    , mkAssociateAdminAccount
    -- ** Request lenses
    , aaaAdminAccount

    -- * Destructuring the response
    , AssociateAdminAccountResponse (..)
    , mkAssociateAdminAccountResponse
    ) where

import qualified Network.AWS.FMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateAdminAccount' smart constructor.
newtype AssociateAdminAccount = AssociateAdminAccount'
  { adminAccount :: Types.AdminAccount
    -- ^ The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateAdminAccount' value with any optional fields omitted.
mkAssociateAdminAccount
    :: Types.AdminAccount -- ^ 'adminAccount'
    -> AssociateAdminAccount
mkAssociateAdminAccount adminAccount
  = AssociateAdminAccount'{adminAccount}

-- | The AWS account ID to associate with AWS Firewall Manager as the AWS Firewall Manager administrator account. This can be an AWS Organizations master account or a member account. For more information about AWS Organizations and master accounts, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts.html Managing the AWS Accounts in Your Organization> . 
--
-- /Note:/ Consider using 'adminAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaAdminAccount :: Lens.Lens' AssociateAdminAccount Types.AdminAccount
aaaAdminAccount = Lens.field @"adminAccount"
{-# INLINEABLE aaaAdminAccount #-}
{-# DEPRECATED adminAccount "Use generic-lens or generic-optics with 'adminAccount' instead"  #-}

instance Core.ToQuery AssociateAdminAccount where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateAdminAccount where
        toHeaders AssociateAdminAccount{..}
          = Core.pure
              ("X-Amz-Target", "AWSFMS_20180101.AssociateAdminAccount")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateAdminAccount where
        toJSON AssociateAdminAccount{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AdminAccount" Core..= adminAccount)])

instance Core.AWSRequest AssociateAdminAccount where
        type Rs AssociateAdminAccount = AssociateAdminAccountResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AssociateAdminAccountResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateAdminAccountResponse' smart constructor.
data AssociateAdminAccountResponse = AssociateAdminAccountResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateAdminAccountResponse' value with any optional fields omitted.
mkAssociateAdminAccountResponse
    :: AssociateAdminAccountResponse
mkAssociateAdminAccountResponse = AssociateAdminAccountResponse'
