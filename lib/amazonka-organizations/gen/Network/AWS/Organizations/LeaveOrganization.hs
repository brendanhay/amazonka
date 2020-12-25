{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.LeaveOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member account from its parent organization. This version of the operation is performed by the account that wants to leave. To remove a member account as a user in the management account, use 'RemoveAccountFromOrganization' instead.
--
-- This operation can be called only from a member account in the organization.
-- /Important:/
--     * The management account in an organization with all features enabled can set service control policies (SCPs) that can restrict what administrators of member accounts can do. This includes preventing them from successfully calling @LeaveOrganization@ and leaving the organization.
--
--
--     * You can leave an organization as a member account only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is /not/ automatically collected. For each account that you want to make standalone, you must perform the following steps. If any of the steps are already completed for this account, that step doesn't appear.
--
--     * Choose a support plan
--
--
--     * Provide and verify the required contact information
--
--
--     * Provide a current payment method
--
--
-- AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account isn't attached to an organization. Follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./
--
--
--     * You can leave an organization only after you enable IAM user access to billing in your account. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/grantaccess.html#ControllingAccessWebsite-Activate Activating Access to the Billing and Cost Management Console> in the /AWS Billing and Cost Management User Guide./
--
--
--     * After the account leaves the organization, all tags that were attached to the account object in the organization are deleted. AWS accounts outside of an organization do not support tags.
module Network.AWS.Organizations.LeaveOrganization
  ( -- * Creating a request
    LeaveOrganization (..),
    mkLeaveOrganization,

    -- * Destructuring the response
    LeaveOrganizationResponse (..),
    mkLeaveOrganizationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkLeaveOrganization' smart constructor.
data LeaveOrganization = LeaveOrganization'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LeaveOrganization' value with any optional fields omitted.
mkLeaveOrganization ::
  LeaveOrganization
mkLeaveOrganization = LeaveOrganization'

instance Core.FromJSON LeaveOrganization where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest LeaveOrganization where
  type Rs LeaveOrganization = LeaveOrganizationResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.LeaveOrganization")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull LeaveOrganizationResponse'

-- | /See:/ 'mkLeaveOrganizationResponse' smart constructor.
data LeaveOrganizationResponse = LeaveOrganizationResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LeaveOrganizationResponse' value with any optional fields omitted.
mkLeaveOrganizationResponse ::
  LeaveOrganizationResponse
mkLeaveOrganizationResponse = LeaveOrganizationResponse'
