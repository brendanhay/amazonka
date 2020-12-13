{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.RemoveAccountFromOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified account from the organization.
--
-- The removed account becomes a standalone account that isn't a member of any organization. It's no longer subject to any policies and is responsible for its own bill payments. The organization's management account is no longer charged for any expenses accrued by the member account after it's removed from the organization.
-- This operation can be called only from the organization's management account. Member accounts can remove themselves with 'LeaveOrganization' instead.
-- /Important:/
--     * You can remove an account from your organization only if the account is configured with the information required to operate as a standalone account. When you create an account in an organization using the AWS Organizations console, API, or CLI commands, the information required of standalone accounts is /not/ automatically collected. For an account that you want to make standalone, you must choose a support plan, provide and verify the required contact information, and provide a current payment method. AWS uses the payment method to charge for any billable (not free tier) AWS activity that occurs while the account isn't attached to an organization. To remove an account that doesn't yet have this information, you must sign in as the member account and follow the steps at <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_remove.html#leave-without-all-info To leave an organization when all required account information has not yet been provided> in the /AWS Organizations User Guide./
--
--
--     * After the account leaves the organization, all tags that were attached to the account object in the organization are deleted. AWS accounts outside of an organization do not support tags.
module Network.AWS.Organizations.RemoveAccountFromOrganization
  ( -- * Creating a request
    RemoveAccountFromOrganization (..),
    mkRemoveAccountFromOrganization,

    -- ** Request lenses
    rafoAccountId,

    -- * Destructuring the response
    RemoveAccountFromOrganizationResponse (..),
    mkRemoveAccountFromOrganizationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveAccountFromOrganization' smart constructor.
newtype RemoveAccountFromOrganization = RemoveAccountFromOrganization'
  { -- | The unique identifier (ID) of the member account that you want to remove from the organization.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAccountFromOrganization' with the minimum fields required to make a request.
--
-- * 'accountId' - The unique identifier (ID) of the member account that you want to remove from the organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
mkRemoveAccountFromOrganization ::
  -- | 'accountId'
  Lude.Text ->
  RemoveAccountFromOrganization
mkRemoveAccountFromOrganization pAccountId_ =
  RemoveAccountFromOrganization' {accountId = pAccountId_}

-- | The unique identifier (ID) of the member account that you want to remove from the organization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rafoAccountId :: Lens.Lens' RemoveAccountFromOrganization Lude.Text
rafoAccountId = Lens.lens (accountId :: RemoveAccountFromOrganization -> Lude.Text) (\s a -> s {accountId = a} :: RemoveAccountFromOrganization)
{-# DEPRECATED rafoAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest RemoveAccountFromOrganization where
  type
    Rs RemoveAccountFromOrganization =
      RemoveAccountFromOrganizationResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull RemoveAccountFromOrganizationResponse'

instance Lude.ToHeaders RemoveAccountFromOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.RemoveAccountFromOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RemoveAccountFromOrganization where
  toJSON RemoveAccountFromOrganization' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AccountId" Lude..= accountId)])

instance Lude.ToPath RemoveAccountFromOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveAccountFromOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveAccountFromOrganizationResponse' smart constructor.
data RemoveAccountFromOrganizationResponse = RemoveAccountFromOrganizationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAccountFromOrganizationResponse' with the minimum fields required to make a request.
mkRemoveAccountFromOrganizationResponse ::
  RemoveAccountFromOrganizationResponse
mkRemoveAccountFromOrganizationResponse =
  RemoveAccountFromOrganizationResponse'
