-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
  ( OrganizationResourceDetailedStatusFilters (..),

    -- * Smart constructor
    mkOrganizationResourceDetailedStatusFilters,

    -- * Lenses
    ordsfStatus,
    ordsfAccountId,
  )
where

import Network.AWS.Config.Types.OrganizationResourceDetailedStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Status filter object to filter results based on specific member account ID or status type for an organization conformance pack.
--
-- /See:/ 'mkOrganizationResourceDetailedStatusFilters' smart constructor.
data OrganizationResourceDetailedStatusFilters = OrganizationResourceDetailedStatusFilters'
  { status ::
      Lude.Maybe
        OrganizationResourceDetailedStatus,
    accountId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrganizationResourceDetailedStatusFilters' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit account ID of the member account within an organization.
-- * 'status' - Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.
--
--
--     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
mkOrganizationResourceDetailedStatusFilters ::
  OrganizationResourceDetailedStatusFilters
mkOrganizationResourceDetailedStatusFilters =
  OrganizationResourceDetailedStatusFilters'
    { status = Lude.Nothing,
      accountId = Lude.Nothing
    }

-- | Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when conformance pack has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when conformance pack is being created in the member account.
--
--
--     * @CREATE_FAILED@ when conformance pack creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when conformance pack is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when conformance pack has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when conformance pack has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when conformance pack is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when conformance pack deletion has failed in the member account.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ordsfStatus :: Lens.Lens' OrganizationResourceDetailedStatusFilters (Lude.Maybe OrganizationResourceDetailedStatus)
ordsfStatus = Lens.lens (status :: OrganizationResourceDetailedStatusFilters -> Lude.Maybe OrganizationResourceDetailedStatus) (\s a -> s {status = a} :: OrganizationResourceDetailedStatusFilters)
{-# DEPRECATED ordsfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The 12-digit account ID of the member account within an organization.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ordsfAccountId :: Lens.Lens' OrganizationResourceDetailedStatusFilters (Lude.Maybe Lude.Text)
ordsfAccountId = Lens.lens (accountId :: OrganizationResourceDetailedStatusFilters -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: OrganizationResourceDetailedStatusFilters)
{-# DEPRECATED ordsfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.ToJSON OrganizationResourceDetailedStatusFilters where
  toJSON OrganizationResourceDetailedStatusFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Status" Lude..=) Lude.<$> status,
            ("AccountId" Lude..=) Lude.<$> accountId
          ]
      )
