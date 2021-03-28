{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationResourceDetailedStatusFilters
  ( OrganizationResourceDetailedStatusFilters (..)
  -- * Smart constructor
  , mkOrganizationResourceDetailedStatusFilters
  -- * Lenses
  , ordsfAccountId
  , ordsfStatus
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.OrganizationResourceDetailedStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status filter object to filter results based on specific member account ID or status type for an organization conformance pack.
--
-- /See:/ 'mkOrganizationResourceDetailedStatusFilters' smart constructor.
data OrganizationResourceDetailedStatusFilters = OrganizationResourceDetailedStatusFilters'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The 12-digit account ID of the member account within an organization.
  , status :: Core.Maybe Types.OrganizationResourceDetailedStatus
    -- ^ Indicates deployment status for conformance pack in a member account. When master account calls @PutOrganizationConformancePack@ action for the first time, conformance pack status is created in the member account. When master account calls @PutOrganizationConformancePack@ action for the second time, conformance pack status is updated in the member account. Conformance pack status is deleted when the master account deletes @OrganizationConformancePack@ and disables service access for @config-multiaccountsetup.amazonaws.com@ . 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OrganizationResourceDetailedStatusFilters' value with any optional fields omitted.
mkOrganizationResourceDetailedStatusFilters
    :: OrganizationResourceDetailedStatusFilters
mkOrganizationResourceDetailedStatusFilters
  = OrganizationResourceDetailedStatusFilters'{accountId =
                                                 Core.Nothing,
                                               status = Core.Nothing}

-- | The 12-digit account ID of the member account within an organization.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ordsfAccountId :: Lens.Lens' OrganizationResourceDetailedStatusFilters (Core.Maybe Types.AccountId)
ordsfAccountId = Lens.field @"accountId"
{-# INLINEABLE ordsfAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

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
ordsfStatus :: Lens.Lens' OrganizationResourceDetailedStatusFilters (Core.Maybe Types.OrganizationResourceDetailedStatus)
ordsfStatus = Lens.field @"status"
{-# INLINEABLE ordsfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON OrganizationResourceDetailedStatusFilters
         where
        toJSON OrganizationResourceDetailedStatusFilters{..}
          = Core.object
              (Core.catMaybes
                 [("AccountId" Core..=) Core.<$> accountId,
                  ("Status" Core..=) Core.<$> status])
