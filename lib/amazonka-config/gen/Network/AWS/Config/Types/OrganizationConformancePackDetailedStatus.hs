{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationConformancePackDetailedStatus
  ( OrganizationConformancePackDetailedStatus (..)
  -- * Smart constructor
  , mkOrganizationConformancePackDetailedStatus
  -- * Lenses
  , ocpdsAccountId
  , ocpdsConformancePackName
  , ocpdsStatus
  , ocpdsErrorCode
  , ocpdsErrorMessage
  , ocpdsLastUpdateTime
  ) where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.OrganizationResourceDetailedStatus as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Organization conformance pack creation or deletion status in each member account. This includes the name of the conformance pack, the status, error code and error message when the conformance pack creation or deletion failed. 
--
-- /See:/ 'mkOrganizationConformancePackDetailedStatus' smart constructor.
data OrganizationConformancePackDetailedStatus = OrganizationConformancePackDetailedStatus'
  { accountId :: Types.AccountId
    -- ^ The 12-digit account ID of a member account.
  , conformancePackName :: Types.StringWithCharLimit256
    -- ^ The name of conformance pack deployed in the member account.
  , status :: Types.OrganizationResourceDetailedStatus
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
  , errorCode :: Core.Maybe Core.Text
    -- ^ An error code that is returned when conformance pack creation or deletion failed in the member account. 
  , errorMessage :: Core.Maybe Core.Text
    -- ^ An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account. 
  , lastUpdateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the last status update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OrganizationConformancePackDetailedStatus' value with any optional fields omitted.
mkOrganizationConformancePackDetailedStatus
    :: Types.AccountId -- ^ 'accountId'
    -> Types.StringWithCharLimit256 -- ^ 'conformancePackName'
    -> Types.OrganizationResourceDetailedStatus -- ^ 'status'
    -> OrganizationConformancePackDetailedStatus
mkOrganizationConformancePackDetailedStatus accountId
  conformancePackName status
  = OrganizationConformancePackDetailedStatus'{accountId,
                                               conformancePackName, status,
                                               errorCode = Core.Nothing,
                                               errorMessage = Core.Nothing,
                                               lastUpdateTime = Core.Nothing}

-- | The 12-digit account ID of a member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsAccountId :: Lens.Lens' OrganizationConformancePackDetailedStatus Types.AccountId
ocpdsAccountId = Lens.field @"accountId"
{-# INLINEABLE ocpdsAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of conformance pack deployed in the member account.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsConformancePackName :: Lens.Lens' OrganizationConformancePackDetailedStatus Types.StringWithCharLimit256
ocpdsConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE ocpdsConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

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
ocpdsStatus :: Lens.Lens' OrganizationConformancePackDetailedStatus Types.OrganizationResourceDetailedStatus
ocpdsStatus = Lens.field @"status"
{-# INLINEABLE ocpdsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | An error code that is returned when conformance pack creation or deletion failed in the member account. 
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsErrorCode :: Lens.Lens' OrganizationConformancePackDetailedStatus (Core.Maybe Core.Text)
ocpdsErrorCode = Lens.field @"errorCode"
{-# INLINEABLE ocpdsErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | An error message indicating that conformance pack account creation or deletion has failed due to an error in the member account. 
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsErrorMessage :: Lens.Lens' OrganizationConformancePackDetailedStatus (Core.Maybe Core.Text)
ocpdsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE ocpdsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The timestamp of the last status update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpdsLastUpdateTime :: Lens.Lens' OrganizationConformancePackDetailedStatus (Core.Maybe Core.NominalDiffTime)
ocpdsLastUpdateTime = Lens.field @"lastUpdateTime"
{-# INLINEABLE ocpdsLastUpdateTime #-}
{-# DEPRECATED lastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead"  #-}

instance Core.FromJSON OrganizationConformancePackDetailedStatus
         where
        parseJSON
          = Core.withObject "OrganizationConformancePackDetailedStatus"
              Core.$
              \ x ->
                OrganizationConformancePackDetailedStatus' Core.<$>
                  (x Core..: "AccountId") Core.<*> x Core..: "ConformancePackName"
                    Core.<*> x Core..: "Status"
                    Core.<*> x Core..:? "ErrorCode"
                    Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "LastUpdateTime"
