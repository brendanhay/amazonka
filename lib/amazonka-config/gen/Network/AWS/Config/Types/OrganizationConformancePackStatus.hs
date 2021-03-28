{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.OrganizationConformancePackStatus
  ( OrganizationConformancePackStatus (..)
  -- * Smart constructor
  , mkOrganizationConformancePackStatus
  -- * Lenses
  , ocpsOrganizationConformancePackName
  , ocpsStatus
  , ocpsErrorCode
  , ocpsErrorMessage
  , ocpsLastUpdateTime
  ) where

import qualified Network.AWS.Config.Types.OrganizationConformancePackName as Types
import qualified Network.AWS.Config.Types.OrganizationResourceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns the status for an organization conformance pack in an organization.
--
-- /See:/ 'mkOrganizationConformancePackStatus' smart constructor.
data OrganizationConformancePackStatus = OrganizationConformancePackStatus'
  { organizationConformancePackName :: Types.OrganizationConformancePackName
    -- ^ The name that you assign to organization conformance pack.
  , status :: Types.OrganizationResourceStatus
    -- ^ Indicates deployment status of an organization conformance pack. When master account calls PutOrganizationConformancePack for the first time, conformance pack status is created in all the member accounts. When master account calls PutOrganizationConformancePack for the second time, conformance pack status is updated in all the member accounts. Additionally, conformance pack status is updated when one or more member accounts join or leave an organization. Conformance pack status is deleted when the master account deletes OrganizationConformancePack in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when an organization conformance pack has been successfully created in all the member accounts. 
--
--
--     * @CREATE_IN_PROGRESS@ when an organization conformance pack creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization conformance pack creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization conformance pack deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization conformance pack deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization conformance pack has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization conformance pack has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization conformance pack update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization conformance pack update failed in one or more member accounts within that organization.
--
--
  , errorCode :: Core.Maybe Core.Text
    -- ^ An error code that is returned when organization conformance pack creation or deletion has failed in a member account. 
  , errorMessage :: Core.Maybe Core.Text
    -- ^ An error message indicating that organization conformance pack creation or deletion failed due to an error. 
  , lastUpdateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp of the last update.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OrganizationConformancePackStatus' value with any optional fields omitted.
mkOrganizationConformancePackStatus
    :: Types.OrganizationConformancePackName -- ^ 'organizationConformancePackName'
    -> Types.OrganizationResourceStatus -- ^ 'status'
    -> OrganizationConformancePackStatus
mkOrganizationConformancePackStatus organizationConformancePackName
  status
  = OrganizationConformancePackStatus'{organizationConformancePackName,
                                       status, errorCode = Core.Nothing,
                                       errorMessage = Core.Nothing, lastUpdateTime = Core.Nothing}

-- | The name that you assign to organization conformance pack.
--
-- /Note:/ Consider using 'organizationConformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsOrganizationConformancePackName :: Lens.Lens' OrganizationConformancePackStatus Types.OrganizationConformancePackName
ocpsOrganizationConformancePackName = Lens.field @"organizationConformancePackName"
{-# INLINEABLE ocpsOrganizationConformancePackName #-}
{-# DEPRECATED organizationConformancePackName "Use generic-lens or generic-optics with 'organizationConformancePackName' instead"  #-}

-- | Indicates deployment status of an organization conformance pack. When master account calls PutOrganizationConformancePack for the first time, conformance pack status is created in all the member accounts. When master account calls PutOrganizationConformancePack for the second time, conformance pack status is updated in all the member accounts. Additionally, conformance pack status is updated when one or more member accounts join or leave an organization. Conformance pack status is deleted when the master account deletes OrganizationConformancePack in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the conformance pack to:
--
--     * @CREATE_SUCCESSFUL@ when an organization conformance pack has been successfully created in all the member accounts. 
--
--
--     * @CREATE_IN_PROGRESS@ when an organization conformance pack creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization conformance pack creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization conformance pack deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization conformance pack deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization conformance pack has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization conformance pack has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization conformance pack update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization conformance pack update failed in one or more member accounts within that organization.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsStatus :: Lens.Lens' OrganizationConformancePackStatus Types.OrganizationResourceStatus
ocpsStatus = Lens.field @"status"
{-# INLINEABLE ocpsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | An error code that is returned when organization conformance pack creation or deletion has failed in a member account. 
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsErrorCode :: Lens.Lens' OrganizationConformancePackStatus (Core.Maybe Core.Text)
ocpsErrorCode = Lens.field @"errorCode"
{-# INLINEABLE ocpsErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | An error message indicating that organization conformance pack creation or deletion failed due to an error. 
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsErrorMessage :: Lens.Lens' OrganizationConformancePackStatus (Core.Maybe Core.Text)
ocpsErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE ocpsErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The timestamp of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocpsLastUpdateTime :: Lens.Lens' OrganizationConformancePackStatus (Core.Maybe Core.NominalDiffTime)
ocpsLastUpdateTime = Lens.field @"lastUpdateTime"
{-# INLINEABLE ocpsLastUpdateTime #-}
{-# DEPRECATED lastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead"  #-}

instance Core.FromJSON OrganizationConformancePackStatus where
        parseJSON
          = Core.withObject "OrganizationConformancePackStatus" Core.$
              \ x ->
                OrganizationConformancePackStatus' Core.<$>
                  (x Core..: "OrganizationConformancePackName") Core.<*>
                    x Core..: "Status"
                    Core.<*> x Core..:? "ErrorCode"
                    Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "LastUpdateTime"
