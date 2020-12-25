{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleStatus
  ( OrganizationConfigRuleStatus (..),

    -- * Smart constructor
    mkOrganizationConfigRuleStatus,

    -- * Lenses
    ocrsOrganizationConfigRuleName,
    ocrsOrganizationRuleStatus,
    ocrsErrorCode,
    ocrsErrorMessage,
    ocrsLastUpdateTime,
  )
where

import qualified Network.AWS.Config.Types.OrganizationConfigRuleName as Types
import qualified Network.AWS.Config.Types.OrganizationRuleStatus as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns the status for an organization config rule in an organization.
--
-- /See:/ 'mkOrganizationConfigRuleStatus' smart constructor.
data OrganizationConfigRuleStatus = OrganizationConfigRuleStatus'
  { -- | The name that you assign to organization config rule.
    organizationConfigRuleName :: Types.OrganizationConfigRuleName,
    -- | Indicates deployment status of an organization config rule. When master account calls PutOrganizationConfigRule action for the first time, config rule status is created in all the member accounts. When master account calls PutOrganizationConfigRule action for the second time, config rule status is updated in all the member accounts. Additionally, config rule status is updated when one or more member accounts join or leave an organization. Config rule status is deleted when the master account deletes OrganizationConfigRule in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
    --
    -- AWS Config sets the state of the rule to:
    --
    --     * @CREATE_SUCCESSFUL@ when an organization config rule has been successfully created in all the member accounts.
    --
    --
    --     * @CREATE_IN_PROGRESS@ when an organization config rule creation is in progress.
    --
    --
    --     * @CREATE_FAILED@ when an organization config rule creation failed in one or more member accounts within that organization.
    --
    --
    --     * @DELETE_FAILED@ when an organization config rule deletion failed in one or more member accounts within that organization.
    --
    --
    --     * @DELETE_IN_PROGRESS@ when an organization config rule deletion is in progress.
    --
    --
    --     * @DELETE_SUCCESSFUL@ when an organization config rule has been successfully deleted from all the member accounts.
    --
    --
    --     * @UPDATE_SUCCESSFUL@ when an organization config rule has been successfully updated in all the member accounts.
    --
    --
    --     * @UPDATE_IN_PROGRESS@ when an organization config rule update is in progress.
    --
    --
    --     * @UPDATE_FAILED@ when an organization config rule update failed in one or more member accounts within that organization.
    organizationRuleStatus :: Types.OrganizationRuleStatus,
    -- | An error code that is returned when organization config rule creation or deletion has failed.
    errorCode :: Core.Maybe Types.String,
    -- | An error message indicating that organization config rule creation or deletion failed due to an error.
    errorMessage :: Core.Maybe Types.String,
    -- | The timestamp of the last update.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OrganizationConfigRuleStatus' value with any optional fields omitted.
mkOrganizationConfigRuleStatus ::
  -- | 'organizationConfigRuleName'
  Types.OrganizationConfigRuleName ->
  -- | 'organizationRuleStatus'
  Types.OrganizationRuleStatus ->
  OrganizationConfigRuleStatus
mkOrganizationConfigRuleStatus
  organizationConfigRuleName
  organizationRuleStatus =
    OrganizationConfigRuleStatus'
      { organizationConfigRuleName,
        organizationRuleStatus,
        errorCode = Core.Nothing,
        errorMessage = Core.Nothing,
        lastUpdateTime = Core.Nothing
      }

-- | The name that you assign to organization config rule.
--
-- /Note:/ Consider using 'organizationConfigRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsOrganizationConfigRuleName :: Lens.Lens' OrganizationConfigRuleStatus Types.OrganizationConfigRuleName
ocrsOrganizationConfigRuleName = Lens.field @"organizationConfigRuleName"
{-# DEPRECATED ocrsOrganizationConfigRuleName "Use generic-lens or generic-optics with 'organizationConfigRuleName' instead." #-}

-- | Indicates deployment status of an organization config rule. When master account calls PutOrganizationConfigRule action for the first time, config rule status is created in all the member accounts. When master account calls PutOrganizationConfigRule action for the second time, config rule status is updated in all the member accounts. Additionally, config rule status is updated when one or more member accounts join or leave an organization. Config rule status is deleted when the master account deletes OrganizationConfigRule in all the member accounts and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the rule to:
--
--     * @CREATE_SUCCESSFUL@ when an organization config rule has been successfully created in all the member accounts.
--
--
--     * @CREATE_IN_PROGRESS@ when an organization config rule creation is in progress.
--
--
--     * @CREATE_FAILED@ when an organization config rule creation failed in one or more member accounts within that organization.
--
--
--     * @DELETE_FAILED@ when an organization config rule deletion failed in one or more member accounts within that organization.
--
--
--     * @DELETE_IN_PROGRESS@ when an organization config rule deletion is in progress.
--
--
--     * @DELETE_SUCCESSFUL@ when an organization config rule has been successfully deleted from all the member accounts.
--
--
--     * @UPDATE_SUCCESSFUL@ when an organization config rule has been successfully updated in all the member accounts.
--
--
--     * @UPDATE_IN_PROGRESS@ when an organization config rule update is in progress.
--
--
--     * @UPDATE_FAILED@ when an organization config rule update failed in one or more member accounts within that organization.
--
--
--
-- /Note:/ Consider using 'organizationRuleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsOrganizationRuleStatus :: Lens.Lens' OrganizationConfigRuleStatus Types.OrganizationRuleStatus
ocrsOrganizationRuleStatus = Lens.field @"organizationRuleStatus"
{-# DEPRECATED ocrsOrganizationRuleStatus "Use generic-lens or generic-optics with 'organizationRuleStatus' instead." #-}

-- | An error code that is returned when organization config rule creation or deletion has failed.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsErrorCode :: Lens.Lens' OrganizationConfigRuleStatus (Core.Maybe Types.String)
ocrsErrorCode = Lens.field @"errorCode"
{-# DEPRECATED ocrsErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message indicating that organization config rule creation or deletion failed due to an error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsErrorMessage :: Lens.Lens' OrganizationConfigRuleStatus (Core.Maybe Types.String)
ocrsErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED ocrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The timestamp of the last update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocrsLastUpdateTime :: Lens.Lens' OrganizationConfigRuleStatus (Core.Maybe Core.NominalDiffTime)
ocrsLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED ocrsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Core.FromJSON OrganizationConfigRuleStatus where
  parseJSON =
    Core.withObject "OrganizationConfigRuleStatus" Core.$
      \x ->
        OrganizationConfigRuleStatus'
          Core.<$> (x Core..: "OrganizationConfigRuleName")
          Core.<*> (x Core..: "OrganizationRuleStatus")
          Core.<*> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "LastUpdateTime")
