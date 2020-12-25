{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MemberAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MemberAccountStatus
  ( MemberAccountStatus (..),

    -- * Smart constructor
    mkMemberAccountStatus,

    -- * Lenses
    masAccountId,
    masConfigRuleName,
    masMemberAccountRuleStatus,
    masErrorCode,
    masErrorMessage,
    masLastUpdateTime,
  )
where

import qualified Network.AWS.Config.Types.AccountId as Types
import qualified Network.AWS.Config.Types.MemberAccountRuleStatus as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Config.Types.StringWithCharLimit64 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Organization config rule creation or deletion status in each member account. This includes the name of the rule, the status, error code and error message when the rule creation or deletion failed.
--
-- /See:/ 'mkMemberAccountStatus' smart constructor.
data MemberAccountStatus = MemberAccountStatus'
  { -- | The 12-digit account ID of a member account.
    accountId :: Types.AccountId,
    -- | The name of config rule deployed in the member account.
    configRuleName :: Types.StringWithCharLimit64,
    -- | Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
    --
    -- AWS Config sets the state of the rule to:
    --
    --     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.
    --
    --
    --     * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.
    --
    --
    --     * @CREATE_FAILED@ when config rule creation has failed in the member account.
    --
    --
    --     * @DELETE_FAILED@ when config rule deletion has failed in the member account.
    --
    --
    --     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.
    --
    --
    --     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.
    --
    --
    --     * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.
    --
    --
    --     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.
    --
    --
    --     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
    memberAccountRuleStatus :: Types.MemberAccountRuleStatus,
    -- | An error code that is returned when config rule creation or deletion failed in the member account.
    errorCode :: Core.Maybe Types.String,
    -- | An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
    errorMessage :: Core.Maybe Types.String,
    -- | The timestamp of the last status update.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'MemberAccountStatus' value with any optional fields omitted.
mkMemberAccountStatus ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'configRuleName'
  Types.StringWithCharLimit64 ->
  -- | 'memberAccountRuleStatus'
  Types.MemberAccountRuleStatus ->
  MemberAccountStatus
mkMemberAccountStatus
  accountId
  configRuleName
  memberAccountRuleStatus =
    MemberAccountStatus'
      { accountId,
        configRuleName,
        memberAccountRuleStatus,
        errorCode = Core.Nothing,
        errorMessage = Core.Nothing,
        lastUpdateTime = Core.Nothing
      }

-- | The 12-digit account ID of a member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masAccountId :: Lens.Lens' MemberAccountStatus Types.AccountId
masAccountId = Lens.field @"accountId"
{-# DEPRECATED masAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of config rule deployed in the member account.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masConfigRuleName :: Lens.Lens' MemberAccountStatus Types.StringWithCharLimit64
masConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED masConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
--
-- AWS Config sets the state of the rule to:
--
--     * @CREATE_SUCCESSFUL@ when config rule has been created in the member account.
--
--
--     * @CREATE_IN_PROGRESS@ when config rule is being created in the member account.
--
--
--     * @CREATE_FAILED@ when config rule creation has failed in the member account.
--
--
--     * @DELETE_FAILED@ when config rule deletion has failed in the member account.
--
--
--     * @DELETE_IN_PROGRESS@ when config rule is being deleted in the member account.
--
--
--     * @DELETE_SUCCESSFUL@ when config rule has been deleted in the member account.
--
--
--     * @UPDATE_SUCCESSFUL@ when config rule has been updated in the member account.
--
--
--     * @UPDATE_IN_PROGRESS@ when config rule is being updated in the member account.
--
--
--     * @UPDATE_FAILED@ when config rule deletion has failed in the member account.
--
--
--
-- /Note:/ Consider using 'memberAccountRuleStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masMemberAccountRuleStatus :: Lens.Lens' MemberAccountStatus Types.MemberAccountRuleStatus
masMemberAccountRuleStatus = Lens.field @"memberAccountRuleStatus"
{-# DEPRECATED masMemberAccountRuleStatus "Use generic-lens or generic-optics with 'memberAccountRuleStatus' instead." #-}

-- | An error code that is returned when config rule creation or deletion failed in the member account.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masErrorCode :: Lens.Lens' MemberAccountStatus (Core.Maybe Types.String)
masErrorCode = Lens.field @"errorCode"
{-# DEPRECATED masErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masErrorMessage :: Lens.Lens' MemberAccountStatus (Core.Maybe Types.String)
masErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED masErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The timestamp of the last status update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masLastUpdateTime :: Lens.Lens' MemberAccountStatus (Core.Maybe Core.NominalDiffTime)
masLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED masLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Core.FromJSON MemberAccountStatus where
  parseJSON =
    Core.withObject "MemberAccountStatus" Core.$
      \x ->
        MemberAccountStatus'
          Core.<$> (x Core..: "AccountId")
          Core.<*> (x Core..: "ConfigRuleName")
          Core.<*> (x Core..: "MemberAccountRuleStatus")
          Core.<*> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "LastUpdateTime")
