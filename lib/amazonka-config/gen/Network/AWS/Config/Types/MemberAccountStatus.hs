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
    masMemberAccountRuleStatus,
    masConfigRuleName,
    masAccountId,
    masErrorCode,
    masErrorMessage,
    masLastUpdateTime,
  )
where

import Network.AWS.Config.Types.MemberAccountRuleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Organization config rule creation or deletion status in each member account. This includes the name of the rule, the status, error code and error message when the rule creation or deletion failed.
--
-- /See:/ 'mkMemberAccountStatus' smart constructor.
data MemberAccountStatus = MemberAccountStatus'
  { -- | Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
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
    memberAccountRuleStatus :: MemberAccountRuleStatus,
    -- | The name of config rule deployed in the member account.
    configRuleName :: Lude.Text,
    -- | The 12-digit account ID of a member account.
    accountId :: Lude.Text,
    -- | An error code that is returned when config rule creation or deletion failed in the member account.
    errorCode :: Lude.Maybe Lude.Text,
    -- | An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | The timestamp of the last status update.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MemberAccountStatus' with the minimum fields required to make a request.
--
-- * 'memberAccountRuleStatus' - Indicates deployment status for config rule in the member account. When master account calls @PutOrganizationConfigRule@ action for the first time, config rule status is created in the member account. When master account calls @PutOrganizationConfigRule@ action for the second time, config rule status is updated in the member account. Config rule status is deleted when the master account deletes @OrganizationConfigRule@ and disables service access for @config-multiaccountsetup.amazonaws.com@ .
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
-- * 'configRuleName' - The name of config rule deployed in the member account.
-- * 'accountId' - The 12-digit account ID of a member account.
-- * 'errorCode' - An error code that is returned when config rule creation or deletion failed in the member account.
-- * 'errorMessage' - An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
-- * 'lastUpdateTime' - The timestamp of the last status update.
mkMemberAccountStatus ::
  -- | 'memberAccountRuleStatus'
  MemberAccountRuleStatus ->
  -- | 'configRuleName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  MemberAccountStatus
mkMemberAccountStatus
  pMemberAccountRuleStatus_
  pConfigRuleName_
  pAccountId_ =
    MemberAccountStatus'
      { memberAccountRuleStatus =
          pMemberAccountRuleStatus_,
        configRuleName = pConfigRuleName_,
        accountId = pAccountId_,
        errorCode = Lude.Nothing,
        errorMessage = Lude.Nothing,
        lastUpdateTime = Lude.Nothing
      }

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
masMemberAccountRuleStatus :: Lens.Lens' MemberAccountStatus MemberAccountRuleStatus
masMemberAccountRuleStatus = Lens.lens (memberAccountRuleStatus :: MemberAccountStatus -> MemberAccountRuleStatus) (\s a -> s {memberAccountRuleStatus = a} :: MemberAccountStatus)
{-# DEPRECATED masMemberAccountRuleStatus "Use generic-lens or generic-optics with 'memberAccountRuleStatus' instead." #-}

-- | The name of config rule deployed in the member account.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masConfigRuleName :: Lens.Lens' MemberAccountStatus Lude.Text
masConfigRuleName = Lens.lens (configRuleName :: MemberAccountStatus -> Lude.Text) (\s a -> s {configRuleName = a} :: MemberAccountStatus)
{-# DEPRECATED masConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

-- | The 12-digit account ID of a member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masAccountId :: Lens.Lens' MemberAccountStatus Lude.Text
masAccountId = Lens.lens (accountId :: MemberAccountStatus -> Lude.Text) (\s a -> s {accountId = a} :: MemberAccountStatus)
{-# DEPRECATED masAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | An error code that is returned when config rule creation or deletion failed in the member account.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masErrorCode :: Lens.Lens' MemberAccountStatus (Lude.Maybe Lude.Text)
masErrorCode = Lens.lens (errorCode :: MemberAccountStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: MemberAccountStatus)
{-# DEPRECATED masErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | An error message indicating that config rule account creation or deletion has failed due to an error in the member account.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masErrorMessage :: Lens.Lens' MemberAccountStatus (Lude.Maybe Lude.Text)
masErrorMessage = Lens.lens (errorMessage :: MemberAccountStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: MemberAccountStatus)
{-# DEPRECATED masErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The timestamp of the last status update.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
masLastUpdateTime :: Lens.Lens' MemberAccountStatus (Lude.Maybe Lude.Timestamp)
masLastUpdateTime = Lens.lens (lastUpdateTime :: MemberAccountStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: MemberAccountStatus)
{-# DEPRECATED masLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON MemberAccountStatus where
  parseJSON =
    Lude.withObject
      "MemberAccountStatus"
      ( \x ->
          MemberAccountStatus'
            Lude.<$> (x Lude..: "MemberAccountRuleStatus")
            Lude.<*> (x Lude..: "ConfigRuleName")
            Lude.<*> (x Lude..: "AccountId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "LastUpdateTime")
      )
