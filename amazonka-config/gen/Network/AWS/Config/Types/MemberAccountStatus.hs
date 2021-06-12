{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MemberAccountStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MemberAccountStatus where

import Network.AWS.Config.Types.MemberAccountRuleStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Organization config rule creation or deletion status in each member
-- account. This includes the name of the rule, the status, error code and
-- error message when the rule creation or deletion failed.
--
-- /See:/ 'newMemberAccountStatus' smart constructor.
data MemberAccountStatus = MemberAccountStatus'
  { -- | The timestamp of the last status update.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | An error message indicating that config rule account creation or
    -- deletion has failed due to an error in the member account.
    errorMessage :: Core.Maybe Core.Text,
    -- | An error code that is returned when config rule creation or deletion
    -- failed in the member account.
    errorCode :: Core.Maybe Core.Text,
    -- | The 12-digit account ID of a member account.
    accountId :: Core.Text,
    -- | The name of config rule deployed in the member account.
    configRuleName :: Core.Text,
    -- | Indicates deployment status for config rule in the member account. When
    -- master account calls @PutOrganizationConfigRule@ action for the first
    -- time, config rule status is created in the member account. When master
    -- account calls @PutOrganizationConfigRule@ action for the second time,
    -- config rule status is updated in the member account. Config rule status
    -- is deleted when the master account deletes @OrganizationConfigRule@ and
    -- disables service access for @config-multiaccountsetup.amazonaws.com@.
    --
    -- AWS Config sets the state of the rule to:
    --
    -- -   @CREATE_SUCCESSFUL@ when config rule has been created in the member
    --     account.
    --
    -- -   @CREATE_IN_PROGRESS@ when config rule is being created in the member
    --     account.
    --
    -- -   @CREATE_FAILED@ when config rule creation has failed in the member
    --     account.
    --
    -- -   @DELETE_FAILED@ when config rule deletion has failed in the member
    --     account.
    --
    -- -   @DELETE_IN_PROGRESS@ when config rule is being deleted in the member
    --     account.
    --
    -- -   @DELETE_SUCCESSFUL@ when config rule has been deleted in the member
    --     account.
    --
    -- -   @UPDATE_SUCCESSFUL@ when config rule has been updated in the member
    --     account.
    --
    -- -   @UPDATE_IN_PROGRESS@ when config rule is being updated in the member
    --     account.
    --
    -- -   @UPDATE_FAILED@ when config rule deletion has failed in the member
    --     account.
    memberAccountRuleStatus :: MemberAccountRuleStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MemberAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTime', 'memberAccountStatus_lastUpdateTime' - The timestamp of the last status update.
--
-- 'errorMessage', 'memberAccountStatus_errorMessage' - An error message indicating that config rule account creation or
-- deletion has failed due to an error in the member account.
--
-- 'errorCode', 'memberAccountStatus_errorCode' - An error code that is returned when config rule creation or deletion
-- failed in the member account.
--
-- 'accountId', 'memberAccountStatus_accountId' - The 12-digit account ID of a member account.
--
-- 'configRuleName', 'memberAccountStatus_configRuleName' - The name of config rule deployed in the member account.
--
-- 'memberAccountRuleStatus', 'memberAccountStatus_memberAccountRuleStatus' - Indicates deployment status for config rule in the member account. When
-- master account calls @PutOrganizationConfigRule@ action for the first
-- time, config rule status is created in the member account. When master
-- account calls @PutOrganizationConfigRule@ action for the second time,
-- config rule status is updated in the member account. Config rule status
-- is deleted when the master account deletes @OrganizationConfigRule@ and
-- disables service access for @config-multiaccountsetup.amazonaws.com@.
--
-- AWS Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when config rule has been created in the member
--     account.
--
-- -   @CREATE_IN_PROGRESS@ when config rule is being created in the member
--     account.
--
-- -   @CREATE_FAILED@ when config rule creation has failed in the member
--     account.
--
-- -   @DELETE_FAILED@ when config rule deletion has failed in the member
--     account.
--
-- -   @DELETE_IN_PROGRESS@ when config rule is being deleted in the member
--     account.
--
-- -   @DELETE_SUCCESSFUL@ when config rule has been deleted in the member
--     account.
--
-- -   @UPDATE_SUCCESSFUL@ when config rule has been updated in the member
--     account.
--
-- -   @UPDATE_IN_PROGRESS@ when config rule is being updated in the member
--     account.
--
-- -   @UPDATE_FAILED@ when config rule deletion has failed in the member
--     account.
newMemberAccountStatus ::
  -- | 'accountId'
  Core.Text ->
  -- | 'configRuleName'
  Core.Text ->
  -- | 'memberAccountRuleStatus'
  MemberAccountRuleStatus ->
  MemberAccountStatus
newMemberAccountStatus
  pAccountId_
  pConfigRuleName_
  pMemberAccountRuleStatus_ =
    MemberAccountStatus'
      { lastUpdateTime = Core.Nothing,
        errorMessage = Core.Nothing,
        errorCode = Core.Nothing,
        accountId = pAccountId_,
        configRuleName = pConfigRuleName_,
        memberAccountRuleStatus = pMemberAccountRuleStatus_
      }

-- | The timestamp of the last status update.
memberAccountStatus_lastUpdateTime :: Lens.Lens' MemberAccountStatus (Core.Maybe Core.UTCTime)
memberAccountStatus_lastUpdateTime = Lens.lens (\MemberAccountStatus' {lastUpdateTime} -> lastUpdateTime) (\s@MemberAccountStatus' {} a -> s {lastUpdateTime = a} :: MemberAccountStatus) Core.. Lens.mapping Core._Time

-- | An error message indicating that config rule account creation or
-- deletion has failed due to an error in the member account.
memberAccountStatus_errorMessage :: Lens.Lens' MemberAccountStatus (Core.Maybe Core.Text)
memberAccountStatus_errorMessage = Lens.lens (\MemberAccountStatus' {errorMessage} -> errorMessage) (\s@MemberAccountStatus' {} a -> s {errorMessage = a} :: MemberAccountStatus)

-- | An error code that is returned when config rule creation or deletion
-- failed in the member account.
memberAccountStatus_errorCode :: Lens.Lens' MemberAccountStatus (Core.Maybe Core.Text)
memberAccountStatus_errorCode = Lens.lens (\MemberAccountStatus' {errorCode} -> errorCode) (\s@MemberAccountStatus' {} a -> s {errorCode = a} :: MemberAccountStatus)

-- | The 12-digit account ID of a member account.
memberAccountStatus_accountId :: Lens.Lens' MemberAccountStatus Core.Text
memberAccountStatus_accountId = Lens.lens (\MemberAccountStatus' {accountId} -> accountId) (\s@MemberAccountStatus' {} a -> s {accountId = a} :: MemberAccountStatus)

-- | The name of config rule deployed in the member account.
memberAccountStatus_configRuleName :: Lens.Lens' MemberAccountStatus Core.Text
memberAccountStatus_configRuleName = Lens.lens (\MemberAccountStatus' {configRuleName} -> configRuleName) (\s@MemberAccountStatus' {} a -> s {configRuleName = a} :: MemberAccountStatus)

-- | Indicates deployment status for config rule in the member account. When
-- master account calls @PutOrganizationConfigRule@ action for the first
-- time, config rule status is created in the member account. When master
-- account calls @PutOrganizationConfigRule@ action for the second time,
-- config rule status is updated in the member account. Config rule status
-- is deleted when the master account deletes @OrganizationConfigRule@ and
-- disables service access for @config-multiaccountsetup.amazonaws.com@.
--
-- AWS Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when config rule has been created in the member
--     account.
--
-- -   @CREATE_IN_PROGRESS@ when config rule is being created in the member
--     account.
--
-- -   @CREATE_FAILED@ when config rule creation has failed in the member
--     account.
--
-- -   @DELETE_FAILED@ when config rule deletion has failed in the member
--     account.
--
-- -   @DELETE_IN_PROGRESS@ when config rule is being deleted in the member
--     account.
--
-- -   @DELETE_SUCCESSFUL@ when config rule has been deleted in the member
--     account.
--
-- -   @UPDATE_SUCCESSFUL@ when config rule has been updated in the member
--     account.
--
-- -   @UPDATE_IN_PROGRESS@ when config rule is being updated in the member
--     account.
--
-- -   @UPDATE_FAILED@ when config rule deletion has failed in the member
--     account.
memberAccountStatus_memberAccountRuleStatus :: Lens.Lens' MemberAccountStatus MemberAccountRuleStatus
memberAccountStatus_memberAccountRuleStatus = Lens.lens (\MemberAccountStatus' {memberAccountRuleStatus} -> memberAccountRuleStatus) (\s@MemberAccountStatus' {} a -> s {memberAccountRuleStatus = a} :: MemberAccountStatus)

instance Core.FromJSON MemberAccountStatus where
  parseJSON =
    Core.withObject
      "MemberAccountStatus"
      ( \x ->
          MemberAccountStatus'
            Core.<$> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
            Core.<*> (x Core..: "AccountId")
            Core.<*> (x Core..: "ConfigRuleName")
            Core.<*> (x Core..: "MemberAccountRuleStatus")
      )

instance Core.Hashable MemberAccountStatus

instance Core.NFData MemberAccountStatus
