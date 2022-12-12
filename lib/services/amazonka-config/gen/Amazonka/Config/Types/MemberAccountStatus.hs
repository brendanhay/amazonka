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
-- Module      : Amazonka.Config.Types.MemberAccountStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.MemberAccountStatus where

import Amazonka.Config.Types.MemberAccountRuleStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Organization Config rule creation or deletion status in each member
-- account. This includes the name of the rule, the status, error code and
-- error message when the rule creation or deletion failed.
--
-- /See:/ 'newMemberAccountStatus' smart constructor.
data MemberAccountStatus = MemberAccountStatus'
  { -- | An error code that is returned when Config rule creation or deletion
    -- failed in the member account.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message indicating that Config rule account creation or
    -- deletion has failed due to an error in the member account.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last status update.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The 12-digit account ID of a member account.
    accountId :: Prelude.Text,
    -- | The name of Config rule deployed in the member account.
    configRuleName :: Prelude.Text,
    -- | Indicates deployment status for Config rule in the member account. When
    -- management account calls @PutOrganizationConfigRule@ action for the
    -- first time, Config rule status is created in the member account. When
    -- management account calls @PutOrganizationConfigRule@ action for the
    -- second time, Config rule status is updated in the member account. Config
    -- rule status is deleted when the management account deletes
    -- @OrganizationConfigRule@ and disables service access for
    -- @config-multiaccountsetup.amazonaws.com@.
    --
    -- Config sets the state of the rule to:
    --
    -- -   @CREATE_SUCCESSFUL@ when Config rule has been created in the member
    --     account.
    --
    -- -   @CREATE_IN_PROGRESS@ when Config rule is being created in the member
    --     account.
    --
    -- -   @CREATE_FAILED@ when Config rule creation has failed in the member
    --     account.
    --
    -- -   @DELETE_FAILED@ when Config rule deletion has failed in the member
    --     account.
    --
    -- -   @DELETE_IN_PROGRESS@ when Config rule is being deleted in the member
    --     account.
    --
    -- -   @DELETE_SUCCESSFUL@ when Config rule has been deleted in the member
    --     account.
    --
    -- -   @UPDATE_SUCCESSFUL@ when Config rule has been updated in the member
    --     account.
    --
    -- -   @UPDATE_IN_PROGRESS@ when Config rule is being updated in the member
    --     account.
    --
    -- -   @UPDATE_FAILED@ when Config rule deletion has failed in the member
    --     account.
    memberAccountRuleStatus :: MemberAccountRuleStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'memberAccountStatus_errorCode' - An error code that is returned when Config rule creation or deletion
-- failed in the member account.
--
-- 'errorMessage', 'memberAccountStatus_errorMessage' - An error message indicating that Config rule account creation or
-- deletion has failed due to an error in the member account.
--
-- 'lastUpdateTime', 'memberAccountStatus_lastUpdateTime' - The timestamp of the last status update.
--
-- 'accountId', 'memberAccountStatus_accountId' - The 12-digit account ID of a member account.
--
-- 'configRuleName', 'memberAccountStatus_configRuleName' - The name of Config rule deployed in the member account.
--
-- 'memberAccountRuleStatus', 'memberAccountStatus_memberAccountRuleStatus' - Indicates deployment status for Config rule in the member account. When
-- management account calls @PutOrganizationConfigRule@ action for the
-- first time, Config rule status is created in the member account. When
-- management account calls @PutOrganizationConfigRule@ action for the
-- second time, Config rule status is updated in the member account. Config
-- rule status is deleted when the management account deletes
-- @OrganizationConfigRule@ and disables service access for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when Config rule has been created in the member
--     account.
--
-- -   @CREATE_IN_PROGRESS@ when Config rule is being created in the member
--     account.
--
-- -   @CREATE_FAILED@ when Config rule creation has failed in the member
--     account.
--
-- -   @DELETE_FAILED@ when Config rule deletion has failed in the member
--     account.
--
-- -   @DELETE_IN_PROGRESS@ when Config rule is being deleted in the member
--     account.
--
-- -   @DELETE_SUCCESSFUL@ when Config rule has been deleted in the member
--     account.
--
-- -   @UPDATE_SUCCESSFUL@ when Config rule has been updated in the member
--     account.
--
-- -   @UPDATE_IN_PROGRESS@ when Config rule is being updated in the member
--     account.
--
-- -   @UPDATE_FAILED@ when Config rule deletion has failed in the member
--     account.
newMemberAccountStatus ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'memberAccountRuleStatus'
  MemberAccountRuleStatus ->
  MemberAccountStatus
newMemberAccountStatus
  pAccountId_
  pConfigRuleName_
  pMemberAccountRuleStatus_ =
    MemberAccountStatus'
      { errorCode = Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        lastUpdateTime = Prelude.Nothing,
        accountId = pAccountId_,
        configRuleName = pConfigRuleName_,
        memberAccountRuleStatus = pMemberAccountRuleStatus_
      }

-- | An error code that is returned when Config rule creation or deletion
-- failed in the member account.
memberAccountStatus_errorCode :: Lens.Lens' MemberAccountStatus (Prelude.Maybe Prelude.Text)
memberAccountStatus_errorCode = Lens.lens (\MemberAccountStatus' {errorCode} -> errorCode) (\s@MemberAccountStatus' {} a -> s {errorCode = a} :: MemberAccountStatus)

-- | An error message indicating that Config rule account creation or
-- deletion has failed due to an error in the member account.
memberAccountStatus_errorMessage :: Lens.Lens' MemberAccountStatus (Prelude.Maybe Prelude.Text)
memberAccountStatus_errorMessage = Lens.lens (\MemberAccountStatus' {errorMessage} -> errorMessage) (\s@MemberAccountStatus' {} a -> s {errorMessage = a} :: MemberAccountStatus)

-- | The timestamp of the last status update.
memberAccountStatus_lastUpdateTime :: Lens.Lens' MemberAccountStatus (Prelude.Maybe Prelude.UTCTime)
memberAccountStatus_lastUpdateTime = Lens.lens (\MemberAccountStatus' {lastUpdateTime} -> lastUpdateTime) (\s@MemberAccountStatus' {} a -> s {lastUpdateTime = a} :: MemberAccountStatus) Prelude.. Lens.mapping Data._Time

-- | The 12-digit account ID of a member account.
memberAccountStatus_accountId :: Lens.Lens' MemberAccountStatus Prelude.Text
memberAccountStatus_accountId = Lens.lens (\MemberAccountStatus' {accountId} -> accountId) (\s@MemberAccountStatus' {} a -> s {accountId = a} :: MemberAccountStatus)

-- | The name of Config rule deployed in the member account.
memberAccountStatus_configRuleName :: Lens.Lens' MemberAccountStatus Prelude.Text
memberAccountStatus_configRuleName = Lens.lens (\MemberAccountStatus' {configRuleName} -> configRuleName) (\s@MemberAccountStatus' {} a -> s {configRuleName = a} :: MemberAccountStatus)

-- | Indicates deployment status for Config rule in the member account. When
-- management account calls @PutOrganizationConfigRule@ action for the
-- first time, Config rule status is created in the member account. When
-- management account calls @PutOrganizationConfigRule@ action for the
-- second time, Config rule status is updated in the member account. Config
-- rule status is deleted when the management account deletes
-- @OrganizationConfigRule@ and disables service access for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when Config rule has been created in the member
--     account.
--
-- -   @CREATE_IN_PROGRESS@ when Config rule is being created in the member
--     account.
--
-- -   @CREATE_FAILED@ when Config rule creation has failed in the member
--     account.
--
-- -   @DELETE_FAILED@ when Config rule deletion has failed in the member
--     account.
--
-- -   @DELETE_IN_PROGRESS@ when Config rule is being deleted in the member
--     account.
--
-- -   @DELETE_SUCCESSFUL@ when Config rule has been deleted in the member
--     account.
--
-- -   @UPDATE_SUCCESSFUL@ when Config rule has been updated in the member
--     account.
--
-- -   @UPDATE_IN_PROGRESS@ when Config rule is being updated in the member
--     account.
--
-- -   @UPDATE_FAILED@ when Config rule deletion has failed in the member
--     account.
memberAccountStatus_memberAccountRuleStatus :: Lens.Lens' MemberAccountStatus MemberAccountRuleStatus
memberAccountStatus_memberAccountRuleStatus = Lens.lens (\MemberAccountStatus' {memberAccountRuleStatus} -> memberAccountRuleStatus) (\s@MemberAccountStatus' {} a -> s {memberAccountRuleStatus = a} :: MemberAccountStatus)

instance Data.FromJSON MemberAccountStatus where
  parseJSON =
    Data.withObject
      "MemberAccountStatus"
      ( \x ->
          MemberAccountStatus'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..: "AccountId")
            Prelude.<*> (x Data..: "ConfigRuleName")
            Prelude.<*> (x Data..: "MemberAccountRuleStatus")
      )

instance Prelude.Hashable MemberAccountStatus where
  hashWithSalt _salt MemberAccountStatus' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` memberAccountRuleStatus

instance Prelude.NFData MemberAccountStatus where
  rnf MemberAccountStatus' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf memberAccountRuleStatus
