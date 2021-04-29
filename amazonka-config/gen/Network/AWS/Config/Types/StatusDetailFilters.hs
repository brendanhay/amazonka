{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.StatusDetailFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StatusDetailFilters where

import Network.AWS.Config.Types.MemberAccountRuleStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status filter object to filter results based on specific member account
-- ID or status type for an organization config rule.
--
-- /See:/ 'newStatusDetailFilters' smart constructor.
data StatusDetailFilters = StatusDetailFilters'
  { -- | The 12-digit account ID of the member account within an organization.
    accountId :: Prelude.Maybe Prelude.Text,
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
    memberAccountRuleStatus :: Prelude.Maybe MemberAccountRuleStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StatusDetailFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'statusDetailFilters_accountId' - The 12-digit account ID of the member account within an organization.
--
-- 'memberAccountRuleStatus', 'statusDetailFilters_memberAccountRuleStatus' - Indicates deployment status for config rule in the member account. When
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
newStatusDetailFilters ::
  StatusDetailFilters
newStatusDetailFilters =
  StatusDetailFilters'
    { accountId = Prelude.Nothing,
      memberAccountRuleStatus = Prelude.Nothing
    }

-- | The 12-digit account ID of the member account within an organization.
statusDetailFilters_accountId :: Lens.Lens' StatusDetailFilters (Prelude.Maybe Prelude.Text)
statusDetailFilters_accountId = Lens.lens (\StatusDetailFilters' {accountId} -> accountId) (\s@StatusDetailFilters' {} a -> s {accountId = a} :: StatusDetailFilters)

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
statusDetailFilters_memberAccountRuleStatus :: Lens.Lens' StatusDetailFilters (Prelude.Maybe MemberAccountRuleStatus)
statusDetailFilters_memberAccountRuleStatus = Lens.lens (\StatusDetailFilters' {memberAccountRuleStatus} -> memberAccountRuleStatus) (\s@StatusDetailFilters' {} a -> s {memberAccountRuleStatus = a} :: StatusDetailFilters)

instance Prelude.Hashable StatusDetailFilters

instance Prelude.NFData StatusDetailFilters

instance Prelude.ToJSON StatusDetailFilters where
  toJSON StatusDetailFilters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AccountId" Prelude..=) Prelude.<$> accountId,
            ("MemberAccountRuleStatus" Prelude..=)
              Prelude.<$> memberAccountRuleStatus
          ]
      )
