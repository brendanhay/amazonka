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
-- Module      : Amazonka.Config.Types.OrganizationConfigRuleStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationConfigRuleStatus where

import Amazonka.Config.Types.OrganizationRuleStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the status for an organization Config rule in an organization.
--
-- /See:/ 'newOrganizationConfigRuleStatus' smart constructor.
data OrganizationConfigRuleStatus = OrganizationConfigRuleStatus'
  { -- | An error code that is returned when organization Config rule creation or
    -- deletion has failed.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | An error message indicating that organization Config rule creation or
    -- deletion failed due to an error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last update.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name that you assign to organization Config rule.
    organizationConfigRuleName :: Prelude.Text,
    -- | Indicates deployment status of an organization Config rule. When
    -- management account calls PutOrganizationConfigRule action for the first
    -- time, Config rule status is created in all the member accounts. When
    -- management account calls PutOrganizationConfigRule action for the second
    -- time, Config rule status is updated in all the member accounts.
    -- Additionally, Config rule status is updated when one or more member
    -- accounts join or leave an organization. Config rule status is deleted
    -- when the management account deletes OrganizationConfigRule in all the
    -- member accounts and disables service access for
    -- @config-multiaccountsetup.amazonaws.com@.
    --
    -- Config sets the state of the rule to:
    --
    -- -   @CREATE_SUCCESSFUL@ when an organization Config rule has been
    --     successfully created in all the member accounts.
    --
    -- -   @CREATE_IN_PROGRESS@ when an organization Config rule creation is in
    --     progress.
    --
    -- -   @CREATE_FAILED@ when an organization Config rule creation failed in
    --     one or more member accounts within that organization.
    --
    -- -   @DELETE_FAILED@ when an organization Config rule deletion failed in
    --     one or more member accounts within that organization.
    --
    -- -   @DELETE_IN_PROGRESS@ when an organization Config rule deletion is in
    --     progress.
    --
    -- -   @DELETE_SUCCESSFUL@ when an organization Config rule has been
    --     successfully deleted from all the member accounts.
    --
    -- -   @UPDATE_SUCCESSFUL@ when an organization Config rule has been
    --     successfully updated in all the member accounts.
    --
    -- -   @UPDATE_IN_PROGRESS@ when an organization Config rule update is in
    --     progress.
    --
    -- -   @UPDATE_FAILED@ when an organization Config rule update failed in
    --     one or more member accounts within that organization.
    organizationRuleStatus :: OrganizationRuleStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationConfigRuleStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'organizationConfigRuleStatus_errorCode' - An error code that is returned when organization Config rule creation or
-- deletion has failed.
--
-- 'errorMessage', 'organizationConfigRuleStatus_errorMessage' - An error message indicating that organization Config rule creation or
-- deletion failed due to an error.
--
-- 'lastUpdateTime', 'organizationConfigRuleStatus_lastUpdateTime' - The timestamp of the last update.
--
-- 'organizationConfigRuleName', 'organizationConfigRuleStatus_organizationConfigRuleName' - The name that you assign to organization Config rule.
--
-- 'organizationRuleStatus', 'organizationConfigRuleStatus_organizationRuleStatus' - Indicates deployment status of an organization Config rule. When
-- management account calls PutOrganizationConfigRule action for the first
-- time, Config rule status is created in all the member accounts. When
-- management account calls PutOrganizationConfigRule action for the second
-- time, Config rule status is updated in all the member accounts.
-- Additionally, Config rule status is updated when one or more member
-- accounts join or leave an organization. Config rule status is deleted
-- when the management account deletes OrganizationConfigRule in all the
-- member accounts and disables service access for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when an organization Config rule has been
--     successfully created in all the member accounts.
--
-- -   @CREATE_IN_PROGRESS@ when an organization Config rule creation is in
--     progress.
--
-- -   @CREATE_FAILED@ when an organization Config rule creation failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_FAILED@ when an organization Config rule deletion failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_IN_PROGRESS@ when an organization Config rule deletion is in
--     progress.
--
-- -   @DELETE_SUCCESSFUL@ when an organization Config rule has been
--     successfully deleted from all the member accounts.
--
-- -   @UPDATE_SUCCESSFUL@ when an organization Config rule has been
--     successfully updated in all the member accounts.
--
-- -   @UPDATE_IN_PROGRESS@ when an organization Config rule update is in
--     progress.
--
-- -   @UPDATE_FAILED@ when an organization Config rule update failed in
--     one or more member accounts within that organization.
newOrganizationConfigRuleStatus ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  -- | 'organizationRuleStatus'
  OrganizationRuleStatus ->
  OrganizationConfigRuleStatus
newOrganizationConfigRuleStatus
  pOrganizationConfigRuleName_
  pOrganizationRuleStatus_ =
    OrganizationConfigRuleStatus'
      { errorCode =
          Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        lastUpdateTime = Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_,
        organizationRuleStatus =
          pOrganizationRuleStatus_
      }

-- | An error code that is returned when organization Config rule creation or
-- deletion has failed.
organizationConfigRuleStatus_errorCode :: Lens.Lens' OrganizationConfigRuleStatus (Prelude.Maybe Prelude.Text)
organizationConfigRuleStatus_errorCode = Lens.lens (\OrganizationConfigRuleStatus' {errorCode} -> errorCode) (\s@OrganizationConfigRuleStatus' {} a -> s {errorCode = a} :: OrganizationConfigRuleStatus)

-- | An error message indicating that organization Config rule creation or
-- deletion failed due to an error.
organizationConfigRuleStatus_errorMessage :: Lens.Lens' OrganizationConfigRuleStatus (Prelude.Maybe Prelude.Text)
organizationConfigRuleStatus_errorMessage = Lens.lens (\OrganizationConfigRuleStatus' {errorMessage} -> errorMessage) (\s@OrganizationConfigRuleStatus' {} a -> s {errorMessage = a} :: OrganizationConfigRuleStatus)

-- | The timestamp of the last update.
organizationConfigRuleStatus_lastUpdateTime :: Lens.Lens' OrganizationConfigRuleStatus (Prelude.Maybe Prelude.UTCTime)
organizationConfigRuleStatus_lastUpdateTime = Lens.lens (\OrganizationConfigRuleStatus' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConfigRuleStatus' {} a -> s {lastUpdateTime = a} :: OrganizationConfigRuleStatus) Prelude.. Lens.mapping Data._Time

-- | The name that you assign to organization Config rule.
organizationConfigRuleStatus_organizationConfigRuleName :: Lens.Lens' OrganizationConfigRuleStatus Prelude.Text
organizationConfigRuleStatus_organizationConfigRuleName = Lens.lens (\OrganizationConfigRuleStatus' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@OrganizationConfigRuleStatus' {} a -> s {organizationConfigRuleName = a} :: OrganizationConfigRuleStatus)

-- | Indicates deployment status of an organization Config rule. When
-- management account calls PutOrganizationConfigRule action for the first
-- time, Config rule status is created in all the member accounts. When
-- management account calls PutOrganizationConfigRule action for the second
-- time, Config rule status is updated in all the member accounts.
-- Additionally, Config rule status is updated when one or more member
-- accounts join or leave an organization. Config rule status is deleted
-- when the management account deletes OrganizationConfigRule in all the
-- member accounts and disables service access for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when an organization Config rule has been
--     successfully created in all the member accounts.
--
-- -   @CREATE_IN_PROGRESS@ when an organization Config rule creation is in
--     progress.
--
-- -   @CREATE_FAILED@ when an organization Config rule creation failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_FAILED@ when an organization Config rule deletion failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_IN_PROGRESS@ when an organization Config rule deletion is in
--     progress.
--
-- -   @DELETE_SUCCESSFUL@ when an organization Config rule has been
--     successfully deleted from all the member accounts.
--
-- -   @UPDATE_SUCCESSFUL@ when an organization Config rule has been
--     successfully updated in all the member accounts.
--
-- -   @UPDATE_IN_PROGRESS@ when an organization Config rule update is in
--     progress.
--
-- -   @UPDATE_FAILED@ when an organization Config rule update failed in
--     one or more member accounts within that organization.
organizationConfigRuleStatus_organizationRuleStatus :: Lens.Lens' OrganizationConfigRuleStatus OrganizationRuleStatus
organizationConfigRuleStatus_organizationRuleStatus = Lens.lens (\OrganizationConfigRuleStatus' {organizationRuleStatus} -> organizationRuleStatus) (\s@OrganizationConfigRuleStatus' {} a -> s {organizationRuleStatus = a} :: OrganizationConfigRuleStatus)

instance Data.FromJSON OrganizationConfigRuleStatus where
  parseJSON =
    Data.withObject
      "OrganizationConfigRuleStatus"
      ( \x ->
          OrganizationConfigRuleStatus'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..: "OrganizationConfigRuleName")
            Prelude.<*> (x Data..: "OrganizationRuleStatus")
      )

instance
  Prelude.Hashable
    OrganizationConfigRuleStatus
  where
  hashWithSalt _salt OrganizationConfigRuleStatus' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` organizationConfigRuleName
      `Prelude.hashWithSalt` organizationRuleStatus

instance Prelude.NFData OrganizationConfigRuleStatus where
  rnf OrganizationConfigRuleStatus' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf lastUpdateTime `Prelude.seq`
          Prelude.rnf organizationConfigRuleName `Prelude.seq`
            Prelude.rnf organizationRuleStatus
