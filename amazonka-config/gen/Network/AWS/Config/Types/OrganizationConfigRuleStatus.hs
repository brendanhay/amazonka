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
-- Module      : Network.AWS.Config.Types.OrganizationConfigRuleStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRuleStatus where

import Network.AWS.Config.Types.OrganizationRuleStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns the status for an organization config rule in an organization.
--
-- /See:/ 'newOrganizationConfigRuleStatus' smart constructor.
data OrganizationConfigRuleStatus = OrganizationConfigRuleStatus'
  { -- | The timestamp of the last update.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | An error message indicating that organization config rule creation or
    -- deletion failed due to an error.
    errorMessage :: Core.Maybe Core.Text,
    -- | An error code that is returned when organization config rule creation or
    -- deletion has failed.
    errorCode :: Core.Maybe Core.Text,
    -- | The name that you assign to organization config rule.
    organizationConfigRuleName :: Core.Text,
    -- | Indicates deployment status of an organization config rule. When master
    -- account calls PutOrganizationConfigRule action for the first time,
    -- config rule status is created in all the member accounts. When master
    -- account calls PutOrganizationConfigRule action for the second time,
    -- config rule status is updated in all the member accounts. Additionally,
    -- config rule status is updated when one or more member accounts join or
    -- leave an organization. Config rule status is deleted when the master
    -- account deletes OrganizationConfigRule in all the member accounts and
    -- disables service access for @config-multiaccountsetup.amazonaws.com@.
    --
    -- AWS Config sets the state of the rule to:
    --
    -- -   @CREATE_SUCCESSFUL@ when an organization config rule has been
    --     successfully created in all the member accounts.
    --
    -- -   @CREATE_IN_PROGRESS@ when an organization config rule creation is in
    --     progress.
    --
    -- -   @CREATE_FAILED@ when an organization config rule creation failed in
    --     one or more member accounts within that organization.
    --
    -- -   @DELETE_FAILED@ when an organization config rule deletion failed in
    --     one or more member accounts within that organization.
    --
    -- -   @DELETE_IN_PROGRESS@ when an organization config rule deletion is in
    --     progress.
    --
    -- -   @DELETE_SUCCESSFUL@ when an organization config rule has been
    --     successfully deleted from all the member accounts.
    --
    -- -   @UPDATE_SUCCESSFUL@ when an organization config rule has been
    --     successfully updated in all the member accounts.
    --
    -- -   @UPDATE_IN_PROGRESS@ when an organization config rule update is in
    --     progress.
    --
    -- -   @UPDATE_FAILED@ when an organization config rule update failed in
    --     one or more member accounts within that organization.
    organizationRuleStatus :: OrganizationRuleStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrganizationConfigRuleStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTime', 'organizationConfigRuleStatus_lastUpdateTime' - The timestamp of the last update.
--
-- 'errorMessage', 'organizationConfigRuleStatus_errorMessage' - An error message indicating that organization config rule creation or
-- deletion failed due to an error.
--
-- 'errorCode', 'organizationConfigRuleStatus_errorCode' - An error code that is returned when organization config rule creation or
-- deletion has failed.
--
-- 'organizationConfigRuleName', 'organizationConfigRuleStatus_organizationConfigRuleName' - The name that you assign to organization config rule.
--
-- 'organizationRuleStatus', 'organizationConfigRuleStatus_organizationRuleStatus' - Indicates deployment status of an organization config rule. When master
-- account calls PutOrganizationConfigRule action for the first time,
-- config rule status is created in all the member accounts. When master
-- account calls PutOrganizationConfigRule action for the second time,
-- config rule status is updated in all the member accounts. Additionally,
-- config rule status is updated when one or more member accounts join or
-- leave an organization. Config rule status is deleted when the master
-- account deletes OrganizationConfigRule in all the member accounts and
-- disables service access for @config-multiaccountsetup.amazonaws.com@.
--
-- AWS Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when an organization config rule has been
--     successfully created in all the member accounts.
--
-- -   @CREATE_IN_PROGRESS@ when an organization config rule creation is in
--     progress.
--
-- -   @CREATE_FAILED@ when an organization config rule creation failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_FAILED@ when an organization config rule deletion failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_IN_PROGRESS@ when an organization config rule deletion is in
--     progress.
--
-- -   @DELETE_SUCCESSFUL@ when an organization config rule has been
--     successfully deleted from all the member accounts.
--
-- -   @UPDATE_SUCCESSFUL@ when an organization config rule has been
--     successfully updated in all the member accounts.
--
-- -   @UPDATE_IN_PROGRESS@ when an organization config rule update is in
--     progress.
--
-- -   @UPDATE_FAILED@ when an organization config rule update failed in
--     one or more member accounts within that organization.
newOrganizationConfigRuleStatus ::
  -- | 'organizationConfigRuleName'
  Core.Text ->
  -- | 'organizationRuleStatus'
  OrganizationRuleStatus ->
  OrganizationConfigRuleStatus
newOrganizationConfigRuleStatus
  pOrganizationConfigRuleName_
  pOrganizationRuleStatus_ =
    OrganizationConfigRuleStatus'
      { lastUpdateTime =
          Core.Nothing,
        errorMessage = Core.Nothing,
        errorCode = Core.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_,
        organizationRuleStatus =
          pOrganizationRuleStatus_
      }

-- | The timestamp of the last update.
organizationConfigRuleStatus_lastUpdateTime :: Lens.Lens' OrganizationConfigRuleStatus (Core.Maybe Core.UTCTime)
organizationConfigRuleStatus_lastUpdateTime = Lens.lens (\OrganizationConfigRuleStatus' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConfigRuleStatus' {} a -> s {lastUpdateTime = a} :: OrganizationConfigRuleStatus) Core.. Lens.mapping Core._Time

-- | An error message indicating that organization config rule creation or
-- deletion failed due to an error.
organizationConfigRuleStatus_errorMessage :: Lens.Lens' OrganizationConfigRuleStatus (Core.Maybe Core.Text)
organizationConfigRuleStatus_errorMessage = Lens.lens (\OrganizationConfigRuleStatus' {errorMessage} -> errorMessage) (\s@OrganizationConfigRuleStatus' {} a -> s {errorMessage = a} :: OrganizationConfigRuleStatus)

-- | An error code that is returned when organization config rule creation or
-- deletion has failed.
organizationConfigRuleStatus_errorCode :: Lens.Lens' OrganizationConfigRuleStatus (Core.Maybe Core.Text)
organizationConfigRuleStatus_errorCode = Lens.lens (\OrganizationConfigRuleStatus' {errorCode} -> errorCode) (\s@OrganizationConfigRuleStatus' {} a -> s {errorCode = a} :: OrganizationConfigRuleStatus)

-- | The name that you assign to organization config rule.
organizationConfigRuleStatus_organizationConfigRuleName :: Lens.Lens' OrganizationConfigRuleStatus Core.Text
organizationConfigRuleStatus_organizationConfigRuleName = Lens.lens (\OrganizationConfigRuleStatus' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@OrganizationConfigRuleStatus' {} a -> s {organizationConfigRuleName = a} :: OrganizationConfigRuleStatus)

-- | Indicates deployment status of an organization config rule. When master
-- account calls PutOrganizationConfigRule action for the first time,
-- config rule status is created in all the member accounts. When master
-- account calls PutOrganizationConfigRule action for the second time,
-- config rule status is updated in all the member accounts. Additionally,
-- config rule status is updated when one or more member accounts join or
-- leave an organization. Config rule status is deleted when the master
-- account deletes OrganizationConfigRule in all the member accounts and
-- disables service access for @config-multiaccountsetup.amazonaws.com@.
--
-- AWS Config sets the state of the rule to:
--
-- -   @CREATE_SUCCESSFUL@ when an organization config rule has been
--     successfully created in all the member accounts.
--
-- -   @CREATE_IN_PROGRESS@ when an organization config rule creation is in
--     progress.
--
-- -   @CREATE_FAILED@ when an organization config rule creation failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_FAILED@ when an organization config rule deletion failed in
--     one or more member accounts within that organization.
--
-- -   @DELETE_IN_PROGRESS@ when an organization config rule deletion is in
--     progress.
--
-- -   @DELETE_SUCCESSFUL@ when an organization config rule has been
--     successfully deleted from all the member accounts.
--
-- -   @UPDATE_SUCCESSFUL@ when an organization config rule has been
--     successfully updated in all the member accounts.
--
-- -   @UPDATE_IN_PROGRESS@ when an organization config rule update is in
--     progress.
--
-- -   @UPDATE_FAILED@ when an organization config rule update failed in
--     one or more member accounts within that organization.
organizationConfigRuleStatus_organizationRuleStatus :: Lens.Lens' OrganizationConfigRuleStatus OrganizationRuleStatus
organizationConfigRuleStatus_organizationRuleStatus = Lens.lens (\OrganizationConfigRuleStatus' {organizationRuleStatus} -> organizationRuleStatus) (\s@OrganizationConfigRuleStatus' {} a -> s {organizationRuleStatus = a} :: OrganizationConfigRuleStatus)

instance Core.FromJSON OrganizationConfigRuleStatus where
  parseJSON =
    Core.withObject
      "OrganizationConfigRuleStatus"
      ( \x ->
          OrganizationConfigRuleStatus'
            Core.<$> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
            Core.<*> (x Core..: "OrganizationConfigRuleName")
            Core.<*> (x Core..: "OrganizationRuleStatus")
      )

instance Core.Hashable OrganizationConfigRuleStatus

instance Core.NFData OrganizationConfigRuleStatus
