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
-- Module      : Network.AWS.Config.Types.OrganizationConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRule where

import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An organization config rule that has information about config rules that
-- AWS Config creates in member accounts.
--
-- /See:/ 'newOrganizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { -- | The timestamp of the last update.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | An @OrganizationManagedRuleMetadata@ object.
    organizationManagedRuleMetadata :: Core.Maybe OrganizationManagedRuleMetadata,
    -- | An @OrganizationCustomRuleMetadata@ object.
    organizationCustomRuleMetadata :: Core.Maybe OrganizationCustomRuleMetadata,
    -- | A comma-separated list of accounts excluded from organization config
    -- rule.
    excludedAccounts :: Core.Maybe [Core.Text],
    -- | The name that you assign to organization config rule.
    organizationConfigRuleName :: Core.Text,
    -- | Amazon Resource Name (ARN) of organization config rule.
    organizationConfigRuleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OrganizationConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTime', 'organizationConfigRule_lastUpdateTime' - The timestamp of the last update.
--
-- 'organizationManagedRuleMetadata', 'organizationConfigRule_organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
--
-- 'organizationCustomRuleMetadata', 'organizationConfigRule_organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
--
-- 'excludedAccounts', 'organizationConfigRule_excludedAccounts' - A comma-separated list of accounts excluded from organization config
-- rule.
--
-- 'organizationConfigRuleName', 'organizationConfigRule_organizationConfigRuleName' - The name that you assign to organization config rule.
--
-- 'organizationConfigRuleArn', 'organizationConfigRule_organizationConfigRuleArn' - Amazon Resource Name (ARN) of organization config rule.
newOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Core.Text ->
  -- | 'organizationConfigRuleArn'
  Core.Text ->
  OrganizationConfigRule
newOrganizationConfigRule
  pOrganizationConfigRuleName_
  pOrganizationConfigRuleArn_ =
    OrganizationConfigRule'
      { lastUpdateTime =
          Core.Nothing,
        organizationManagedRuleMetadata = Core.Nothing,
        organizationCustomRuleMetadata = Core.Nothing,
        excludedAccounts = Core.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_,
        organizationConfigRuleArn =
          pOrganizationConfigRuleArn_
      }

-- | The timestamp of the last update.
organizationConfigRule_lastUpdateTime :: Lens.Lens' OrganizationConfigRule (Core.Maybe Core.UTCTime)
organizationConfigRule_lastUpdateTime = Lens.lens (\OrganizationConfigRule' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConfigRule' {} a -> s {lastUpdateTime = a} :: OrganizationConfigRule) Core.. Lens.mapping Core._Time

-- | An @OrganizationManagedRuleMetadata@ object.
organizationConfigRule_organizationManagedRuleMetadata :: Lens.Lens' OrganizationConfigRule (Core.Maybe OrganizationManagedRuleMetadata)
organizationConfigRule_organizationManagedRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationManagedRuleMetadata} -> organizationManagedRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationManagedRuleMetadata = a} :: OrganizationConfigRule)

-- | An @OrganizationCustomRuleMetadata@ object.
organizationConfigRule_organizationCustomRuleMetadata :: Lens.Lens' OrganizationConfigRule (Core.Maybe OrganizationCustomRuleMetadata)
organizationConfigRule_organizationCustomRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationCustomRuleMetadata} -> organizationCustomRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationCustomRuleMetadata = a} :: OrganizationConfigRule)

-- | A comma-separated list of accounts excluded from organization config
-- rule.
organizationConfigRule_excludedAccounts :: Lens.Lens' OrganizationConfigRule (Core.Maybe [Core.Text])
organizationConfigRule_excludedAccounts = Lens.lens (\OrganizationConfigRule' {excludedAccounts} -> excludedAccounts) (\s@OrganizationConfigRule' {} a -> s {excludedAccounts = a} :: OrganizationConfigRule) Core.. Lens.mapping Lens._Coerce

-- | The name that you assign to organization config rule.
organizationConfigRule_organizationConfigRuleName :: Lens.Lens' OrganizationConfigRule Core.Text
organizationConfigRule_organizationConfigRuleName = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: OrganizationConfigRule)

-- | Amazon Resource Name (ARN) of organization config rule.
organizationConfigRule_organizationConfigRuleArn :: Lens.Lens' OrganizationConfigRule Core.Text
organizationConfigRule_organizationConfigRuleArn = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleArn} -> organizationConfigRuleArn) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleArn = a} :: OrganizationConfigRule)

instance Core.FromJSON OrganizationConfigRule where
  parseJSON =
    Core.withObject
      "OrganizationConfigRule"
      ( \x ->
          OrganizationConfigRule'
            Core.<$> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "OrganizationManagedRuleMetadata")
            Core.<*> (x Core..:? "OrganizationCustomRuleMetadata")
            Core.<*> (x Core..:? "ExcludedAccounts" Core..!= Core.mempty)
            Core.<*> (x Core..: "OrganizationConfigRuleName")
            Core.<*> (x Core..: "OrganizationConfigRuleArn")
      )

instance Core.Hashable OrganizationConfigRule

instance Core.NFData OrganizationConfigRule
