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
-- Module      : Amazonka.Config.Types.OrganizationConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationConfigRule where

import Amazonka.Config.Types.OrganizationCustomRuleMetadata
import Amazonka.Config.Types.OrganizationManagedRuleMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An organization config rule that has information about config rules that
-- Config creates in member accounts.
--
-- /See:/ 'newOrganizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { -- | An @OrganizationManagedRuleMetadata@ object.
    organizationManagedRuleMetadata :: Prelude.Maybe OrganizationManagedRuleMetadata,
    -- | A comma-separated list of accounts excluded from organization config
    -- rule.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | An @OrganizationCustomRuleMetadata@ object.
    organizationCustomRuleMetadata :: Prelude.Maybe OrganizationCustomRuleMetadata,
    -- | The timestamp of the last update.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The name that you assign to organization config rule.
    organizationConfigRuleName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of organization config rule.
    organizationConfigRuleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationConfigRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationManagedRuleMetadata', 'organizationConfigRule_organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
--
-- 'excludedAccounts', 'organizationConfigRule_excludedAccounts' - A comma-separated list of accounts excluded from organization config
-- rule.
--
-- 'organizationCustomRuleMetadata', 'organizationConfigRule_organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
--
-- 'lastUpdateTime', 'organizationConfigRule_lastUpdateTime' - The timestamp of the last update.
--
-- 'organizationConfigRuleName', 'organizationConfigRule_organizationConfigRuleName' - The name that you assign to organization config rule.
--
-- 'organizationConfigRuleArn', 'organizationConfigRule_organizationConfigRuleArn' - Amazon Resource Name (ARN) of organization config rule.
newOrganizationConfigRule ::
  -- | 'organizationConfigRuleName'
  Prelude.Text ->
  -- | 'organizationConfigRuleArn'
  Prelude.Text ->
  OrganizationConfigRule
newOrganizationConfigRule
  pOrganizationConfigRuleName_
  pOrganizationConfigRuleArn_ =
    OrganizationConfigRule'
      { organizationManagedRuleMetadata =
          Prelude.Nothing,
        excludedAccounts = Prelude.Nothing,
        organizationCustomRuleMetadata = Prelude.Nothing,
        lastUpdateTime = Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_,
        organizationConfigRuleArn =
          pOrganizationConfigRuleArn_
      }

-- | An @OrganizationManagedRuleMetadata@ object.
organizationConfigRule_organizationManagedRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationManagedRuleMetadata)
organizationConfigRule_organizationManagedRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationManagedRuleMetadata} -> organizationManagedRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationManagedRuleMetadata = a} :: OrganizationConfigRule)

-- | A comma-separated list of accounts excluded from organization config
-- rule.
organizationConfigRule_excludedAccounts :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe [Prelude.Text])
organizationConfigRule_excludedAccounts = Lens.lens (\OrganizationConfigRule' {excludedAccounts} -> excludedAccounts) (\s@OrganizationConfigRule' {} a -> s {excludedAccounts = a} :: OrganizationConfigRule) Prelude.. Lens.mapping Lens.coerced

-- | An @OrganizationCustomRuleMetadata@ object.
organizationConfigRule_organizationCustomRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationCustomRuleMetadata)
organizationConfigRule_organizationCustomRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationCustomRuleMetadata} -> organizationCustomRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationCustomRuleMetadata = a} :: OrganizationConfigRule)

-- | The timestamp of the last update.
organizationConfigRule_lastUpdateTime :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe Prelude.UTCTime)
organizationConfigRule_lastUpdateTime = Lens.lens (\OrganizationConfigRule' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConfigRule' {} a -> s {lastUpdateTime = a} :: OrganizationConfigRule) Prelude.. Lens.mapping Core._Time

-- | The name that you assign to organization config rule.
organizationConfigRule_organizationConfigRuleName :: Lens.Lens' OrganizationConfigRule Prelude.Text
organizationConfigRule_organizationConfigRuleName = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: OrganizationConfigRule)

-- | Amazon Resource Name (ARN) of organization config rule.
organizationConfigRule_organizationConfigRuleArn :: Lens.Lens' OrganizationConfigRule Prelude.Text
organizationConfigRule_organizationConfigRuleArn = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleArn} -> organizationConfigRuleArn) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleArn = a} :: OrganizationConfigRule)

instance Core.FromJSON OrganizationConfigRule where
  parseJSON =
    Core.withObject
      "OrganizationConfigRule"
      ( \x ->
          OrganizationConfigRule'
            Prelude.<$> (x Core..:? "OrganizationManagedRuleMetadata")
            Prelude.<*> ( x Core..:? "ExcludedAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "OrganizationCustomRuleMetadata")
            Prelude.<*> (x Core..:? "LastUpdateTime")
            Prelude.<*> (x Core..: "OrganizationConfigRuleName")
            Prelude.<*> (x Core..: "OrganizationConfigRuleArn")
      )

instance Prelude.Hashable OrganizationConfigRule where
  hashWithSalt _salt OrganizationConfigRule' {..} =
    _salt
      `Prelude.hashWithSalt` organizationManagedRuleMetadata
      `Prelude.hashWithSalt` excludedAccounts
      `Prelude.hashWithSalt` organizationCustomRuleMetadata
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` organizationConfigRuleName
      `Prelude.hashWithSalt` organizationConfigRuleArn

instance Prelude.NFData OrganizationConfigRule where
  rnf OrganizationConfigRule' {..} =
    Prelude.rnf organizationManagedRuleMetadata
      `Prelude.seq` Prelude.rnf excludedAccounts
      `Prelude.seq` Prelude.rnf organizationCustomRuleMetadata
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf organizationConfigRuleName
      `Prelude.seq` Prelude.rnf organizationConfigRuleArn
