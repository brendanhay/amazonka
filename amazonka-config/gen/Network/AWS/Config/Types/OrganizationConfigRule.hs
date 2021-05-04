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
-- Module      : Network.AWS.Config.Types.OrganizationConfigRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.OrganizationConfigRule where

import Network.AWS.Config.Types.OrganizationCustomRuleMetadata
import Network.AWS.Config.Types.OrganizationManagedRuleMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An organization config rule that has information about config rules that
-- AWS Config creates in member accounts.
--
-- /See:/ 'newOrganizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { -- | The timestamp of the last update.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | An @OrganizationManagedRuleMetadata@ object.
    organizationManagedRuleMetadata :: Prelude.Maybe OrganizationManagedRuleMetadata,
    -- | An @OrganizationCustomRuleMetadata@ object.
    organizationCustomRuleMetadata :: Prelude.Maybe OrganizationCustomRuleMetadata,
    -- | A comma-separated list of accounts excluded from organization config
    -- rule.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The name that you assign to organization config rule.
    organizationConfigRuleName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of organization config rule.
    organizationConfigRuleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'organizationConfigRuleArn'
  Prelude.Text ->
  OrganizationConfigRule
newOrganizationConfigRule
  pOrganizationConfigRuleName_
  pOrganizationConfigRuleArn_ =
    OrganizationConfigRule'
      { lastUpdateTime =
          Prelude.Nothing,
        organizationManagedRuleMetadata = Prelude.Nothing,
        organizationCustomRuleMetadata = Prelude.Nothing,
        excludedAccounts = Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_,
        organizationConfigRuleArn =
          pOrganizationConfigRuleArn_
      }

-- | The timestamp of the last update.
organizationConfigRule_lastUpdateTime :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe Prelude.UTCTime)
organizationConfigRule_lastUpdateTime = Lens.lens (\OrganizationConfigRule' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConfigRule' {} a -> s {lastUpdateTime = a} :: OrganizationConfigRule) Prelude.. Lens.mapping Prelude._Time

-- | An @OrganizationManagedRuleMetadata@ object.
organizationConfigRule_organizationManagedRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationManagedRuleMetadata)
organizationConfigRule_organizationManagedRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationManagedRuleMetadata} -> organizationManagedRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationManagedRuleMetadata = a} :: OrganizationConfigRule)

-- | An @OrganizationCustomRuleMetadata@ object.
organizationConfigRule_organizationCustomRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationCustomRuleMetadata)
organizationConfigRule_organizationCustomRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationCustomRuleMetadata} -> organizationCustomRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationCustomRuleMetadata = a} :: OrganizationConfigRule)

-- | A comma-separated list of accounts excluded from organization config
-- rule.
organizationConfigRule_excludedAccounts :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe [Prelude.Text])
organizationConfigRule_excludedAccounts = Lens.lens (\OrganizationConfigRule' {excludedAccounts} -> excludedAccounts) (\s@OrganizationConfigRule' {} a -> s {excludedAccounts = a} :: OrganizationConfigRule) Prelude.. Lens.mapping Prelude._Coerce

-- | The name that you assign to organization config rule.
organizationConfigRule_organizationConfigRuleName :: Lens.Lens' OrganizationConfigRule Prelude.Text
organizationConfigRule_organizationConfigRuleName = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: OrganizationConfigRule)

-- | Amazon Resource Name (ARN) of organization config rule.
organizationConfigRule_organizationConfigRuleArn :: Lens.Lens' OrganizationConfigRule Prelude.Text
organizationConfigRule_organizationConfigRuleArn = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleArn} -> organizationConfigRuleArn) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleArn = a} :: OrganizationConfigRule)

instance Prelude.FromJSON OrganizationConfigRule where
  parseJSON =
    Prelude.withObject
      "OrganizationConfigRule"
      ( \x ->
          OrganizationConfigRule'
            Prelude.<$> (x Prelude..:? "LastUpdateTime")
            Prelude.<*> (x Prelude..:? "OrganizationManagedRuleMetadata")
            Prelude.<*> (x Prelude..:? "OrganizationCustomRuleMetadata")
            Prelude.<*> ( x Prelude..:? "ExcludedAccounts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "OrganizationConfigRuleName")
            Prelude.<*> (x Prelude..: "OrganizationConfigRuleArn")
      )

instance Prelude.Hashable OrganizationConfigRule

instance Prelude.NFData OrganizationConfigRule
