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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationConfigRule where

import Amazonka.Config.Types.OrganizationCustomPolicyRuleMetadataNoPolicy
import Amazonka.Config.Types.OrganizationCustomRuleMetadata
import Amazonka.Config.Types.OrganizationManagedRuleMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An organization Config rule that has information about Config rules that
-- Config creates in member accounts.
--
-- /See:/ 'newOrganizationConfigRule' smart constructor.
data OrganizationConfigRule = OrganizationConfigRule'
  { -- | A comma-separated list of accounts excluded from organization Config
    -- rule.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp of the last update.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | An object that specifies metadata for your organization\'s Config Custom
    -- Policy rule. The metadata includes the runtime system in use, which
    -- accounts have debug logging enabled, and other custom rule metadata,
    -- such as resource type, resource ID of Amazon Web Services resource, and
    -- organization trigger types that initiate Config to evaluate Amazon Web
    -- Services resources against a rule.
    organizationCustomPolicyRuleMetadata :: Prelude.Maybe OrganizationCustomPolicyRuleMetadataNoPolicy,
    -- | An @OrganizationCustomRuleMetadata@ object.
    organizationCustomRuleMetadata :: Prelude.Maybe OrganizationCustomRuleMetadata,
    -- | An @OrganizationManagedRuleMetadata@ object.
    organizationManagedRuleMetadata :: Prelude.Maybe OrganizationManagedRuleMetadata,
    -- | The name that you assign to organization Config rule.
    organizationConfigRuleName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of organization Config rule.
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
-- 'excludedAccounts', 'organizationConfigRule_excludedAccounts' - A comma-separated list of accounts excluded from organization Config
-- rule.
--
-- 'lastUpdateTime', 'organizationConfigRule_lastUpdateTime' - The timestamp of the last update.
--
-- 'organizationCustomPolicyRuleMetadata', 'organizationConfigRule_organizationCustomPolicyRuleMetadata' - An object that specifies metadata for your organization\'s Config Custom
-- Policy rule. The metadata includes the runtime system in use, which
-- accounts have debug logging enabled, and other custom rule metadata,
-- such as resource type, resource ID of Amazon Web Services resource, and
-- organization trigger types that initiate Config to evaluate Amazon Web
-- Services resources against a rule.
--
-- 'organizationCustomRuleMetadata', 'organizationConfigRule_organizationCustomRuleMetadata' - An @OrganizationCustomRuleMetadata@ object.
--
-- 'organizationManagedRuleMetadata', 'organizationConfigRule_organizationManagedRuleMetadata' - An @OrganizationManagedRuleMetadata@ object.
--
-- 'organizationConfigRuleName', 'organizationConfigRule_organizationConfigRuleName' - The name that you assign to organization Config rule.
--
-- 'organizationConfigRuleArn', 'organizationConfigRule_organizationConfigRuleArn' - Amazon Resource Name (ARN) of organization Config rule.
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
      { excludedAccounts =
          Prelude.Nothing,
        lastUpdateTime = Prelude.Nothing,
        organizationCustomPolicyRuleMetadata =
          Prelude.Nothing,
        organizationCustomRuleMetadata = Prelude.Nothing,
        organizationManagedRuleMetadata = Prelude.Nothing,
        organizationConfigRuleName =
          pOrganizationConfigRuleName_,
        organizationConfigRuleArn =
          pOrganizationConfigRuleArn_
      }

-- | A comma-separated list of accounts excluded from organization Config
-- rule.
organizationConfigRule_excludedAccounts :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe [Prelude.Text])
organizationConfigRule_excludedAccounts = Lens.lens (\OrganizationConfigRule' {excludedAccounts} -> excludedAccounts) (\s@OrganizationConfigRule' {} a -> s {excludedAccounts = a} :: OrganizationConfigRule) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp of the last update.
organizationConfigRule_lastUpdateTime :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe Prelude.UTCTime)
organizationConfigRule_lastUpdateTime = Lens.lens (\OrganizationConfigRule' {lastUpdateTime} -> lastUpdateTime) (\s@OrganizationConfigRule' {} a -> s {lastUpdateTime = a} :: OrganizationConfigRule) Prelude.. Lens.mapping Data._Time

-- | An object that specifies metadata for your organization\'s Config Custom
-- Policy rule. The metadata includes the runtime system in use, which
-- accounts have debug logging enabled, and other custom rule metadata,
-- such as resource type, resource ID of Amazon Web Services resource, and
-- organization trigger types that initiate Config to evaluate Amazon Web
-- Services resources against a rule.
organizationConfigRule_organizationCustomPolicyRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationCustomPolicyRuleMetadataNoPolicy)
organizationConfigRule_organizationCustomPolicyRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationCustomPolicyRuleMetadata} -> organizationCustomPolicyRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationCustomPolicyRuleMetadata = a} :: OrganizationConfigRule)

-- | An @OrganizationCustomRuleMetadata@ object.
organizationConfigRule_organizationCustomRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationCustomRuleMetadata)
organizationConfigRule_organizationCustomRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationCustomRuleMetadata} -> organizationCustomRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationCustomRuleMetadata = a} :: OrganizationConfigRule)

-- | An @OrganizationManagedRuleMetadata@ object.
organizationConfigRule_organizationManagedRuleMetadata :: Lens.Lens' OrganizationConfigRule (Prelude.Maybe OrganizationManagedRuleMetadata)
organizationConfigRule_organizationManagedRuleMetadata = Lens.lens (\OrganizationConfigRule' {organizationManagedRuleMetadata} -> organizationManagedRuleMetadata) (\s@OrganizationConfigRule' {} a -> s {organizationManagedRuleMetadata = a} :: OrganizationConfigRule)

-- | The name that you assign to organization Config rule.
organizationConfigRule_organizationConfigRuleName :: Lens.Lens' OrganizationConfigRule Prelude.Text
organizationConfigRule_organizationConfigRuleName = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleName} -> organizationConfigRuleName) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleName = a} :: OrganizationConfigRule)

-- | Amazon Resource Name (ARN) of organization Config rule.
organizationConfigRule_organizationConfigRuleArn :: Lens.Lens' OrganizationConfigRule Prelude.Text
organizationConfigRule_organizationConfigRuleArn = Lens.lens (\OrganizationConfigRule' {organizationConfigRuleArn} -> organizationConfigRuleArn) (\s@OrganizationConfigRule' {} a -> s {organizationConfigRuleArn = a} :: OrganizationConfigRule)

instance Data.FromJSON OrganizationConfigRule where
  parseJSON =
    Data.withObject
      "OrganizationConfigRule"
      ( \x ->
          OrganizationConfigRule'
            Prelude.<$> ( x Data..:? "ExcludedAccounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LastUpdateTime")
            Prelude.<*> (x Data..:? "OrganizationCustomPolicyRuleMetadata")
            Prelude.<*> (x Data..:? "OrganizationCustomRuleMetadata")
            Prelude.<*> (x Data..:? "OrganizationManagedRuleMetadata")
            Prelude.<*> (x Data..: "OrganizationConfigRuleName")
            Prelude.<*> (x Data..: "OrganizationConfigRuleArn")
      )

instance Prelude.Hashable OrganizationConfigRule where
  hashWithSalt _salt OrganizationConfigRule' {..} =
    _salt `Prelude.hashWithSalt` excludedAccounts
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` organizationCustomPolicyRuleMetadata
      `Prelude.hashWithSalt` organizationCustomRuleMetadata
      `Prelude.hashWithSalt` organizationManagedRuleMetadata
      `Prelude.hashWithSalt` organizationConfigRuleName
      `Prelude.hashWithSalt` organizationConfigRuleArn

instance Prelude.NFData OrganizationConfigRule where
  rnf OrganizationConfigRule' {..} =
    Prelude.rnf excludedAccounts
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf organizationCustomPolicyRuleMetadata
      `Prelude.seq` Prelude.rnf organizationCustomRuleMetadata
      `Prelude.seq` Prelude.rnf organizationManagedRuleMetadata
      `Prelude.seq` Prelude.rnf organizationConfigRuleName
      `Prelude.seq` Prelude.rnf organizationConfigRuleArn
